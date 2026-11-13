ich baue gentoo mit dockerfiles. ich wuerde gern ein tool haben, dass
die dependencies je nach wahl der use flags und world datei
aufloest. das ziel ist bei verschiedenen alternativen die zu finden
die die geringste groesse (download oder finale binaries), oder
kleinste bau zeit oder so ermoeglicht. wie kann ich da am besten
vorgehen? gibt es dafuer datenbanken oder muesste ich die ebuilds
parsen? wuerdest du eher python oder rust fuer die implementierung
empefhlen?

Das ist ein spannendes Projekt – im Grunde möchtest du einen **Multi-Objective-Optimizer für Portage-Konfigurationen** bauen. Um das pragmatisch und ohne unnötige Sackgassen anzugehen, lohnt sich zuerst ein Blick auf die Datenlage in Gentoo.

---

### 1. Musst du Ebuilds parsen? Und gibt es Datenbanken?

#### Ebuilds & Abhängigkeiten: Nein, kein Bash-Parsing nötig
Ebuilds sind Bash-Skripte mit komplexer Eclass-Vererbung. Sie direkt statisch zu parsen ist praktisch unmöglich, ohne einen vollständigen Bash-Interpreter samt Portage-Builtins nachzubauen.

Glücklicherweise musst du das nicht: Der offizielle Gentoo-Tree (und fast jedes Overlay) liefert unter `/var/db/repos/gentoo/metadata/md5-cache/` bereits **vorausgewertete Metadaten** im einfachen `KEY=VALUE`-Format mit. Darin stehen für jede Paketversion u. a.:
* `DEPEND`, `RDEPEND`, `BDEPEND`, `PDEPEND`, `IDEPEND`
* `IUSE`, `REQUIRED_USE`, `SLOT`, `KEYWORDS`
* `SRC_URI` (inklusive USE-Flag-Bedingungen wie `doc? ( https://... )`)

Du musst also „nur“ die **PMS-Syntax** (Package Manager Specification) für Abhängigkeitsausdrücke parsen (`|| ( a b )`, `flag? ( ... )`, `cat/pkg:slot[use]`), nicht Bash. *(Hinweis: Falls du eigene Ebuilds/Overlays ohne Cache nutzt, generiert `egencache` diesen Cache einmalig für dich.)*

#### Datenbanken für Größen & Bauzeiten: Was existiert und was nicht?
Hier muss man zwischen den Zielgrößen unterscheiden:

1. **Quellcode-Download-Größe (Exakt berechenbar):**
   In jedem Paketverzeichnis liegt eine `Manifest`-Datei mit allen `DIST`-Einträgen inklusive exakter Dateigröße in Bytes. Kombiniert mit dem ausgewerteten `SRC_URI` aus dem `md5-cache` kannst du für jede USE-Flag-Konfiguration **auf das Byte genau** vorab berechnen, wie viel heruntergeladen werden muss.
2. **Binärpaket-Größe (Teilweise vorhanden):**
   Gentoo betreibt mittlerweile offizielle Binhosts. Die dortige `Packages`-Indexdatei enthält pro Binärpaket u. a. `CPV`, `USE`, `RDEPEND` und `SIZE` (die komprimierte Paketgröße des `.gpkg.tar` bzw. `.tbz2`).
   *Einschränkung:* Sobald deine USE-Flags vom Standard-Binhost abweichen, passt das Paket nicht mehr direkt – der Wert taugt aber oft als gute **Start-Heuristik**.
3. **Finale installierte Binary-Größe (Keine zentrale DB):**
   Eine offizielle Datenbank für installierte Größen gibt es nicht, da sie direkt von `CFLAGS` (`-O2` vs. `-Os`, `-flto`), `USE`-Flags, `LINGUAS` und `FEATURES="nodoc noinfo noman"` abhängen.
   *Lösung:* Sobald ein Paket gebaut/installiert wurde, schreibt Portage die entpackte Größe in `/var/db/pkg/<cat>/<pkg>/SIZE` (auch abfragbar via `equery size` oder `qsize`).
4. **Bauzeit (Keine zentrale DB):**
   Es gibt keine öffentliche, verlässliche Datenbank für Kompilierzeiten, da diese extrem von CPU, RAM/tmpfs, Storage-I/O (Docker OverlayFS!), `MAKEOPTS="-jN"` und USE-Flags (z. B. `test`, `lto`, `rust`) abhängen.
   *Lösung:* Portage protokolliert jede Paket-Bauzeit in `/var/log/emerge.log` (auslesbar mit Tools wie `genlop -t` oder `qlop`).

---

### 2. Wie gehst du am besten vor? (Architektur-Vorschlag)

Bevor du einen eigenen Solver schreibst, ein wichtiger **Docker-spezifischer Tipp**, der oft mehr Image-Größe spart als jedes einzelne USE-Flag:
> **Trenne konsequent `BDEPEND`/`DEPEND` von `RDEPEND`!**
> Wenn du in einem Multi-Stage-Dockerfile baust (Stage 1 baut Binärpakete oder installiert via `ROOT=/target` in ein leeres RootFS, Stage 2 übernimmt nur `/target`), landen Build-Werkzeuge wie `gcc`, `rust`, `cmake`, `llvm` oder `python` gar nicht erst im finalen Container. Das hat oft den mit Abstand größten Effekt auf die finale Image-Größe.

Für dein Tool bietet sich ein **dreistufiger Aufbau** an:

#### Schritt A: Auflösung der Abhängigkeiten (Zwei mögliche Ansätze)
* **Ansatz 1: Portage / `emerge` als „Orakel“ nutzen (Pragmatisch & 100% exakt)**
  Einen eigenen Dependency-Resolver zu schreiben, der alle Gentoo-Eigenheiten (Profiles, `use.mask`/`use.force`, Sub-Slots, Blocker, Circular Dependencies, Backtracking, `REQUIRED_USE`) exakt wie `emerge` auflöst, ist sehr komplex.
  Für den Start kannst du in einem Basis-Container deine Test-Konfigurationen (`/etc/portage/package.use`, `world`) generieren und `emerge --pretend --emptytree --with-bdeps=n @world` (oder die Python-API von Portage direkt) aufrufen. Daraus erhältst du exakt die Paketliste, die gebaut würde.
  *Nachteil:* Langsam (mehrere Sekunden pro Konfiguration), daher nicht geeignet, um automatisch zehntausende Flag-Kombinationen durchzutesten.

* **Ansatz 2: Eigener SAT/MaxSAT-Optimizer auf `md5-cache` (Für echte automatische Suche)**
  Wenn dein Tool selbstständig die kleinste Kombination aus `USE`-Flags und `|| ( pkgA pkgB )`-Alternativen *finden* soll:
  1. Lies `md5-cache`, Profile-Defaults (`make.defaults`, `use.mask`, `use.force`, `package.mask`) und deine `world`-Datei ein.
  2. Übersetze die `RDEPEND`/`DEPEND`/`REQUIRED_USE`-Regeln in boolesche Constraints (SAT/MaxSAT oder ILP, z. B. mit `Z3`, `OR-Tools` oder `PySAT`).
  3. Definiere die Gewichte (Größe oder Bauzeit) pro Paket/Flag-Kombination als zu minimierende Zielfunktion.
  4. **Wichtig:** Lass die vom Optimizer gefundene „Sieger-Konfiguration“ am Ende einmal durch `emerge -p` gegenprüfen, um sicherzugehen, dass kein versteckter Slot-Konflikt übersehen wurde.

#### Schritt B: Empirische Metriken-Datenbank (SQLite) aufbauen
Da finale Größe und Bauzeit lokal gemessen werden müssen, solltest du eine kleine SQLite-DB anlegen, die bei jedem Docker-Build automatisch gefüttert wird:
* **Nach dem Build:** Ein Hook / kleines Skript am Ende des Builder-Stages parst `/var/log/emerge.log` (Bauzeit pro CPV) sowie `/var/db/pkg/*/*/SIZE` und `/var/db/pkg/*/*/USE` (tatsächliche Größe je nach aktiven USE-Flags).
* **Kaltstart (noch nicht gebaute Pakete):**
  * Download-Größe: direkt aus `Manifest` + `SRC_URI`.
  * Größe: Fallback auf den `SIZE`-Wert aus dem offiziellen Gentoo-Binhost (`Packages`-Datei), falls vorhanden.
  * Bauzeit: Grobe Schätzwerte anhand von Indikatoren (z. B. `BDEPEND` enthält `dev-lang/rust`, `llvm`, `qtwebengine`, `gcc` -> sehr hoch; C/Meson/Autotools -> niedrig/mittel), bis reale Messwerte aus deinen eigenen Docker-Builds vorliegen.

#### Schritt C: Suchstrategie statt Brute-Force
Alle USE-Flags von hunderten Paketen zu permutieren explodiert kombinatorisch. Effektiver ist:
1. **Flag-Relevanz-Filter:** Ignoriere USE-Flags, die in `RDEPEND`/`DEPEND` überhaupt keine neuen Pakete reinziehen (z. B. beeinflussen Flags wie `nls` oder `threads` kaum die Abhängigkeiten, während `gui`, `X`, `wayland`, `python`, `ssl`, `dbus`, `systemd` ganze Subgraphen auslösen).
2. **Delta-Analyse:** Berechne zuerst den Minimal-Graphen (alle optionalen Flags aus, soweit `REQUIRED_USE` und Profile es zulassen) und zeige an, welche einzelnen USE-Flags oder `world`-Pakete die größten „Kosten-Treiber“ (schwere transitive Dependencies) sind.

---

### 3. Python oder Rust für die Implementierung?

Beide Sprachen haben hier sehr starke, aber völlig unterschiedliche Argumente:

#### Wann **Python** die bessere Wahl ist:
* **Direkte Portage-Integration:** Portage selbst ist in Python geschrieben. Du kannst `import portage` direkt nutzen, um Profile, `make.conf`, `package.use`, `md5-cache` und den VDB (`/var/db/pkg`) mit exakt derselben Logik auszulesen, die auch `emerge` verwendet – ohne PMS-Parser selbst schreiben zu müssen.
* **Alternative `pkgcore`:** Eine weitere ausgereifte Gentoo-Bibliothek in Python, die oft sauberer und schneller als Portages interne API ist.
* **Optimierungs-Ökosystem:** Libraries wie Google `OR-Tools`, `z3-solver` oder `python-sat` sind in Minuten eingebunden.
* **Nachteil:** Wenn du zehntausende Ebuilds/Cache-Dateien parsen und komplexe Graphen oder Backtracking-Suchen in reinem Python durchrechnen willst, wird es spürbar zäh.

#### Wann **Rust** die bessere Wahl ist:
* **Performance bei Graph-Suche:** Wenn du einen eigenen Solver / MaxSAT-Optimizer schreibst, der tausende Konfigurationen über den ~30.000-Ebuild-Tree exploriert, ist Rust um Größenordnungen schneller.
* **Reife Gentoo-Crates existieren bereits:** Du musst nicht bei Null anfangen!
  * **`pkgcraft`**: Ein sehr ambitioniertes, modernes Gentoo-Ökosystem in Rust (inkl. Repo-Handling, `md5-cache`, Atoms, Profile-Support) – es ist sogar schon direkt im Gentoo-Tree (`sys-libs/pkgcraft`) paketiert und bietet auch Python-/C-Bindings.
  * **`portage-metadata`** & **`portage-atom`**: Schlanke Rust-Crates speziell zum Parsen von `md5-cache`, `SRC_URI`, `REQUIRED_USE` und PMS-Atoms.
* **Single-Binary in Docker:** Das fertige Tool lässt sich als einzelnes Binary extrem sauber in CI/CD-Pipelines und Docker-Builder integrieren, ohne Python-Abhängigkeiten im Host-System.
* **Nachteil:** Du hast keinen nativen Zugriff auf Portages echten `depgraph`-Resolver – bei komplexen Randfällen müsstest du trotzdem `emerge -p` als Subprozess zur finalen Verifikation aufrufen.

### Empfehlung
* Wenn du **schnell ein praxistaugliches Werkzeug für deinen Docker-Workflow** willst (Varianten vergleichen, `BDEPEND` vs. `RDEPEND` aufschlüsseln, `Manifest`-Größen summieren, `emerge.log` + VDB-Größen auswerten): Nimm **Python** (direkt im Gentoo-Container mit Zugriff auf `portage` oder `pkgcraft`-Bindings).
* Wenn das Projekt primär ein **eigenständiger, blitzschneller Graph-Optimizer / Solver** werden soll, der automatisch den riesigen USE-Flag-Suchraum mathematisch durchsucht: Nimm **Rust** auf Basis von `pkgcraft` (oder `portage-metadata` + `portage-atom`) und nutze `emerge -p` nur noch als finalen Sanity-Check.


ich stelle mir eine rust tui vor. ausgehend von einer initialen world
datei soll der nutzer gefuehrt werden hierbei wuerde ich ein neues
datei format fuer den input der world datei definieren. statt blosser
liste, sollen darin alternativen gelistet werden kommentare sollen
erlauben die pros und cons jeder alternative zu dokumentieren. mein
plan ist eine normale world datei von ein ai agenten in so einen input
mit alternativen umwandeln zu lassen

neben jedem eintrag der world datei soll die groesse stehen (erstmal
nur download weil das am einfachsten zu bekommen ist). neben jedem
eintrag sollen auch die moeglichen use flags stehen (jeder use flag
soll +2kB, -10kB oder sowas stehen haben damit man sieht was passiert
wenn man ihn ein oder ausschaltet.  manche use flags forcieren andere
flags im selben packet oder in anderen packeten. dies soll da mit
eingerechnet werden). es kann sein, dass dies sehr langsam ist, falls
die latenz unbearable ist soll es entweder einmal durchgerechnet
werden oder immer nur fuer ein packet angezeigt werden

ich denke das ist schonmal ein gutes MVP. wenn wir das haben koennen
wir vielleicht mit SAT solvern beginnen oder so.

reviewe dieses konzept. hast du noch andere vorschlaege wie man das
tool nuetzlicher machen koennte?


Das Konzept ist richtig stark – vor allem die **Arbeitsteilung zwischen AI-Agent und deterministischem Rust-Tool** ist architektonisch sehr klug gedacht:
* Die **AI** liefert das semantische Wissen (*„Statt `nginx` könntest du auch `caddy` oder `lighttpd` nehmen, Vor-/Nachteile sind X/Y“*) – genau das, was LLMs gut können.
* Das **Rust-Tool** rechnet auf den harten Portage-Metadaten deterministisch und ohne Halluzinationen die exakten Abhängigkeiten und Größen aus – genau das, was Code gut kann.

Die interaktive TUI (z. B. mit `ratatui`), die live die Kosten jedes Flags anzeigt („ncdu für Gentoo-USE-Flags“), macht die Trade-offs greifbar statt wie eine Blackbox zu wirken.

Im Detail stecken in deinem Konzept aber ein paar **versteckte Fallen**, die du direkt beim Design des Datenmodells berücksichtigen solltest, damit die Zahlen am Ende nicht in die Irre führen.

---

### 1. Review der kritischen Punkte & Denkfehler-Fallen

#### Falle A: Das „Shared-Dependency“-Problem (Was bedeutet „Größe“ eines Eintrags?)
Angenommen, in deiner `world`-Datei stehen Paket A und Paket B, und beide brauchen `dev-lang/python` (20 MB). Welche Größe zeigst du neben Paket A an?
* **Inklusive Größe (A + alle Deps):** A zeigt 21 MB, B zeigt 21 MB. Die Summe wäre scheinbar 42 MB, obwohl es real nur 22 MB sind.
* **Exklusive Größe (was wegfällt, wenn man A abwählt):** A zeigt 1 MB, B zeigt 1 MB – und die 20 MB für Python scheinen nirgends aufzutauchen.

**Das gilt genauso für USE-Flag-Deltas:** Wenn Paket A `USE="ssl"` hat (braucht `openssl`), Paket B aber *ohnehin* `openssl` braucht (oder `openssl` schon im `@system`/Stage3 steckt), dann bringt das Ausschalten von `ssl` bei A exakt **`-0 kB`**.

**Lösung für die UI:** Du solltest im Kern immer ein **Reference-Counting auf dem gesamten aktiven Graphen** (inkl. `@system`/Stage3-Baseline!) fahren und in der TUI zwei Zahlen unterscheiden:
1. **Marginal-Delta (`-12 MB` / `+0 B`):** Was passiert *tatsächlich* mit der Gesamtgröße des Containers, wenn ich diese Alternative oder dieses Flag **jetzt gerade** umschalte?
2. **Subgraph-Größe (kleiner/eingeklappt):** Wie schwer ist der gesamte Rattenschwanz an Dependencies dieses Pakets (damit man sieht, wer mit wem zusammenhängt).

#### Falle B: Source-Download-Größe kann extrem irreführend sein (`BDEPEND` & Tarballs)
Download-Größe (`Manifest` + `SRC_URI`) ist als MVP super leicht zu berechnen, hat aber zwei fiese Stolpersteine:
1. **Das `linux-headers`- / `llvm`-Problem:** Das Paket `sys-kernel/linux-headers` installiert im Container nur ~6 MB Header-Dateien, lädt laut `Manifest` aber den kompletten ~140 MB Linux-Kernel-Tarball herunter. Umgekehrt ist `llvm` als Tarball ca. 130 MB groß, kompiliert aber zu >1 GB Binaries.
2. **`BDEPEND` vs. `RDEPEND`:** Wenn ein kleines Go/Rust-Tool in `world` steht, zieht es beim Download eventuell `dev-lang/rust` (~300 MB Download) als `BDEPEND` rein. Im Multi-Stage-Docker-Image landet davon aber **0 Byte** im finalen Container.

**Pragmatischer Fix schon fürs MVP:**
* Trenne im Graphen von Anfang an strikt `RDEPEND` (Runtime) von `BDEPEND`/`DEPEND` (Buildtime). Zeige zumindest in der Statusleiste beides getrennt an.
* **Quick-Win statt Bau-Datenbank:** Lad dir die eine `Packages`-Indexdatei vom offiziellen Gentoo-Binhost herunter (ca. 5 MB Textdatei). Dort steht für fast jedes Paket mit Standard-Flags der `SIZE`-Wert des **fertig kompilierten Binärpakets**. Das dauert 2 Sekunden zu parsen und gibt dir kostenlos einen viel realistischeren Proxy für die finale Container-Größe als die `SRC_URI`-Tarballs!

#### Falle C: USE-Flag-Kaskaden & Konflikte (Nicht nur Zahlen!)
Du schreibst richtig: *„manche use flags forcieren andere flags im selben paket oder in anderen paketen.“*
Dabei gibt es drei Fälle, die dein Delta-Rechner kennen muss:
1. **Kaskade im selben Paket (`REQUIRED_USE`):** z. B. `wayland? ( egl )` oder `^^ ( qt6 gtk )`. Wenn du `wayland` einschaltest, musst du `egl` mit einschalten (oder falls `gtk` an war, `gtk` ausschalten -> Delta kann auch negativ + positiv gemischt sein!).
2. **Kaskade in Dependencies (`cat/pkg[foo]`):** Paket A braucht mit `USE=bar` plötzlich `libX[foo]`. Das kann auf `libX` wieder einen riesigen neuen Subgraphen auslösen.
3. **Konflikte!** Was, wenn Paket A mit `USE=bar` `libX[foo]` erzwingt, aber Paket C in deiner `world` explizit `libX[-foo]` verlangt? Dann darf bei `bar` nicht `+20 kB` stehen, sondern **`CONFLICT (mit C)`** (oder ein Warnsymbol, was man aufklappen kann).

#### Falle D: Performance & Latenz (Gute Nachrichten!)
Deine Sorge, dass das Durchrechnen aller Deltas unerträglich langsam wird, lässt sich in Rust komplett entschärfen – **solange du dafür nicht `emerge` als Subprozess aufrufst**:
* Ein typischer Gentoo-Container hat im aufgelösten Graphen ca. 50–400 Pakete.
* Das Einlesen von `md5-cache` + `Manifest` für den erreichbaren Subgraphen (inkl. aller Alternativen) dauert in Rust mit `rayon` geschätzt **50–200 ms** (oder du legst einen binären Cache z. B. mit `rkyv`/`bincode` an, dann startet die TUI in <20 ms).
* Sobald die Pakete intern auf Integer-IDs gemappt sind (`Vec`/Index-basiert statt String-Allokationen), dauert *ein* kompletter Graph-Reachability-Lauf über 300 Pakete **wenige Mikrosekunden**.
* Selbst wenn du beim Start (oder nach jeder Änderung) die exakten Marginal-Deltas für **500 verschiedene USE-Flags** im Hintergrund neu durchrechnest, dauert das in Rust vermutlich **unter 20–50 ms**! Du brauchst also gar keine künstliche Begrenzung auf ein Paket – die TUI wird sich absolut verzögerungsfrei anfühlen.

#### Falle E: Das neue Input-Dateiformat (Kommentare nicht verlieren!)
Wenn du `# Kommentare` in einer Textdatei nutzt, um Pros/Cons zu dokumentieren, wirft ein normaler Parser (TOML/YAML) die Kommentare beim Einlesen weg – dann kann deine TUI die Pros & Cons gar nicht anzeigen!

Besser: Nutze ein bestehendes, AI-freundliches Format wie **TOML** oder **KDL** und mache Pros/Cons zu echten Feldern:

```toml
[baseline]
profile = "default/linux/amd64/23.0/no-multilib"
# oder stage3-Paketliste zum Abziehen

[[choice]]
id = "web-server"
required = true
description = "HTTP Reverse Proxy"

  [[choice.option]]
  atom = "www-servers/nginx"
  pros = "Sehr modular über USE-Flags, extrem verbreitet"
  cons = "Viele Module veraltet, C-Codebase, braucht PCRE"
  suggested_use = ["-http2", "ssl"]

  [[choice.option]]
  atom = "www-servers/caddy"
  pros = "Automatisches HTTPS out of the box, ein Go-Binary"
  cons = "Großes Binary (~40MB), braucht dev-lang/go als BDEPEND"

  [[choice.option]]
  atom = "www-servers/lighttpd"
  pros = "Winziger Footprint, sehr wenige Runtime-Deps"
  cons = "Weniger Features für komplexe Proxy-Setups"
```
**Vorteile:** Ein AI-Agent produziert valides TOML nahezu fehlerfrei, `serde` + `toml` parsen es in 10 Zeilen Rust, und deine TUI kann rechts in einer Detail-Box sauber `Pros`, `Cons` und `AI-Hinweis` anzeigen, während du durch die Alternativen scrollst.

---

### 2. Weitere Ideen, die das Tool noch deutlich nützlicher machen

1. **Die „Warum ist das hier?“-Ansicht (Reverse-Dependency / Blame-View):**
   Das ist meiner Erfahrung nach das **wichtigste Feature überhaupt**, um Gentoo-Systeme klein zu bekommen. Wenn irgendwo tief unten plötzlich `dev-lang/python` oder `dev-libs/icu` (30 MB!) auftaucht, willst du auf das Paket gehen und sofort die Kette sehen:
   `world:app-A (USE:+xml) -> dev-libs/libxml2 (USE:+icu) -> dev-libs/icu`
   Erst dadurch weiß der Nutzer, *welches* unscheinbare Flag ganz oben den Elefanten durch die Tür geschoben hat.

2. **Transitive `|| ( pkgA pkgB )`-Alternativen in der TUI aufdecken:**
   Alternativen gibt es nicht nur in deiner `world`-Datei! In den `md5-cache`-Dateien stehen überall `|| ( ... )`-Blöcke (z. B. `|| ( net-misc/curl net-misc/wget )` oder `|| ( dev-libs/openssl dev-libs/libressl )` oder `virtual/awk`). Wenn dein Resolver auf so eine Verzweigung stößt, kann er in der TUI automatisch einen neuen Eintrag unter „Implizite Entscheidungen“ anlegen, bei dem man zwischen `curl` und `wget` wählen kann – inklusive Größen-Delta!

3. **Globale „Big Levers“-Ansicht (Globale USE-Flags):**
   Statt bei 40 Paketen einzeln `doc`, `nls`, `python`, `X`, `wayland`, `dbus` oder `test` abzuschalten, wäre ein extra Tab/Panel genial, das aggregiert:
   * `USE="-python"` global: **-82 MB** (betrifft 9 Pakete, *1 Konflikt bei Paket X*)
   * `USE="-nls"` global: **-14 MB** (betrifft 23 Pakete)
   So schlägt man 80 % der Größe mit 3 Tastendrücken raus und geht danach erst ins Feintuning.

4. **Multi-Slot- & `virtual/*`-Detektor (Warnungen):**
   Nichts bläht ein Gentoo-Image so heimlich auf wie zwei parallele Slots (z. B. `python:3.12` und `python:3.13`, oder `openssl:0` und `openssl:3` oder zwei JDKs). Ein kleiner Indikator in der TUI, sobald zwei Pakete unterschiedliche Slots desselben Pakets erzwingen, spart sofort Platz und Ärger.

5. **Baseline / Stage3-Subtraktion:**
   Gib dem Tool die Paketliste deines Basis-Images (`docker run --rm gentoo/stage3 qlist -ICS` oder `equery list '*'`) als Baseline mit. Alles, was dort schon drin ist, kostet im Delta `0 B` (es sei denn, du baust komplett from scratch mit `ROOT=/target` ohne Stage3-Reste). Damit stimmen die Zahlen für deinen realen Container.

6. **„Export & Verify with `emerge -p`“:**
   Dein eigener Rust-Graph-Resolver wird für das schnelle interaktive Explorieren 99 % aller Fälle richtig treffen, aber Portages echter Resolver hat hunderte Sonderregeln (Sub-Slot-Rebuilds, `package.mask`, komplexe Backtracking-Zyklen).
   Bau am Ende einen Export-Button (`[E]`): Der schreibt saubere `world`, `package.use` und `package.accept_keywords` Dateien und bietet optional an, einmalig `emerge --pretend --emptytree --verbose @world` im Ziel-Container zu triggern, um die **100 % verbindliche Portage-Absegnung** zu bekommen.

### Fazit zum MVP-Scope
Dein geplanter MVP-Umfang ist genau richtig geschnitten – lass den SAT-Solver wirklich erst mal komplett weg! Ein schneller in-memory Graph in Rust (z. B. auf Basis der Crates `pkgcraft` oder `portage-metadata` + `portage-atom`), kombiniert mit `ratatui`, `Reference-Counting` für Marginal-Deltas, der `RDEPEND`/`BDEPEND`-Trennung und einem TOML-Format für die AI-Alternativen ist ein extrem rundes, realistisch umsetzbares Projekt mit sofortigem Praxisnutzen.





1A ja am ende interessiert mich die binaer groesse des finalen runtime
systems (und vielleicht auch die compile zeit bzw code complexitaet)

1B du hast recht die SIZE werte des binaer packets sollten ins MVP.
aber fuer die use deltas brauchen wir sicher die download ggroessen

1C ja konflikte sollten hervorgehoben werden. vielleicht sollte man in
diesen stellen schnell umschalten koennen zwischen validen
konfigurationen und diese in der input datei abspeichern

1E ja toml ist gut

2.1 guter punkt das sollte ins mvp

2.2 wenn es alternativen im md5-cache gibt sollte die tui diese anzeigen. vielleicht brauchen wir gar keine AI

2.3 ja, besonders hilfreich ist wenn man da die groese, konflikte und anzahl dependencies sieht

2.4 auch gut

2.5 naja ich hatte im die vorstellung mit baseline world datei anzufangen

2.6 ja


ich denke wir verstehen jetzt besser was die tui machen soll.  ich
habe das gefuehl wir haben einige ideen die zunaechst nicht in den
proof of concept sollten.  ordne die anforderungen nochmal nach der
prioritaet und sortiere. starte mit den wichtigsten und plane damit
die umsetzung des mvp und die ersten paar steigerungen in der
funktionalitaet

Das ergibt jetzt ein sehr klares, stimmiges Bild. Besonders der Mix aus **Binhost-`SIZE` (für reale Runtime-Größe) + `Manifest`-Download (für feine USE-Deltas) + Blame-View + `||`-Alternativen aus `md5-cache`** macht das Tool extrem praxisnah – und reduziert die Abhängigkeit von einer AI erheblich.

Damit du dich nicht verrennst, sortieren wir zuerst alle Anforderungen knallhart nach **Priorität (Nutzen vs. technische Abhängigkeit)** und bauen daraus eine klare **Stufen-Roadmap** vom PoC bis zur Ausbaustufe.

---

### 1. Priorisierung der Anforderungen (Top-Down)

#### Priorität 1: Fundament (ohne das geht nichts)
1. **Daten-Loader (`md5-cache` + `Manifest` + Binhost `Packages` + Profile/Keywords/Masks):**
   * *Wichtiges Detail:* Du musst zwingend `KEYWORDS` (z. B. `amd64`) und `package.mask` berücksichtigen, sonst wählt dein Resolver ständig `9999`-Live-Ebuilds ohne `Manifest`-Größe und ohne Binhost-`SIZE`.
2. **In-Memory `RDEPEND`-Graph mit Reference-Counting:**
   * Auflösen der Laufzeit-Abhängigkeiten ab einer Baseline-`world` und Berechnung von **Total-Size** sowie **Marginal-Deltas** (was ändert sich *wirklich* am Gesamtsystem?).
   * *Hinweis zu Dual-Metrik:* Wenn ein USE-Flag auf Paket A andere Pakete B und C reinzieht/rauswirft, siehst du das direkt über deren Binhost-`SIZE`. Wenn ein Flag nur Paket A selbst verändert (ohne neue Deps), liefert zumindest `SRC_URI` einen Hinweis.
3. **TOML-Format (Baseline `world` -> TOML-Konvertierung & Speichern):**
   * Struktur für Pakete, aktive USE-Flags und Alternativen (`[[choice]]`), sodass Zustand und Entscheidungen persistent bleiben.

#### Priorität 2: Kern-MVP (macht das Tool im Alltag benutzbar)
4. **Minimale TUI (`ratatui`) mit Paketliste, USE-Flags & Live-Deltas.**
5. **„Blame / Why is this here?“-View (2.1):**
   * Rückwärts-Pfad von jedem beliebigen Paket zurück zu `world` + auslösendem USE-Flag. (Ohne dieses Feature sind große Dep-Graphen kaum debuggbar.)
6. **Transitive `|| ( a b )`-Alternativen aus `md5-cache` (2.2):**
   * Versteckte Wahlmöglichkeiten im Baum sichtbar und umschaltbar machen.
7. **`REQUIRED_USE` & `cat/pkg[flag]`-Kaskaden + Konflikt-Erkennung (1C Teil 1):**
   * Mitberechnen von erzwungenen Flags und rotes Markieren von Widersprüchen.
8. **Export (`world` & `package.use`).**

#### Priorität 3: Produktivitäts-Booster (erste echte Steigerung)
9. **Globale „Big Levers“-Ansicht (2.3):** Globale USE-Flags inkl. Gesamtersparnis, Paketanzahl und Konfliktanzahl.
10. **Interaktiver Konflikt-Resolver / Quick-Switch (1C Teil 2):** Auf einem Konflikt schnell zwischen gültigen Auflösungen wechseln und speichern.
11. **`emerge -p` Verifikation (2.6):** Einmal-Check gegen das echte Portage im Docker-Container.

#### Priorität 4: Tiefe & Zukunft (später)
12. **Multi-Slot- & `virtual/*`-Detektor (2.4).**
13. **`BDEPEND`-Trennung / Stage3-Subtraktion / Empirische SQLite-DB** (reale gemessene Größen aus `/var/db/pkg/*/*/SIZE` und Bauzeiten aus `/var/log/emerge.log`).
14. **AI-Alternativen-Generator & SAT-Solver.**

---

### 2. Umsetzungsplan: PoC → MVP → Ausbaustufen

#### Phase 0: Proof of Concept (PoC) – *„Der Motor läuft“*
**Ziel:** Beweisen, dass das Einlesen, Auflösen und Delta-Rechnen in Rust schnell und korrekt funktioniert – **noch ohne komplexe TUI**.

* **Scope:**
  * Ein kleines CLI-Binary (`cargo run -- --world ./world`).
  * Lädt `md5-cache`, `Manifest` und eine lokale Binhost-`Packages`-Datei.
  * Filtert Versionen sauber nach `KEYWORDS` (z. B. `amd64`) und `package.mask` (erstmal ohne komplexe Slot-Rebuilds, einfach höchste stabile Version pro `cat/pkg`).
  * Baut den `RDEPEND`-Graphen ab der `world`-Datei auf (bei `|| ( a b )` wird für den PoC stumpf immer Option `a` genommen).
  * Berechnet für ein übergebenes Paket alle seine USE-Flags (inkl. einfachem Reference-Counting) und gibt sie im Terminal aus:
    ```text
    www-servers/nginx (Binpkg: 1.2 MB | Subgraph: 14.5 MB)
      [x] ssl      (removing saves: -4.2 MB bin / -9.8 MB dl)
      [ ] http2    (enabling costs: +0.0 MB bin / +0.0 MB dl)
      [ ] pcre2    (removing saves: -1.8 MB bin / -2.3 MB dl)
    ```
* **Architektur-Tipp:** Trenne strikt in zwei Crates/Module: `engine` (daten + graph + delta) und `cli/tui`.
* **Definition of Done:** Der PoC läuft auf deiner echten `world`-Datei in unter 500 ms durch und die ausgegebenen Pakete/Größen sind plausibel.

---

#### Phase 1: MVP – *„Die interaktive Arbeitsversion“*
**Ziel:** Du kannst deine `world`-Datei interaktiv in der TUI verkleinern und gültige Portage-Configs für deinen Docker-Build exportieren.

* **Schritt 1.1: TOML-State & `world`-Import**
  * Befehl `init`: Liest eine normale `world`-Datei ein und erzeugt eine `plan.toml` (inkl. gewählter Defaults).
  * Zustand (`plan.toml`) speichert explizit gesetzte USE-Flags und getroffene Entscheidungen.
* **Schritt 1.2: Kaskaden & Konflikte (`REQUIRED_USE` & `pkg[flag]`)**
  * Erweiterung des Graph-Resolvers: Wenn Flag `A` auf Paket 1 ein Flag `B` auf Paket 1 (`REQUIRED_USE`) oder Paket 2 (`cat/pkg[B]`) erzwingt, wird das rekursiv durchgerechnet.
  * Tritt ein Widerspruch auf (`B` wird sowohl als `+B` als auch als `-B` verlangt), wird das betreffende Flag als **`CONFLICT`** markiert (inkl. Angabe, *wer* widerspricht).
* **Schritt 1.3: Transitive `|| ( a b )`-Alternativen**
  * Stößt der Resolver im `md5-cache` auf `|| ( ... )`, legt er das im Zustand als offene Entscheidung ab (Default: erste gültige Option), sodass du sie in der TUI umschalten kannst.
* **Schritt 1.4: Die `ratatui`-Oberfläche (3-Pane-Layout)**
  * **Links:** Liste der `world`-Einträge + (einklappbar/separat) gefundene `||`-Entscheidungen aus dem Tree + alle transitiv gezogenen Pakete sortiert nach Größe.
  * **Mitte:** Details zum gewählten Paket: Alle USE-Flags mit Live-Marginal-Delta (`+X MB` / `-Y kB` / `CONFLICT`). Leertaste toggelt das Flag und aktualisiert alle Zahlen live.
  * **Rechts (oder Tab/Popup): Blame-View („Warum ist das hier?“):** Zeigt für das aktuell ausgewählte Paket alle Dependency-Ketten von `world` bis zu diesem Paket.
  * **Footer:** Gesamtgröße (Binpkg-Summe + Download-Summe), Paketanzahl, Konflikt-Counter, `[S]` Speichern in TOML, `[E]` Export (`world` + `package.use`).

* **Definition of Done:** Du nimmst eine echte `world`-Datei aus deinem Docker-Setup, drückst in der TUI unerwünschte fette Pakete über Blame-View & USE-Flags raus, exportierst `package.use` und baust damit erfolgreich ein kleineres Image.

---

#### Phase 2: Erste funktionale Steigerung – *„Komfort & Verifikation“*
**Ziel:** Weniger manuelle Klickarbeit und 100 % Sicherheit vor dem Docker-Build.

1. **Globale „Big Levers“-Ansicht (2.3):**
   * Neuer Tab `[2] Global USE`: Aggregiert jedes USE-Flag über alle aktiven Pakete. Zeigt: *Flag-Name | Betroffene Pakete | Gesamt-Delta bei Deaktivierung | Anzahl entstehender Konflikte*.
2. **Schnell-Umschalter bei Konflikten (1C):**
   * Wenn du auf einem roten Konflikt stehst, öffnet `[Enter]` ein kleines Menü mit validen Auflösungen (z. B. *Option A: `libX[+foo]` erzwingen (+3 MB)* vs. *Option B: `pkgC[-bar]` abschalten (-1 MB)*) und übernimmt die gewählte Konfiguration direkt in die `plan.toml`.
3. **`emerge -p`-Verifikation (2.6):**
   * Ein Export-Hook/Befehl, der die generierten `world` & `package.use` gegen ein lokales `emerge --pretend --emptytree` (oder via `docker run` gegen dein Basis-Image) prüft und Abweichungen meldet (z. B. falls ein versteckter Sub-Slot-Konflikt übersehen wurde).

---

#### Phase 3: Präzision & Realdaten – *„Vom Proxy zur Exaktheit“*
**Ziel:** Exakte Zahlen für *deine* spezifischen `CFLAGS` und optional Build-Zeiten.

1. **Multi-Slot- & `virtual/*`-Warnungen (2.4):**
   * Banner/Badge in der UI, sobald zwei Slots desselben Pakets (z. B. `python:3.12` und `3.13`) aktiv sind.
2. **Empirische Build-Telemetrie (SQLite):**
   * Kleines Post-Build-Skript für dein Dockerfile, das `/var/db/pkg/*/*/SIZE`, `USE` und Bauzeiten aus `/var/log/emerge.log` einsammelt.
   * Die Engine nutzt diese lokalen Messwerte automatisch mit höherer Priorität als den Binhost-`SIZE`-Schätzwert – und kann zusätzlich `BDEPEND`-Größen und geschätzte Compile-Zeiten anzeigen.
3. **Optional: Stage3-Subtraktion & AI-Anreicherung:**
   * Baseline-Liste abziehen, falls man auf einem Standard-Stage3 aufsetzt.
   * Hilfsbefehl, der deine `plan.toml` um `[[choice]]`-Alternativen (mit `pros`/`cons`) per LLM anreichert.

---

### Ein letzter Technik-Tipp für den Start von Phase 0
Prüfe am Tag 1, ob dir für das Parsing die Crate **`pkgcraft`** (sehr vollständig, inkl. Profiles/EAPI/Repos, aber schwergewichtiger) oder die schlankere Kombination **`portage-metadata` + `portage-atom`** besser liegt. Für den PoC brauchst du von den Ebuilds wirklich nur `RDEPEND`, `IUSE`, `REQUIRED_USE`, `SLOT`, `KEYWORDS`, `SRC_URI` + `Manifest` + Binhost-`Packages`. Wenn du dir das hinter einem kleinen eigenen `RepoIndex`-Struct kapselst, bleibt dein Graph-Motor komplett unabhängig vom Parser.
