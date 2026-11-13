mache ein review vom code und schreibe eine dokumentation.
das walkthrough.md dokument ist fuer mich schwer zu lesen. hole den leser ab und nimm ihn mit. erklaere zum beispiel wo die world datei gesucht wird und ordne ein welche schritte fuer verschiedenene anwendungsfaelle unternommen werden sollten. z.b.

    ich moechte in ffmpeg jpeg to av1 convertierung unterstuetzen, wie viel mehr packete und welche zusaetzliche groesse wird dass hervorrufen?

    wie kann ich alternativen fuer openssh vergleichen?

    welche alternativen gibt es fuer nginx?

schreibe auf wie mir das tui programm helfen kann und wie man es nutzt.

falls du probleme siehst schreibe diese mit loesungsvorschlaegen auf.
ich habe bei meiner nutzung z.b. gefuden
big levers braucht fuer lange zeit 100% cpu von einem core (warum nicht verteilen?)
ebenso fuer tui

bei manchen fenstern scrollt der content nicht mit der selektierten zeile mit

Hier ist ein vollständiges **Code-Review** (inklusive Analyse deiner beobachteten Performance- und Scrolling-Probleme samt konkreten Lösungsvorschlägen) sowie eine **neu aufgebaute, verständliche Benutzer-Dokumentation**, die dich als Leser abholt und deine drei konkreten Anwendungsfälle Schritt für Schritt durchspielt.

---

# Teil 1: Code-Review & Lösungsvorschläge

## 1. Performance-Probleme (warum `big-levers` & TUI 100 % auf einem Core fressen)

Deine Beobachtung trifft exakt die größte Engstelle im Code. Obwohl `rayon` in `Cargo.toml` eingebunden ist, wird es **nur** beim initialen Einlesen des `md5-cache` (`05_repo_index.rs`) verwendet – die gesamte Delta- und Kaskaden-Engine läuft **streng einsträngig auf 1 CPU-Kern** und rechnet obendrein vieles mehrfach:

1. **`big_levers` läuft seriell (`08_delta.rs`)**
   - `for flag in flags` iteriert sequentiell über jedes aktive USE-Flag im gesamten Graphen (bei einer realen `world` mit ~519 Paketen leicht 200–500 Flags) und ruft jeweils `resolve_with_cascade(&mut sim, world)` auf.
   - Jeder Aufruf startet bis zu 8 Runden `resolve()` + `resolve_build_extra()` + `CascadeStep`.
   - Zudem berechnet `big_levers` die Baseline mit `resolve(engine, world)` (ohne Kaskade!), die Flag-Simulationen aber mit `resolve_with_cascade(...)` – bei bestehenden impliziten Kaskaden kann dadurch bereits ein Schein-Delta entstehen.

2. **`flag_delta` berechnet die Baseline für jedes einzelne Flag neu (`08_delta.rs`)**
   - In `package_deltas` wird `flag_delta` in einer Schleife für jedes Flag eines Pakets aufgerufen.
   - Innerhalb von `flag_delta` steht:
     ```rust
     let before = resolve_with_cascade(&mut engine.fork(), world)?.graph.totals();
     ```
   - Bei einem Paket wie `media-video/ffmpeg` mit ~60–100 Flags wird der komplette Graph also **120–200 Mal** statt 60–100 Mal aufgelöst – ebenfalls seriell auf 1 Kern.

3. **`AppState::refresh()` triggert bei jedem TUI-Tastendruck ALLES neu (`12_tui_state.rs` & `14_tui_runner.rs`)**
   - **Beim Navigieren (`j`/`k`) in der linken Liste:** `move_selection` ruft `rebuild_middle()` → `package_deltas()` auf (bei großem Paket also ~150 Graph-Auflösungen pro Cursor-Schritt).
   - **Beim Flag-Toggle (`Space`):** `refresh()` berechnet `resolve_with_cascade` → `package_deltas` → **komplettes `big_levers` über alle Pakete!** Ein einzelnes `Space` stößt also mehrere Hundert vollständige Graph- und Kaskaden-Läufe synchron im UI-Thread an, während das Terminal blockiert.
   - **Beim Tippen im Suchfilter (`/`):** `14_tui_runner.rs` ruft bei *jedem einzelnen getippten Buchstaben* `state.refresh()` auf, obwohl sich nur der Textfilter geändert hat!

4. **Ungenutzte Integer-IDs (`01_types.rs` vs. `06_graph.rs`)**
   - `01_types.rs` definiert extra `PkgId(u32)`, `FlagId(u32)` und `IdTable` mit dem Kommentar: *„Integer-IDs machen den Graphen evaluations-schnell“*.
   - In `06_graph.rs` und `07_cascade.rs` wird davon **nichts** genutzt. Alles arbeitet mit `String`-Cloning, `HashMap<String, ...>`, `Dep::parse` und `DepEntry::evaluate_use` (inkl. `BDEPEND`-BFS bei jeder Simulation).

5. **Mehrfaches Parsen von `Manifest` (`05_repo_index.rs`)**
   - `parse_cache_file` liest `<repo>/<kat>/<pkg>/Manifest` für **jede einzelne Ebuild-Version** erneut von Platte (hat ein Paket 10 Versionen in `md5-cache`, wird dieselbe `Manifest`-Datei 10-mal gelesen und geparst).

### Lösungsvorschläge für Performance:
- **Sofort-Gewinn 1 (`08_delta.rs` mit `rayon`):**
  1. `before` einmalig *vor* der Schleife in `package_deltas` berechnen (`flag_delta_with_base(...)`), ebenso in `big_levers` `resolve_with_cascade` für `base` nutzen.
  2. Sowohl `big_levers` als auch `package_deltas` auf `rayon::prelude::*` (`par_iter()`) umstellen. `Engine::fork()` klont nur zwei `HashMap`s; da `RepoIndex` und `Profile` nur unveränderliche Referenzen (`&RepoIndex`, `&Profile`) sind, lassen sie sich direkt parallel auswerten (sofern `CacheEntry`/`Dep` `Sync` sind; falls ein Upstream-Typ nicht `Sync` ist, vorab in einfache eigene `Vec`-Strukturen übersetzen).
- **Sofort-Gewinn 2 (`12_tui_state.rs` & `14_tui_runner.rs`):**
  1. Beim Filter-Tippen (`/`) **nur** `rebuild_left()` + `rebuild_middle()` aufrufen, niemals `refresh()`.
  2. `big_levers` **lazy** berechnen (nur wenn Tab 2 geöffnet wird oder auf Knopfdruck `R`), nicht bei jedem `Space` in Tab 1.
  3. `package_deltas` beim Scrollen in der linken Pane entkoppeln/cachen oder asynchron im Hintergrund-Thread berechnen (Channel an den Event-Loop), damit `j`/`k` sofort mit 60 FPS reagieren.
- **Mittelfristig (`06_graph.rs`):**
  - Die in `01_types.rs` bereits vorbereiteten `PkgId`/`FlagId` tatsächlich nutzen: Beim Laden von `RepoIndex` (oder einmal vor der Simulation) für jedes sichtbare Paket die geparsten `RDEPEND`-Bäume in kompakte `u32`-ID-Strukturen legen und `resolve_build_extra` bei reinen Runtime-Delta-Simulationen optional überspringen. Dann dauert ein Graph-Durchlauf tatsächlich Mikrosekunden statt Millisekunden.

---

## 2. TUI-Scrolling & Bedienungs-Bugs (`12_tui_state.rs`, `13_tui_view.rs`, `14_tui_runner.rs`)

Hier liegt die Erklärung für dein zweites Symptom (*„bei manchen fenstern scrollt der content nicht mit der selektierten zeile mit“*):

1. **Kein einziges Widget in `13_tui_view.rs` verwendet Scroll-State!**
   - **`render_left` (Pakete):** Nutzt `frame.render_widget(list, area)` statt `frame.render_stateful_widget(list, area, &mut list_state)`. Die Liste startet immer bei Zeile 0 – ab ca. Zeile 35 verschwindet der Cursor unsichtbar nach unten aus dem Bild.
   - **`render_middle` (USE-Flags):** Nutzt `Paragraph::new(...)` ohne `.scroll((offset, 0))`. Bei Paketen mit vielen Flags (`ffmpeg`, `python`) sind die unteren Flags nicht sichtbar.
   - **`render_levers` (Tab 2 Big Levers):** Nutzt `frame.render_widget(table, area)` statt `frame.render_stateful_widget(table, area, &mut table_state)`. Auch hier scrollt die Tabelle nicht mit `lever_selected`.
   - **`render_right` (Blame / Pros-Cons):** Ebenfalls ein nicht scrollbarer `Paragraph`; `move_selection` gibt für `Pane::Right` `0` zurück (`12_tui_state.rs`).

2. **Weitere UX-Bugs in der TUI:**
   - **Verlorene Fokus-Anzeige links:** In `render_left` wird die gewählte Zeile nur hervorgehoben, wenn `state.pane == Pane::Left`. Sobald du mit `Tab` in die Mitte wechselst, siehst du links nicht mehr, welches Paket gerade aktiv ist.
   - **Tab 2 (`Big Levers`) hat keine Toggle-Aktion:** Drückst du auf Tab 2 `Space`, setzt `14_tui_runner.rs` hart `state.pane = Pane::Middle` und toggelt ein Flag aus Tab 1 statt den ausgewählten Big Lever!
   - **Export (`E`) exportiert veralteten Stand:** `14_tui_runner.rs` ruft `export_plan(&state.plan, ...)` auf. `state.plan` wird aber **nur bei `S` (Save)** mit den aktuellen Toggles aus `engine.overrides` synchronisiert (`sync_plan_from_state`). Toggelst du Flags und drückst `E` ohne vorheriges `S`, exportierst du den alten Stand!
   - **Filter (`/`) unsichtbar:** Der aktive Suchstring `state.filter` wird nirgends im UI angezeigt (Titel bleibt `Pakete (N)`).
   - **`[[choice]]`-Alternativen in der TUI nicht umschaltbar:** `12_tui_state.rs` liest `[[choice]]` beim Start ein und `13_tui_view.rs` zeigt rechts `Pros`/`Cons` der gewählten Option – es gibt aber keinen Eintrag links und keine Taste, um zwischen `[[choice.option]]`-Einträgen umzuschalten.
   - **Kein Default-Subcommand `tui`:** `main.rs` / `11_cli.rs` erfordert explizit `gentoo-dep-tui tui`, obwohl `tasks.md` T18 vorgesehen hatte, dass ein Aufruf ohne Subcommand die TUI startet.

### Lösungsvorschläge für TUI & Scrolling:
- In `AppState` für jede Liste einen Zustand (`ListState` für links, `TableState` für Big Levers, `scroll_middle: u16` / `scroll_right: u16`) führen und über `render_stateful_widget` bzw. `Paragraph::scroll((offset, 0))` rendern.
- Für `render_middle`: vor dem Rendern die Zeilennummer der selektierten Option (`2 + middle_selected`) gegen die innere Box-Höhe (`area.height - 2`) prüfen und `offset = selected_line.saturating_sub(inner_height - 1)` setzen.
- Links den aktiven Eintrag auch bei Fokus in Mitte/Rechts dezent markieren (z. B. `Color::Blue` oder `>`-Präfix, aktive Pane `Color::DarkGray`).
- Bei `E` (Export) automatisch `sync_plan_from_state` vor dem Exportieren aufrufen (oder `export_portage` direkt aus `state.engine` speisen).

---

## 3. Fachliche & Portage-Logik-Bugs

1. **Stilles Verschlucken fehlender `world`-Datei (`11_cli.rs`)**
   - `load_world` nutzt `read_world(&args.world).unwrap_or_default()`. Existiert `/var/lib/portage/world` nicht (z. B. im Ubuntu-Docker-Container ohne `--world`), startet das Programm still mit **0 Paketen** statt mit einer klaren Fehlermeldung.
   - **Fix:** Fehler propagieren (mit hilfreichem Hinweis auf `--world <PFAD>`), außer wenn explizit leer.

2. **`/etc/portage/make.profile` & Profil-`USE` werden ignoriert (`02_profile.rs`, `11_cli.rs`)**
   - `load_profile` liest nicht den Symlink `/etc/portage/make.profile`, sondern fällt hart auf `default/linux/amd64/23.0` zurück (auch wenn dein Host `no-multilib`, `systemd` oder `23.0/desktop` nutzt).
   - `load_profile_stack` liest aus `make.defaults` zwar `ARCH`, `ELIBC`, `KERNEL` und `USE_EXPAND`, aber **nicht `USE="..."`**! Gentoo-Basisprofile setzen zentrale Defaults (z. B. `ssl`, `zlib`, `unicode`, `udev`, `seccomp`) genau dort.
   - `read_flag_list` (`02_profile.rs`) für `use.mask` / `use.force` behandelt `-flag` falsch: Statt ein in Eltern-Profilen maskiertes/erzwungenes Flag wieder **aufzuheben** (`remove`), fügt es `flag` der Maske hinzu (`out.insert(token[1..])`).
   - `package.use`-Lookup macht nur exakten `cat/pkg`-String-Abgleich – Einträge mit Version/Slot wie `>=media-video/ffmpeg-6.0 svt-av1` oder `dev-libs/openssl:0 -ssl` werden ignoriert.

3. **Graph & Resolver (`06_graph.rs`)**
   - `graph.nodes` ist nur nach `cat/pkg` indiziert, nicht nach `(cat/pkg, slot)`. Zwei unterschiedliche Slots desselben Pakets (z. B. `dev-lang/python:3.12` und `:3.13`) kollabieren zu einem Knoten.
   - Versions- und Slot-Restriktionen aus `RDEPEND` (z. B. `>=dev-libs/foo-2.0`) werden bei `queue.push_back((pkg.cpn.clone(), pkg.cpn.clone(), None))` verworfen – `resolve_target` nimmt dann einfach `index.select(cpn)` (die absolut neueste Version).

4. **Delta- & Kaskaden-Lücken (`07_cascade.rs`, `08_delta.rs`)**
   - **`FlagDelta` kennt keine Paketdifferenz:** Obwohl `flag_delta` den neuen `ResolvedGraph` komplett berechnet, wirft es die Liste der hinzugekommenen/entfernten Pakete weg und behält nur `binpkg_bytes` und `download_bytes`.
   - **Kein Binhost = `0 B` Binpkg-Anzeige:** Ohne `--binhost` steht überall `0 B bin`, obwohl Quell-Downloads (`dl`) vorhanden sind. Eine Fallback-Schätzung oder ein VDB-Loader (`/var/db/pkg/*/*/SIZE`) fehlt noch.
   - **`QuickSwitch` teilweise leer (`07_cascade.rs`):** Bei `REQUIRED_USE` mit `!flag` oder bei „Parent-Constraint lockern“ ist `changes: vec![]` – der Quick-Switch ändert beim Anwenden dann nichts.

5. **Verify-Kommando (`10_export_verify.rs`)**
   - `VerifyRunner::against_configroot` ruft `emerge --config-root <dir> @world` auf. In Portage steuert `--config-root` aber nur `/etc/portage` (`package.use`, `make.conf`), **nicht** `world` (das liegt unter `--root` bzw. `/var/lib/portage/world`). Außerdem geht das lokale Kommando im Ubuntu-Container mangels `emerge` ins Leere, solange kein `--command "ssh ..."` übergeben wird.

---

# Teil 2: Verständliche Benutzer-Dokumentation & Praxis-Leitfaden

## Was macht `gentoo-dep-tui` überhaupt? (Das Grundprinzip)

Wenn du in Gentoo ein USE-Flag änderst (etwa `svt-av1` bei `ffmpeg` anschaltest), weißt du oft nicht vorher:
1. Zieht das Flag 15 neue Bibliotheken rein – oder ist die Bibliothek eh schon wegen eines anderen Pakets installiert (dann kostet das Flag **0 Byte extra**)?
2. Welche **Quell-Downloads** (`Manifest`-Tarballs) und welche **Binärpaket-Größen** (Binhost-`Packages`) kommen dazu?
3. Löst das Flag einen **`REQUIRED_USE`-Konflikt** mit einem anderen Paket aus?

`gentoo-dep-tui` beantwortet genau diese Fragen offline auf Basis der Gentoo-Metadaten (`metadata/md5-cache`) – so wie `ncdu` für Festplattenplatz, nur für Gentoo-Abhängigkeiten und USE-Flags.

```text
  Deine Konfig                   Repo-Daten                    Ergebnis
  ───────────                   ──────────                    ────────
  world             ──┐
  make.conf         ──┼──▶  RepoIndex (md5-cache)  ──▶  Laufzeit-Graph (RDEPEND)
  package.use       ──┤     + Manifest (Download)        ├─ Deltas pro Flag
  Profil (amd64)    ──┘     + Binhost (Binpkg-Größe)     ├─ Blame („Wer zieht das rein?“)
                                                         └─ Export (world + package.use)
```

> **Wichtige Unterscheidung bei den Größen:**
> - **`dl` (Download-Größe):** Stammt aus den `Manifest`-Dateien im Gentoo-Tree. Sie zeigt, wie viele Quell-Tarballs heruntergeladen werden müssen (Achtung: bei Paketen wie `linux-headers` ist der Kernel-Tarball 140 MB groß, das fertige Paket aber winzig).
> - **`bin` (Binpkg-Größe):** Stammt optional aus einer Binhost-`Packages`-Datei (`--binhost`). Das ist der bessere Maßstab für die echte Laufzeitgröße. **Gibst du kein `--binhost` an, zeigt das Programm bei `bin` überall `0 B` – achte dann auf die `dl`-Werte!**
> - **`BDEPEND` (Buildtime):** Werkzeuge, die nur zum Kompilieren nötig sind (z. B. `cmake`, `rust`, `llvm`), werden getrennt gezählt, damit sie nicht die Laufzeitgröße deines Systems/Containers verfälschen.

---

## Wo sucht das Programm seine Dateien? (Container vs. Host)

Das Programm liest standardmäßig die typischen Gentoo-Systempfade:

| Was? | Standard-Pfad (wenn kein Flag angegeben) | CLI-Flag zum Überschreiben |
| :--- | :--- | :--- |
| **`world`-Datei** | `/var/lib/portage/world` | `--world <PFAD>` |
| **Gentoo-Repo** | `/var/db/repos/gentoo` | `--repo <PFAD>` |
| **Profil** | `default/linux/amd64/23.0` (relativ zu `<repo>/profiles`) | `--profile <NAME>` (und `--profile-dir`) |
| **`make.conf`** | `/etc/portage/make.conf` | `--make-conf <PFAD>` |
| **`package.use`** | `/etc/portage/package.use` (Datei oder Ordner) | `--package-use <PFAD>` |
| **Binhost-Index** | *(keiner – optional!)* | `--binhost <PFAD/zur/Packages>` |

### Wichtig, wenn du im Ubuntu-Docker-Container arbeitest!
Im Container gibt es kein `/var/db/repos/gentoo` und kein `/var/lib/portage/world`. Du hast zwei Möglichkeiten:
1. **Mit dem Snapshot vom Host arbeiten** (z. B. unter `/tmp/gentoo-hil/`, falls du ihn vom Gentoo-Host gezogen hast):
   Dann **musst** du die Pfade per Flag übergeben, sonst läuft das Programm mit leerer `world` oder findet das Repo nicht:
   ```bash
   cargo run --release -- tui \
     --repo /tmp/gentoo-hil/repo \
     --world /tmp/gentoo-hil/world \
     --make-conf /tmp/gentoo-hil/make.conf \
     --package-use /tmp/gentoo-hil/package.use \
     --profile default/linux/amd64/23.0/no-multilib
   ```
   *(Tipp: Prüfe zuerst mit `index-check` statt `tui`, ob alle Pfade stimmen und wie viele Pakete geladen wurden!)*
2. **Direkt auf dem Gentoo-Host** reichen die Standardpfade (du solltest dort nur `--profile` explizit setzen, falls dein `/etc/portage/make.profile` nicht `default/linux/amd64/23.0` ist).

---

## Wie nutzt man die TUI? (Tasten & Aufbau)

Start mit `gentoo-dep-tui tui [FLAGS]` (optional `--plan plan.toml`).

### Aufbau (Tab `[1] Pakete & Alternativen`)
- **Linke Spalte:** Deine `world`-Pakete, dahinter eventuelle `||`-Entscheidungen aus Ebuilds, dahinter alle transitiv reingezogenen Abhängigkeiten (absteigend nach Binpkg-Größe sortiert).
- **Mittlere Spalte:** Alle schaltbaren USE-Flags des links gewählten Pakets mit Status `[x]`/`[ ]` und dem **Marginal-Delta** (`bin` / `dl`). Steht dort `CONFLICT`, blockiert eine `REQUIRED_USE`- oder `pkg[flag]`-Regel das Flag.
- **Rechte Spalte (`Warum ist das hier?`):** Die **Blame-Kette** – zeigt bei Abhängigkeiten genau an, welches `world`-Paket sie über welches Flag (`[+ssl]`) reingezogen hat. Wenn zu dem Paket ein `[[choice]]`-Eintrag in `plan.toml` existiert, stehen hier auch dessen `Pros` und `Cons`.
- **Fußzeile:** Gesamt-Binpkg, Gesamt-Download, Buildtime (`BDEPEND`), Paketanzahl, Konfliktzähler und Tastenbelegung.

### Wichtigste Tasten
- `1` / `2`: Wechseln zwischen **Pakete & Flags** (`1`) und **Global Big Levers** (`2`, zeigt welche USE-Flags systemweit am meisten Platz fressen).
- `Tab`: Zwischen linker und mittlerer Spalte wechseln.
- `↑`/`↓` oder `j`/`k`: In der aktiven Liste bewegen.
- `/`: Paketliste filtern (z. B. `/ffmpeg`, mit `Enter` oder `Esc` bestätigen).
- `Space`: Das aktuell ausgewählte USE-Flag in der mittleren Spalte umschalten und live neu berechnen lassen (bei Konflikt öffnet sich ein Quick-Switch-Fenster).
- `Enter`: Auf einem `||`-Eintrag links die nächste Alternative durchschalten; auf einem Paket mit `CONFLICT` das Konflikt-Fenster öffnen.
- `S`: Aktuellen Stand in `plan.toml` speichern.
- `E`: Als Portage-Konfiguration (`world`, `package.use`) nach `/tmp/gentoo-export` exportieren (**Wichtig wegen aktuellem Bug:** Immer direkt vorher `S` drücken, damit deine letzten Toggles im Plan stehen!).
- `q` / `Ctrl-C`: Beenden.

---

## Konkrete Anwendungsfälle Schritt für Schritt

### Anwendungsfall 1: „Ich möchte in `ffmpeg` JPEG-zu-AV1-Konvertierung unterstützen – wie viele Pakete und welche Größe kostet das?“

Für JPEG → AV1 brauchst du bei `media-video/ffmpeg` typischerweise JPEG-Support (meist schon über Standard-Codecs/Bibliotheken bzw. `jpeg`/`jpeg2k` je nach Bedarf) und einen **AV1-Encoder** – in Gentoo sind das vor allem die USE-Flags `svt-av1` (schnell, Standard-Empfehlung) oder `aom` (Referenz-Encoder) bzw. `rav1e` (Rust).

#### So gehst du heute mit dem Tool vor:
1. **Schnell-Check über die CLI (empfohlen, um Wartezeit in der TUI zu vermeiden):**
   ```bash
   gentoo-dep-tui deltas media-video/ffmpeg \
     --repo ... --world ... --make-conf ... --package-use ... --profile ...
   ```
   Du bekommst eine Liste aller `ffmpeg`-Flags samt Status `[x]`/`[ ]` und Byte-Delta, z. B.:
   - `[ ] svt-av1  (+0 B bin / +X B dl)`
   - `[ ] aom  (+0 B bin / +Y B dl)`
   - `[ ] jpeg2k ...`

2. **Zusätzliche Paket-Anzahl und exakte Paketnamen herausfinden:**
   *Ehrlicher Hinweis aus dem Code-Review:* Die CLI-Ausgabe `deltas` zeigt aktuell nur die **Byte-Differenz**, noch nicht die Liste der neu hinzugekommenen Pakete. So findest du trotzdem genau heraus, **welche** und **wie viele** Pakete dazukommen:
   - **Weg A (in der TUI):**
     1. TUI starten → `/` tippen → `ffmpeg` eingeben → `Enter`.
     2. In der Fußzeile die aktuelle Zahl `Pakete N` merken.
     3. Mit `Tab` in die Mitte wechseln, auf `svt-av1` (oder `aom`) navigieren und `Space` drücken.
     4. Die neue Zahl in der Fußzeile ablesen (z. B. `Pakete N+1` → also genau 1 neues Paket).
     5. In der linken Spalte mit `/` nach `svt-av1` suchen und in der rechten Spalte (`Blame`) prüfen, dass es von `media-video/ffmpeg [+svt-av1]` gezogen wurde.
   - **Weg B (exakter Diff per CLI – am genauesten):**
     ```bash
     # 1. Vorher-Liste speichern
     gentoo-dep-tui graph --repo ... --world ... > /tmp/vorher.txt
     # 2. Flag testweise in eine Kopie deiner package.use schreiben
     cp /pfad/zu/package.use /tmp/test_package.use
     echo "media-video/ffmpeg svt-av1" >> /tmp/test_package.use
     # 3. Nachher-Liste erzeugen und vergleichen
     gentoo-dep-tui graph --repo ... --world ... --package-use /tmp/test_package.use > /tmp/nachher.txt
     diff -u /tmp/vorher.txt /tmp/nachher.txt
     ```
     Der `diff` zeigt dir direkt:
     - die exakte Differenz bei `Pakete`, `Download` und `Binpkg` (wenn `--binhost` übergeben wurde),
     - plus jede einzelne neu hinzugekommene Paketversion (z. B. `+  media-libs/svt-av1-3.x.x`).

   *(Tipp zur Laufzeit-Performance: Wenn dein `ffmpeg` bereits andere Pakete wie `dav1d` über `world` installiert hat, siehst du sofort ein kleineres Delta, als wenn sie neu dazukommen.)*

---

### Anwendungsfall 2: „Wie kann ich Alternativen für `openssh` vergleichen?“

Hier muss man in Gentoo (und im Tool) zwei Fälle unterscheiden:

1. **Fall A: Eine Abhängigkeit fordert `|| ( net-misc/openssh ... )` (z. B. über ein `virtual/`-Paket)**
   - In der linken Spalte der TUI tauchen solche Gruppen automatisch als `|| <paket>||cand1,cand2` auf.
   - Durch Markieren der Zeile und `Enter` schaltest du zwischen den Kandidaten um und siehst in der Fußzeile sofort, wie sich Gesamtgröße und Paketanzahl verändern.

2. **Fall B: `openssh` steht direkt in deiner `world`-Datei (Normalfall)**
   - Ein Ebuild von `openssh` enthält keine Liste „alternativer SSH-Server“ (wie `net-misc/dropbear` oder `net-misc/tinyssh`).
   - Dafür ist die **`plan.toml`** gedacht: Du kannst dort Alternativen als `[[choice]]`-Block inklusive Vor- und Nachteilen eintragen und mit `gentoo-dep-tui` exakt vermessen, was jede Alternative an Abhängigkeiten und Größe auf deinem System bedeutet.

#### Praktischer Vergleich heute (Workaround, da der interaktive `[[choice]]`-Switcher in der TUI noch fehlt):
Erstelle eine `plan.toml` aus deiner `world`:
```bash
gentoo-dep-tui init --world /pfad/zu/world --plan plan.toml
```
Entferne `net-misc/openssh` aus `[baseline].world` und füge einen `[[choice]]`-Block hinzu:
```toml
[[choice]]
id = "ssh-server"
required = true
description = "SSH Daemon"
selected = 0

[[choice.option]]
atom = "net-misc/openssh"
pros = "Standard, volle OpenSSH-Features, SFTP, Kerberos/PAM"
cons = "Zieht OpenSSL/zlib, groesser"
suggested_use = ["-X", "-kerberos"]

[[choice.option]]
atom = "net-misc/dropbear"
pros = "Sehr klein, fuer Container/Embedded ideal"
cons = "Kein vollstaendiges OpenSSH-Key-Format/Agent-Feature-Set"
suggested_use = ["minimal"]
```
Da die TUI aktuell noch keine Taste hat, um `selected` live umzuschalten, vergleichst du die beiden Optionen einfach durch Export/Graph-Lauf mit `selected = 0` vs. `selected = 1`:
```bash
# Für Option 0 (openssh):
gentoo-dep-tui export --plan plan.toml --out /tmp/cmp-openssh
gentoo-dep-tui graph --repo ... --world /tmp/cmp-openssh/world --package-use /tmp/cmp-openssh/package.use > /tmp/res-openssh.txt

# Dann in plan.toml `selected = 1` setzen (dropbear):
gentoo-dep-tui export --plan plan.toml --out /tmp/cmp-dropbear
gentoo-dep-tui graph --repo ... --world /tmp/cmp-dropbear/world --package-use /tmp/cmp-dropbear/package.use > /tmp/res-dropbear.txt

diff -u /tmp/res-openssh.txt /tmp/res-dropbear.txt
```
*(In der TUI mit `--plan plan.toml` siehst du rechts immer die `Pros`/`Cons` der aktuell gewählten Option.)*

---

### Anwendungsfall 3: „Welche Alternativen gibt es für `nginx`?“

**Wichtige Erwartungsklärung:** `gentoo-dep-tui` hat **keine** eingebaute Datenbank „welches Paket kann ein anderes ersetzen“ (wie z. B. Debian `Provides` für generische Software-Kategorien, außer wo Gentoo ein explizites `virtual/*`-Ebuild pflegt, z. B. `virtual/httpd-cgi` oder `virtual/httpd-php`). Die Idee hinter dem Workflow aus `plan.toml` ist, dass **du oder ein LLM** in `plan.toml` die Alternativen vorschlägst, und `gentoo-dep-tui` rechnet dann **deterministisch und ohne Raten** aus, was jede Variante in deinem konkreten System kostet.

Typische Alternativen zu `www-servers/nginx` im Gentoo-Tree (Kategorie `www-servers/`), die du über `plan.toml` gegeneinander messen kannst:
- **`www-servers/nginx`**: Modularer Klassiker (C), viele optionale Module über `NGINX_MODULES_HTTP` / USE-Flags; benötigt i. d. R. `dev-libs/libpcre2`, `dev-libs/openssl`, `sys-libs/zlib`.
- **`www-servers/caddy`**: Go-basiert, automatisches HTTPS/ACME eingebaut; großes Einzel-Binary, dafür kaum C-Laufzeitbibliotheken (aber `dev-lang/go` in `BDEPEND`!).
- **`www-servers/lighttpd`**: Sehr schlanker C-Webserver für einfache statische Auslieferung / kleine Container.
- **`www-servers/apache`**: Schwergewicht mit umfangreichem Modul-Ökosystem (`app-arch/*`, `dev-libs/apr*`).
- **`www-servers/h2o`** oder **`www-servers/uwsgi`** (je nach Einsatzzweck Reverse-Proxy vs. App-Server).

#### So vergleichst du sie direkt mit deinen vorhandenen System-Paketen:
1. Prüfe zuerst im Repo, welche `www-servers/*`-Pakete existieren und auf deiner Architektur (`amd64`) verfügbar sind:
   ```bash
   ls /pfad/zu/repo/www-servers/
   ```
2. Trage deine Favoriten wie bei Anwendungsfall 2 als `[[choice]]` in `plan.toml` ein (oder teste sie über separate kleine `world`-Kopien mit `gentoo-dep-tui graph`):
   - Achte besonders auf **`BDEPEND-extra`** vs. **Runtime (`Pakete`, `Download`, `Binpkg`)**! Gerade bei `caddy` siehst du in `graph` den Kernvorteil von `gentoo-dep-tui`: `dev-lang/go` taucht nur in `BDEPEND-extra` auf (fällt bei Multi-Stage-Docker-Builds komplett weg), während `nginx` seine C-Bibliotheken in der Runtime behält.
   - Prüfe danach mit `gentoo-dep-tui deltas www-servers/nginx`, welche `nginx`-Flags (z. B. `http2`, `ssl`, `pcre`, `threads`, `vim-syntax`) auf deinem System überhaupt noch Zusatzpakete ziehen oder bereits kostenlos sind.
