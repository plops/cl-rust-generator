mein ziel ist gentoo-dep-tui soweit zu bringen, dass ein nutzer (oder
ein ai agent) moeglichst effizient herausfinden kann welche packete
und use flag kombinationen zu einem gentoo system fuehren dass
moeglichst klein ist oder wenig packete als abhaengigkeiten hat (um
angriffsflaeche zu verkleinern), schnell compiliert, wenige packete
runterladen muss, moeglichst viele packete von binhost verwendet. all
diese anwendung sollten unterstuetzt werden. kannst du vorschlagen wie
die gui verbessert werden koennte um die anwendung weiter zu
vereinfachen?

eine frage die sich mir stellt ist z.b. wie kann ich emacs mit xaw3d
als gui nutzen und mein gentoo so konfigurieren dass gtk rausfliegt
(aber moeglichst viel von der aktuellen funktionaliteat erhalten
bleibt) und wieviel groesse und downloads und anzahl zu compilierender
packete wuerde ich dadurch einsparen?  ich sende dir auch die
zusaetzlichen quelltexte die du angefragt hast

Der Quelltext von `08_delta/shared.rs` und `08_delta.rs` erklärt die Auffälligkeiten aus den Screencaps exakt — und zeigt sehr deutlich, wo das Tool derzeit an deinem eigentlichen Ziel vorbeischießt.

Ich gliedere die Antwort in drei Teile:
1. **Warum `shared.rs` & `08_delta.rs` dich beim `emacs`/`gtk`-Fall in die Irre führen (konkrete Code-Ursachen)**
2. **Was sich aktuell schon über `emacs` + `Xaw3d` ohne `gtk` sagen lässt (und was noch fehlt)**
3. **Konkrete Verbesserungsvorschläge für TUI & AI-Agents** (für minimale Größe, wenig Pakete, wenig Downloads, wenig Kompilierzeit / maximalen Binhost)

---

### 1. Die Ursachen im Code (`shared.rs` & `08_delta.rs`)

#### A. Der „Transitive-Leaf-Bug“ in `08_delta/shared.rs`
In `shared_removal_impact` passiert Folgendes:
1. `pulled_deps(&base.graph, package, flag)` (Zeile 177–198) startet bei den direkten Flag-Kindern `(package, Some(flag))` und läuft über **`base.out_edges` rekursiv durch die gesamte transitive Hülle** — bis ganz nach unten zu Basispaketen wie `sys-libs/readline`, `sys-libs/ncurses`, `sys-libs/zlib`, `dev-libs/openssl`, `dev-lang/python` oder `dev-libs/glib`.
2. Danach behält `shared` (Zeile 80–89) **alle** Knoten aus dieser Menge, die in `outcome_alone` noch existieren.
3. `collect_blockers` (Zeile 212–247) klettert dann von *jedem* dieser überlebenden Basispakete aus wieder hoch bis zu `world`.

**Warum das bei `mupdf[javascript]` und `emacs[gtk]` explodiert:**
* Bei `mupdf[javascript]` wird das einzige direkte Paket `dev-lang/mujs` beim Abschalten **sofort entfernt** (`-1 pkg`). `mujs` hat gar keinen Blocker. Weil `mujs` aber von `readline`/`ncurses` abhängt und `bash`, `python` und `fdupes` ebenfalls `readline`/`ncurses` nutzen, meldet `shared.rs` Blocker für `readline`/`ncurses` (`-53 pkg`, inkl. `sudo`, `docker`, `emacs`!).
* Genau dasselbe passiert bei `emacs[gtk]`: `gtk+` hängt transitiv an Basispaketen (z. B. `glib`, `zlib`, `openssl`, `python`), und über diese Basispakete zieht `collect_blockers` unbeteiligte `world`-Pakete wie `app-admin/sudo`, `app-arch/dpkg` oder `app-containers/docker` in `world_blockers`.

**Logischer Kernfehler:** Wenn `docker` `zlib` braucht, blockiert `docker` **nicht** das Entfernen von `gtk+`! Für das Loswerden von `gtk+` zählen nur die Blocker von `gtk+` selbst (und von Paketen, die ihrerseits *nur* über `gtk+` oder seine direkte GTK-Schicht reinkommen) — niemals die Blocker gemeinsamer OS-Basispakete.

#### B. Was `shared.rs` zusätzlich fehlt
* `if !node.use_flags.contains(flag) { return Ok(None); }` (Zeile 67): Es gibt keine Analyse für das Ziel *„Wie bekomme ich Paket X (z. B. `x11-libs/gtk+`) aus dem System?“*, unabhängig von einem einzelnen Flag.
* `pulled_deps` berücksichtigt nicht, welche Pakete im Binhost fehlen (also lokal kompiliert werden müssen).

#### C. `big_levers_impl` in `08_delta.rs`
* Zeile 334–385 sammelt zuerst alle Flags, die irgendwo aktiv sind (`turn_on = false`), und danach in `known_off` alle Flags, die **nirgends** aktiv sind (`turn_on = true`).
* `BigLever` speichert diese Richtung (`turn_on`) nicht mit. Daher kann weder die CLI noch die TUI unterscheiden, ob ein positives Delta durch **Einschalten** (`kde`, `frei0r`, `flite`) oder durch **Abschalten + `!flag?`-Kaskade** (`X +111 MB` auf 16 Paketen!) entstanden ist.

---

### 2. Zu deiner konkreten Frage: `emacs` mit `Xaw3d` statt `gtk`

**Was aus den vorliegenden Daten sicher feststeht:**
1. **`emacs` selbst mit `Xaw3d` statt `gtk`**:
   * In `package.use`: `app-editors/emacs gui Xaw3d -gtk` (bzw. `-gtk -motif -athena`, falls die sonst gesetzt wären).
   * Laut Screencap 3 läuft `emacs` damit im grafischen Modus über `x11-libs/libXaw3d` (`+220,0 kB bin / +470,7 kB dl`, `+1 Paket`).
   * `libXaw3d` hat einen Binhost-Treffer (`220 kB bin > 0`), es fällt dafür also **kein einziges zusätzliches Paket zum Kompilieren** an.
2. **Warum sich die echte Gesamtersparnis eines kompletten `gtk`-Rauswurfs aus den Screencaps noch nicht ablesen lässt:**
   * Die angezeigten `-163,8 MB / -106 pkg` bzw. `-591,4 MB / -353 pkg` bei `emacs[gtk]` sind durch den `pulled_deps`-Bug in `shared.rs` stark überhöht (sie entfernen `sudo`, `docker`, `dpkg` etc. mit).
   * Um exakt zu beantworten, *welche* Pakete in deinem `world` wirklich `x11-libs/gtk+` erzwingen (z. B. ob `firefox-bin`, `wireshark`, `freerdp` oder `tigervnc` GTK brauchen bzw. auf Qt/CLI umgestellt werden können) und wie viele der aktuell **50 fehlenden Binhost-Pakete** (`554 - 504`) sowie wie viel **`Build-Only`** dadurch wegfällt, fehlt dem Tool aktuell:
     1. Der Fix in `shared.rs` (keine Traversal-Abstiege in gemeinsam genutzte Basispakete),
     2. eine **Paket-Eliminierungs-Analyse** direkt auf `x11-libs/gtk+` (statt nur auf einem Flag von `emacs`),
     3. die Zählung von **nicht-Binhost-Paketen** („muss kompiliert werden“).

---

### 3. Konkrete Verbesserungsvorschläge (GUI & AI-Agent)

#### Priorität 1: Logik- & Metrik-Fundament (nützt Mensch und AI)

1. **`SizeMetrics` um Kompilier- und Angriffsflächen-Metriken erweitern (`01_types.rs`, `06_graph`, `08_delta.rs`):**
   Aktuell misst `SizeMetrics` nur `binpkg_bytes` und `download_bytes`. Ergänze:
   * `package_count` / `delta_packages` (Angriffsfläche)
   * `compile_packages` (= Pakete im Runtime-Graph mit `binpkg == 0`, also ohne Binhost-Treffer!)
   * `build_only_packages` & `build_only_compile_packages` (aus `build_extra`, das ohnehin schon existiert)
   * *Effekt:* Bei jedem USE-Flag, jedem Big Lever und jeder Eliminierung sieht man sofort z. B.:
     `+1 pkg (1 compile! / no binhost)` vs. `+1 pkg (binhost hit)` oder `-12 pkg (-4 compile, -180 MB BDEPEND)`.

2. **Fix in `08_delta/shared.rs` (Präzise Blocker-Analyse):**
   * **Problem beheben:** Starte `shared` primär bei den **direkten Flag-Kindern** (`pulled_direct`), oder steige nur in Kinder ab, deren einziger Grund die direkte Schicht ist — **niemals** weiter absteigen, sobald ein Knoten bereits in `outcome_alone` überlebt (denn ab dort gehört er zur geteilten OS-Basis wie `zlib`/`readline`/`openssl`, und seine Aufwärts-Blocker sind nicht die Blocker des Flags!).
   * Noch besser für Fälle wie `gtk+`: Wenn ein direktes Kind `gtk+` in `outcome_alone` überlebt, suche nur die Blocker von `gtk+` (und ggf. exklusiven `gtk+`-Geschwistern), statt über `zlib` bei `docker` zu landen.

3. **`big_levers` (`08_delta.rs`) aufräumen:**
   * Speichere `turn_on: bool` in `BigLever`.
   * Zeige in CLI & TUI klar `[OFF]` vs. `[ON]` (und markiere, wenn `[OFF]` durch `!flag?` trotzdem teurer wird, wie bei `X` mit `+111 MB`).
   * Optionales Flag / TUI-Filter: *„Nur Einsparungen (ohne neue Konflikte)“*.

#### Priorität 2: Neue TUI-Funktionen für die GUI

1. **Neuer Modus / Aktion auf Paketen: `[X] Paket loswerden (Cut-Analyse)`**
   * Man navigiert links auf ein schweres oder unerwünschtes Paket — z. B. **`x11-libs/gtk+`**, **`sys-devel/llvm`**, **`dev-lang/rust`** oder ein Paket ohne Binhost — und drückt `x` (oder `Enter` im Aktionsmenü).
   * Die rechte Pane zeigt den **minimalen Schnitt**, um genau dieses Paket aus dem Graphen zu entfernen:
     1. **Per USE-Flag abschalten:** z. B. `app-editors/emacs [-gtk]`, `net-analyzer/wireshark [-gtk +qt6]`, `...`
     2. **Per `||`-Alternative umschalten:** z. B. falls ein Virtual auf GTK- vs. Non-GTK-Backend zeigt.
     3. **Verbleibende harte `world`-Blocker:** z. B. `www-client/firefox-bin` (falls unvermeidlich).
     4. **Exaktes Gesamt-Delta (simuliert!):** `-N Pakete`, `-M zu kompilieren`, `-X MB Binpkg`, `-Y MB DL`, `-Z MB Build-Only`.
   * *Mit einer Taste (`A` = „Flags anwenden“)* könnte man alle Flag-Blocker aus Punkt 1 direkt als Override übernehmen und den Effekt live testen.

2. **Alternative- / Ersatz-Vergleich bei `REQUIRED_USE`-Gruppen (`emacs`: `gtk` vs. `Xaw3d` vs. `motif` vs. `no-gui`)**
   * Heute siehst du bei `emacs` erst nach dem Einschalten von `gui`, was `gtk` vs. `motif` vs. `Xaw3d` tut.
   * Wenn `REQUIRED_USE` eine `^^ (...)` oder `?`-Gruppe erkennt (oder in einer Flag-Gruppe `gui`/`Xaw3d`/`gtk`/`motif`), könnte ein kleines Vergleichs-Popup bzw. eine Sortierung/Gruppierung direkt nebeneinander zeigen:
     * `gui + Xaw3d`: `+1 pkg (0 compile), +220 kB bin`
     * `gui + gtk`: `+0 pkg (gtk+ bereits da)` bzw. mit `gtk`-Eliminierung: `+X pkg`
     * `gui + motif`: `+1 pkg (1 compile!), +7.9 MB dl`

3. **Sortierung & Filter in Tab 1 und Tab 2 (`Big Levers`)**
   * In **Tab 2 (`Big Levers`)** per Taste `s` umschalten zwischen:
     * `Größe (bin)` (Standard)
     * `Pakete (-pkg)` (Angriffsfläche minimieren)
     * `Kompilier-Last (-compile, -build-only)` (alles ohne Binhost loswerden!)
     * `Downloads (-dl)`
   * Filter `c`: Konfliktfreie Hebel zuerst / Konflikte ausblenden.
   * In **Tab 1 (Paketliste links)** per Taste `s` sortierbar machen nach:
     * Alphabet (Standard)
     * **„Fehlt im Binhost (muss kompiliert werden)“** ganz oben! (Für dein Binhost-Ziel Gold wert: du siehst sofort die 50 Pakete, die du selbst bauen musst, und kannst über deren Blame-Kette entscheiden, welches USE-Flag sie reinzieht.)
     * **Größter exklusiver Subgraph / meiste Pakete**.

4. **„Baseline vs. Entwurf“-Anzeige in der Fußzeile**
   * Sobald man experimentiert (z. B. `emacs -gtk +Xaw3d` + weitere Flags abschaltet), zeigt die Fußzeile aktuell nur den neuen Absolutwert (`Pakete 554`, `504/554 Hits`).
   * Besser: Vergleich zur unveränderten Start-Konfiguration anzeigen:
     `Pakete 512 (-42) | Kompilieren 34 (-16) | Binpkg 680 MB (-78.8 MB) | DL 2910 MB (-403 MB) | Konflikte 0`
   * Dazu eine einfache **`u` (Undo)** / **`R` (Reset auf Baseline)** Taste, damit man mutig USE-Flag-Kombinationen durchprobieren kann.

#### Priorität 3: Optimierungen speziell für AI-Agents (CLI)

Ein AI-Agent nutzt selten die interaktive TUI über Terminal-Screencaps, sondern will schnell, deterministisch und strukturiert Hypothesen testen:

1. **`--json` für alle Subcommands (`graph`, `deltas`, `big-levers`, `blame`)**
   * Liefert saubere Objekte statt Menschentext (inkl. `compile_packages`, `missing_binhost_list`, `build_only`, `turn_on`, `conflicts`).
   * Wichtig bei `graph --json`: Liste der `missing_binhost` Pakete mit ihren Blame-Ketten, damit ein Agent sofort sieht: *„Welche 5 USE-Flags verursachen 30 meiner 50 lokalen Kompilierungen?“*

2. **Neues CLI-Subcommand `cut <cat/pkg>` (analog zum TUI-`[X] Paket loswerden`)**
   * Beispiel: `gentoo-dep-tui cut x11-libs/gtk+ --json`
   * Gibt strukturiert zurück: `flag_blockers`, `world_blockers`, `savings_without_world_blockers`, `savings_all`. Genau das beantwortet deine `gtk`-Frage für Mensch und Agent in unter einer Sekunde.

3. **Neues CLI-Subcommand `simulate` (oder Batch-Modus auf `plan.toml` / CLI-Overrides)**
   * Beispiel: `gentoo-dep-tui simulate --set app-editors/emacs=+gui,+Xaw3d,-gtk --set app-accessibility/at-spi2-core=-introspection --json`
   * Oder `batch`-Modus per `stdin` (JSON-Zeilen rein → JSON-Ergebnisse raus): Da `index_load` auf deinem System **`~500 ms`** von insgesamt **`650 ms`** ausmacht, spart ein `batch`/REPL-Modus, der den Index einmal lädt, für einen Agenten bei 20 getesteten Kombinationen rund **10 Sekunden** und liefert in Millisekunden exakte Resultate.

4. **Automatischer Konflikt-Erklärer (`REQUIRED_USE`-Hint)**
   * Statt nur `CONFLICT` (wie bei `git[-curl]` oder `mupdf[-javascript]` oder `ffmpeg[+opencolorio]`) sollte sowohl in der TUI als auch im JSON die **verletzte Regel** stehen (z. B. `webdav erfordert curl` oder `opencolorio erfordert ...`), bzw. welche Flag-Kombination den Konflikt auflöst (`-webdav`). Dann muss weder der Mensch noch der Agent raten, warum `CONFLICT` erscheint.

---

### Zusammenfassung der schnellsten nächsten Schritte
Wenn du die Entwicklung priorisieren willst, bringt diese Reihenfolge den größten Nutzen pro Aufwand:
1. **`08_delta/shared.rs` fixen** (keine Abstiege in überlebende OS-Basis-Deps wie `readline`/`zlib` -> `Voll-Ersparnis` wird sofort realistisch).
2. **`compile_packages` (`binpkg == 0`) & `build_only` in `SizeMetrics` + JSON-Output** aufnehmen (macht „schnell bauen / maximaler Binhost“ sofort messbar).
3. **`cut <cat/pkg>` („Paket loswerden“)** für CLI + TUI (`x`-Taste in der Paketliste) bauen -> löst genau Aufgaben wie *„Wie kriege ich `gtk+` oder `llvm` komplett raus?“*.
4. **`BigLever.turn_on` + Sortier-/Filter-Tasten (`-pkg`, `-compile`, „nur fehlende Binhost-Pakete“)** in der TUI ergänzen.
