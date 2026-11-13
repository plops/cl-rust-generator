# Aufgabenplan (`tasks.md`)

Dieser Aufgabenplan wird **strikt seriell** von oben nach unten abgearbeitet. Jeder Schritt wird sofort durch Tests und die Rust-Gates (`cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test`) validiert.

**Reihenfolge-Regel:** Jeder fachliche Modus durchläuft nacheinander:
1. **Implementierung**
2. **Host-Tests** (synthetische `tempfile`-Fixtures)
3. **HIL-Nachweis** (echte Gentoo-Host-Daten via `ssh kiel@localhost -i ~/.ssh/from_android` bzw. Snapshot & `emerge -p`-Abgleich)

Erst wenn **alle** Modi inklusive ihrer HIL-Nachweise abgeschlossen sind, beginnen die **TUI-Tasks**.

---

## Phase A: Setup & Kontext-Validierung

### T01 — Kontext lesen, Branch anlegen & Crate-Skelett aufsetzen
- **Schritte:**
  1. Alle in `plan.md` (Abschnitt 2) genannten Dateien lesen (`prompt.txt`, `deps.md`, `research.md`, `lu-zero__portage-metadata.md`, `pkgcraft.md`, `pkgcore.md`, Repo-Root).
  2. Mit DeepWiki MCP die APIs von `lu-zero/portage-atom`, `lu-zero/portage-metadata` und `ratatui/ratatui` kurz verifizieren.
  3. Git-Status prüfen, auf `master` wechseln und Feature-Branch anlegen (`git checkout -b feature/gentoo-dep-tui`).
  4. Eigenständiges Rust-Crate (Edition `2024`) gemäß Repo-Konvention anlegen (ohne Root-Workspace-`Cargo.toml`). Abhängigkeiten aus `deps.md` in `Cargo.toml` eintragen.
  5. Leeres Modul-Gerüst (`lib.rs`, `main.rs`, `01_types.rs` bis `14_tui_runner.rs`) mit Platzhalter-Modulen anlegen.
- **Validierung:**
  - `cargo check`, `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings` laufen fehlerfrei durch.
  - Commit: `chore(scaffold): initialize standalone crate and module layout`

### T02 — Gentoo-Host-Zugang & HIL-Testdatensatz prüfen
- **Schritte:**
  1. Per `ssh kiel@localhost -i ~/.ssh/from_android` Leseberechtigung auf `/var/db/repos/gentoo/metadata/md5-cache`, `/etc/portage/`, `/var/lib/portage/world` und Verfügbarkeit von `emerge --version` prüfen.
  2. Für schnelle, wiederholbare Container-HIL-Tests einen Read-Only-Snapshot der benötigten Metadaten (`world`, `make.profile`-Kette, `make.conf`, `package.use`, `package.mask`, relevante/vollständige `md5-cache`- & `Manifest`-Dateien sowie eine Gentoo-Binhost-`Packages`-Indexdatei) nach `/tmp/gentoo-hil/` synchronisieren/ablegen (nicht ins Git-Repo committen!).
  3. Referenz-Output von `emerge --pretend --emptytree --with-bdeps=n --verbose @world` auf dem Host sichern (`/tmp/gentoo-hil/emerge_world_ref.txt`), sofern `world` auf dem Host auflösbar ist (sonst kleine Test-`world` definieren und auf dem Host mit `emerge -p` auflösen).
- **Validierung:**
  - `/tmp/gentoo-hil/` enthält `md5-cache`, `Manifest`-Beispiele, Profil-Dateien, `Packages` (Binhost) und `emerge_world_ref.txt`.

---

## Modus 1: Repo-Index, Profil-Filter, Manifest & Binhost (`index-check`)

### T03 — Implementierung Modus 1 (`index-check`)
- **Dateien:** `01_types.rs`, `02_profile.rs`, `03_manifest.rs`, `04_binhost.rs`, `05_repo_index.rs`, `11_cli.rs` (Subcommand `index-check`), `lib.rs`, `main.rs`.
- **Schritte:**
  1. `01_types.rs`: Grundtypen (`PkgId`, `FlagId`, `SizeMetrics { binpkg_bytes, download_bytes }`, `UseState`, `PkgRecord`, strukturierte Fehler mit `thiserror`).
  2. `02_profile.rs`: Einlesen von `world`, `make.conf` (`USE`, `ACCEPT_KEYWORDS`), `package.mask`, `package.use`, Profil-Hierarchie (`parent`-Auflösung für `use.mask`, `use.force`, `package.mask`, `make.defaults`).
  3. `03_manifest.rs`: Parser für `Manifest` (`DIST <filename> <bytes> ...`) in `HashMap<String, u64>`.
  4. `04_binhost.rs`: Parser für Gentoo-Binhost-`Packages`-Stanzas (`CPV`, `USE`, `SIZE`, `RDEPEND`).
  5. `05_repo_index.rs`: Paralleles Einlesen von `metadata/md5-cache/` (`rayon` + `walkdir` + `portage-metadata`). Kapselung hinter `RepoIndex`: Versionselektion pro `cat/pkg` unter Filterung von `KEYWORDS` (z. B. `amd64`) und `package.mask` (keine `9999`-Live-Ebuilds!). Zuordnung der `Manifest`- und Binhost-`SIZE`-Werte.
  6. `11_cli.rs`: Subcommand `index-check --repo <path> --binhost <path> --world <path>` gibt Statistik (Anzahl Kategorien/Pakete, Trefferquote Binhost-`SIZE`, Ladezeit in ms) aus.
- **Validierung:** `cargo build` und `cargo clippy --all-targets -- -D warnings` grün.

### T04 — Host-Tests Modus 1 (`index-check`)
- **Schritte:**
  1. Unit-Tests in `02_profile.rs` bis `05_repo_index.rs` und Integrationstest in `tests/host_integration.rs` mit `tempfile`-Mini-Repo:
     - `9999`-Live-Ebuild und maskierte Version werden ignoriert; höchste stabile Version mit passendem `KEYWORD` wird gewählt.
     - `Manifest`-Parser summiert exakte `DIST`-Bytes passend zu `SRC_URI`-Einträgen.
     - `Binhost`-Parser liest `CPV`, `SIZE` und `USE` korrekt ein.
     - Profil-Vererbung (`parent`) wendet `use.mask`/`use.force` korrekt an.
  2. Edge-Cases von `portage-metadata` (leere Felder, mehrzeilige/ungewöhnliche Werte) gezielt testen.
- **Validierung:** `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` komplett grün.

### T05 — HIL-Nachweis Modus 1 (`index-check`)
- **Schritte:**
  1. `cargo run --release -- index-check` gegen `/tmp/gentoo-hil/` (echter Gentoo-Tree + Binhost-`Packages`) ausführen.
  2. Ladezeit messen (Ziel: Einlesen des erreichbaren/vollen Index deutlich unter 1 s bzw. Startzeit dokumentieren) und Stichproben (z. B. `sys-kernel/linux-headers`, `dev-libs/openssl`, `www-servers/nginx`) auf korrekte Version, `Manifest`-Bytes und Binhost-`SIZE` prüfen.
  3. Regressionstest in `tests/hil_regression.rs` anlegen (wird ausgeführt, wenn `GENTOO_HIL_REPO` gesetzt ist, sonst sauber übersprungen).
- **Validierung:**
  - `GENTOO_HIL_REPO=/tmp/gentoo-hil cargo test` grün + `cargo test` (ohne Env-Var) grün.
  - Commit: `feat(index): implement md5-cache, profile, manifest and binhost loaders`

---

## Modus 2a: `RDEPEND`-Graph, `||`-Alternativen & Blame-Chain (`graph`, `blame`)

### T06 — Implementierung Modus 2a (`graph`, `blame`)
- **Dateien:** `06_graph.rs`, `11_cli.rs` (Subcommands `graph`, `blame`).
- **Schritte:**
  1. `06_graph.rs`:
     - Mapping aktiver Pakete und USE-Flags auf kompakte Integer-IDs (`Vec`-basiert).
     - Auswertung von `DepEntry` (`portage-atom::DepEntry::evaluate_use`) gegen effektive USE-Flags.
     - Strikte Trennung von `RDEPEND` (Runtime) vs. `BDEPEND`/`DEPEND` (Buildtime).
     - Reference-Counting je Knoten im aktiven `RDEPEND`-Graphen (Basis für exakte Marginal-Deltas).
     - Erkennung von `|| ( a b )` (`AnyOf`) im `RDEPEND`-Baum und Speicherung als explizite `AlternativeChoice` (Default: erste gültige Option).
     - Aufbau von Reverse-Dependency-Kanten (`pkg -> [(parent_pkg, triggering_use_flag)]`) für die Blame-Ansicht.
  2. `11_cli.rs`:
     - Subcommand `graph`: Gibt aufgelöste Runtime-Pakete, Gesamt-Binpkg-Größe, Gesamt-Download-Größe, `BDEPEND`-Größe getrennt sowie entdeckte `||`-Alternativen aus.
     - Subcommand `blame <cat/pkg>`: Gibt alle Dependency-Ketten von `world` bis `<cat/pkg>` inkl. auslösendem USE-Flag aus.
- **Validierung:** `cargo build` und `cargo clippy --all-targets -- -D warnings` grün.

### T07 — Host-Tests Modus 2a (`graph`, `blame`)
- **Schritte:**
  1. Synthetische Graphen in Unit- und `tests/host_integration.rs`-Tests:
     - **Shared-Dependency-Diamond:** `world` enthält A und B, beide brauchen C; Reference-Count von C ist 2; Gesamtsumme zählt C nur **einmal**.
     - **Zyklenfestigkeit:** Zirkuläre `RDEPEND`-Kanten (`A -> B -> A`) terminieren ohne Endlosschleife.
     - **`BDEPEND`-Trennung:** Fettes Build-Tool in `BDEPEND` erhöht nicht die Runtime-`RDEPEND`-Summe.
     - **Transitive `||`-Alternative:** `pkgA` hat `|| ( curl wget )`; Entscheidung wird erkannt und zwischen `curl` und `wget` umschaltbar aufgelöst.
     - **Blame-Kette:** `world:app -> libxml2[+icu] -> icu` liefert exakten Rückwärtspfad inklusive Flag `[+icu]`.
- **Validierung:** `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` grün.

### T08 — HIL-Nachweis Modus 2a (`graph`, `blame`)
- **Schritte:**
  1. `graph` auf dem echten `/tmp/gentoo-hil/`-Datensatz und realer `world`-Datei ausführen.
  2. Aufgelöste Runtime-Paketliste mit `/tmp/gentoo-hil/emerge_world_ref.txt` (`emerge -p --emptytree --with-bdeps=n`) vergleichen. Bekannte Unterschiede (z. B. `virtual/*`-Auswahl oder Sub-Slots) dokumentieren und echte Fehler sofort beheben.
  3. `blame` für mindestens ein schweres transitives Paket (z. B. `dev-lang/python`, `dev-libs/icu` oder `dev-libs/openssl`) ausführen und Pfad auf Plausibilität prüfen.
  4. HIL-Regressionstest in `tests/hil_regression.rs` ergänzen.
- **Validierung:**
  - `GENTOO_HIL_REPO=/tmp/gentoo-hil cargo test` und alle Gates grün.
  - Commit: `feat(graph): add RDEPEND solver, ref-counting, any-of discovery and blame view`

---

## Modus 2b: Kaskaden, Konflikte, Marginal-Deltas & Big Levers (`deltas`, `big-levers`)

### T09 — Implementierung Modus 2b (`deltas`, `big-levers`)
- **Dateien:** `07_cascade.rs`, `08_delta.rs`, `11_cli.rs` (Subcommands `deltas`, `big-levers`).
- **Schritte:**
  1. `07_cascade.rs`:
     - Auswertung von `REQUIRED_USE` (`flag? ( ... )`, `!flag? ( ... )`, `|| ( ... )`, `^^ ( ... )`, `?? ( ... )`) innerhalb eines Pakets.
     - Auswertung von USE-Dependencies in `RDEPEND` (`cat/pkg[foo]`, `cat/pkg[-foo]`, `cat/pkg[foo?]`, `cat/pkg[!foo?]`) über Paketgrenzen hinweg.
     - Fixpunkt-Propagation der erzwungenen USE-Flags; Erkennung von Widersprüchen (`+foo` vs. `-foo` oder `^^`-Verletzung) als `ConflictInfo` mit Verursacher-Kette.
     - Berechnung konkreter Quick-Switch-Auflösungs-Kandidaten für jeden Konflikt (z. B. „Option A: erzwinge `+foo` auf `libX`“ vs. „Option B: deaktiviere `-bar` auf `pkgC`“).
  2. `08_delta.rs`:
     - Marginal-Delta-Berechnung pro Paket und USE-Flag (Simulation eines Flag-Toggles inkl. Kaskaden auf dem Integer-ID-Graphen): liefert `delta_binpkg_bytes` (signed `i64`), `delta_download_bytes` (`i64`) sowie `Option<ConflictInfo>`.
     - Globale „Big Levers“-Aggregation: Gruppiert jedes USE-Flag über alle aktiven Pakete (`betroffene Pakete`, `Gesamt-Delta bei Deaktivierung`, `Anzahl entstehender Konflikte`).
  3. `11_cli.rs`:
     - `deltas <cat/pkg>`: Zeigt alle USE-Flags des Pakets mit aktuellem Status, Marginal-Delta (Binpkg + DL), Kaskaden-Hinweisen und eventuellen `CONFLICT`-Markierungen.
     - `big-levers`: Listet die größten globalen USE-Hebel sortiert nach Einsparpotenzial.
- **Validierung:** `cargo build` und `cargo clippy --all-targets -- -D warnings` grün.

### T10 — Host-Tests Modus 2b (`deltas`, `big-levers`)
- **Schritte:**
  1. Tests in `07_cascade.rs`, `08_delta.rs` und `tests/host_integration.rs`:
     - **Shared-Dep-Delta:** Wenn A und B beide `openssl` über `USE=ssl` ziehen, liefert Abschalten von `ssl` nur bei A exakt `0 B` Binpkg-Delta; Abschalten bei beiden liefert `-size(openssl)`.
     - **Kaskade (`REQUIRED_USE`):** Einschalten von `wayland` erzwingt `egl`; bei `^^ ( qt6 gtk )` schaltet Einschalten von `qt6` `gtk` ab (gemischtes Delta).
     - **Inter-Paket-Kaskade & Konflikt:** Paket A verlangt `libX[foo]`, Paket C verlangt `libX[-foo]` → `ConflictInfo` wird gemeldet und liefert mindestens zwei valide Auflösungsoptionen.
     - **Big Levers:** Globales Abschalten eines Flags wie `nls` oder `python` aggregiert korrekte Paketanzahl, Gesamt-Delta und Konfliktanzahl.
- **Validierung:** `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` grün.

### T11 — HIL-Nachweis Modus 2b (`deltas`, `big-levers`)
- **Schritte:**
  1. `deltas` für mindestens 3 reale Pakete der Host-`world` und `big-levers` auf dem gesamten `/tmp/gentoo-hil/`-Graphen ausführen.
  2. Ausführungszeit der Neuberechnung aller Deltas messen und dokumentieren (Performance-Ziel prüfen: In-Memory-Graph-Evaluation im ms-Bereich).
  3. Konkrete Zahlen (Top-5 Big Levers + reale Konfliktbeispiele) protokollieren und in `tests/hil_regression.rs` per Laufzeit-/Sanity-Check absichern.
- **Validierung:**
  - `GENTOO_HIL_REPO=/tmp/gentoo-hil cargo test` und alle Gates grün.
  - Commit: `feat(delta): implement REQUIRED_USE cascades, conflict detection and marginal deltas`

---

## Modus 3: TOML-Plan, Portage-Export & `emerge -p`-Verifikation (`init`, `export`, `verify`)

### T12 — Implementierung Modus 3 (`init`, `export`, `verify`)
- **Dateien:** `09_plan_toml.rs`, `10_export_verify.rs`, `11_cli.rs` (Subcommands `init`, `export`, `verify`).
- **Schritte:**
  1. `09_plan_toml.rs`:
     - Serde-Schema für `plan.toml` (`[baseline]`, `[[package]]` mit expliziten USE-Overrides, `[[choice]]` mit `[[choice.option]]` inkl. `atom`, `pros`, `cons`, `suggested_use`, `[[or_decision]]` für persistierte `||`-Entscheidungen).
     - `init`: Konvertiert eine klassische Gentoo-`world`-Datei in eine saubere `plan.toml`.
     - Laden & Speichern von `plan.toml` ohne Verlust von `pros`/`cons`-Feldern.
  2. `10_export_verify.rs`:
     - `export`: Schreibt aus dem aktuellen Zustand (inkl. gewählter `[[choice]]`-Optionen und `||`-Entscheidungen) gültige Portage-Dateien (`world`, `package.use` und bei Bedarf `package.accept_keywords`) in ein Zielverzeichnis.
     - `verify`: Führt `emerge --pretend --emptytree --verbose` (lokal oder über konfigurierbaren SSH-/Command-Runner gegen den Gentoo-Host mit temporärer `--root` / `PORTAGE_CONFIGROOT`) aus und parst Erfolg / Slot- oder Blocker-Fehler.
  3. `11_cli.rs`: Subcommands `init`, `export`, `verify`.
- **Validierung:** `cargo build` und `cargo clippy --all-targets -- -D warnings` grün.

### T13 — Host-Tests Modus 3 (`init`, `export`, `verify`)
- **Schritte:**
  1. Roundtrip-Test (`world` → `init` → `plan.toml` mit `[[choice]]` + `pros`/`cons` anreichern → laden → Flag ändern → speichern → `pros`/`cons` bleiben identisch erhalten).
  2. Export-Test: Generierte `world` und `package.use` entsprechen exakt Portage-Syntax (`cat/pkg flag1 -flag2`).
  3. Mock-Tests für den `verify`-Output-Parser (sauberer `emerge -p`-Lauf vs. `REQUIRED_USE`- oder `slot conflict`-Fehlermeldung).
- **Validierung:** `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` grün.

### T14 — HIL-Nachweis Modus 3 (`init`, `export`, `verify`)
- **Schritte:**
  1. Echte Host-`world` via `init` in eine temporäre `plan.toml` umwandeln.
  2. Ein USE-Flag über CLI/TOML anpassen, mit `export` eine temporäre `PORTAGE_CONFIGROOT` erzeugen und `verify` per `ssh kiel@localhost -i ~/.ssh/from_android` (mit `PORTAGE_CONFIGROOT` auf `/tmp/...` oder `emerge --pretend` ohne Root-Schreibrechte) gegen das echte `emerge` prüfen.
  3. Ergebnis dokumentieren; falls `emerge -p` Abweichungen/Konflikte meldet, Ursache analysieren und beheben.
- **Validierung:**
  - `GENTOO_HIL_REPO=/tmp/gentoo-hil cargo test` und alle Gates grün.
  - Commit: `feat(plan): add plan.toml state, portage export and emerge-p verification`

---

## Phase B: TUI-Tasks (erst nach Abschluss von T01–T14!)

### T15 — Implementierung TUI-State & Aktionen (`12_tui_state.rs`)
- **Schritte:**
  1. `AppState` für zwei Haupttabs (`[1] Pakete & Alternativen`, `[2] Global Big Levers`), Fokus-Wechsel zwischen den drei Panes (`Left: Pakete/Choices/||-Entscheidungen`, `Middle: USE-Flags`, `Right: Blame/Pros-Cons`), Such-/Filtermodus (`/`).
  2. Aktionen: `Space` (USE-Flag toggeln inkl. Live-Neuberechnung via `06_graph` + `07_cascade` + `08_delta`), `Enter` auf `||`-Alternative oder `[[choice]]` (Option umschalten), `Enter` auf `CONFLICT`-Flag (Konflikt-Quick-Switch-Popup mit validen Auflösungen aus `07_cascade`), `S` (Speichern in `plan.toml`), `E` (Export `world` + `package.use`), `V` (`verify` triggern und Status anzeigen).
  3. Unit-Tests in `12_tui_state.rs` für alle State-Übergänge (Tab-Wechsel, Toggle aktualisiert Marginal-Deltas, Konflikt-Quick-Switch löst Konflikt auf, Dirty-Flag nach `Save` zurückgesetzt).
- **Validierung:** `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` grün.

### T16 — Implementierung TUI-Widgets & 3-Pane-Layout (`13_tui_view.rs`)
- **Schritte:**
  1. Tab `[1]` 3-Pane-Layout mit `ratatui`:
     - **Links:** Gruppierte Liste (`world`/`[[choice]]`-Einträge, entdeckte transitive `||`-Entscheidungen, transitive Pakete sortiert nach Binpkg-Größe), Konflikt-Indikatoren.
     - **Mitte:** Details zum gewählten Paket (Binpkg-`SIZE`, Subgraph-Größe, `BDEPEND`-Hinweis) + scrollbarer Liste aller USE-Flags mit farbigem Marginal-Delta (`-4.2 MB bin / -9.8 MB dl`, `+0 B`, rotes `CONFLICT`).
     - **Rechts:** Blame-View („Warum ist das hier?“ — alle Dependency-Ketten bis `world`) sowie bei `[[choice]]` klar formatierte `Pros` / `Cons`.
  2. Tab `[2]` Global „Big Levers“: Tabelle mit `Flag | Betroffene Pakete | Gesamt-Delta | Konflikte` + Schnell-Toggle-Möglichkeit.
  3. Konflikt-Quick-Switch-Modal: Zeigt Verursacher und wählbare Auflösungsoptionen inkl. deren Delta.
  4. Footer: Gesamt-Binpkg-Größe, Gesamt-Download-Größe, `BDEPEND`-Größe, Paketanzahl, Konflikt-Counter, Tastenbelegung (`[Space] Toggle`, `[Tab] Pane`, `[1/2] View`, `[S] Save`, `[E] Export`, `[V] Verify`, `[q] Quit`).
- **Validierung:** `cargo build` und `cargo clippy --all-targets -- -D warnings` grün.

### T17 — TUI-Rendering-Tests mit `ratatui::TestBackend`
- **Schritte:**
  1. Automatisierte UI-Tests in `13_tui_view.rs` / `tests/host_integration.rs` mit `ratatui::backend::TestBackend`:
     - 3-Pane-Rendering auf normalem Terminal (`120x40`) und kleinem Terminal (`80x24`) ohne Panic oder abgeschnittene Statuszeile.
     - Sichtbarkeitsprüfung im gerenderten Buffer: Paketnamen, Marginal-Deltas, `CONFLICT`-Markierung, Blame-Kette (`->`), `Pros`/`Cons` und Big-Levers-Tabelle.
     - Rendering des geöffneten Konflikt-Quick-Switch-Modals.
- **Validierung:** `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` grün.

### T18 — TUI-Runner & Interaktiver Smoke-Test (`14_tui_runner.rs`)
- **Schritte:**
  1. `14_tui_runner.rs` mit `ratatui::run` (bzw. `init`/`restore` inkl. Panic-Hook zur sicheren Terminal-Wiederherstellung) und `crossterm` Event-Loop implementieren.
  2. `main.rs` so verdrahten, dass Aufruf ohne Subcommand (bzw. `tui --plan plan.toml`) die TUI startet.
  3. Smoke-Test auf dem echten `/tmp/gentoo-hil/`-Datensatz und einer aus Host-`world` erzeugten `plan.toml` (via PTY/Skript oder manueller/automatisierter Key-Sequenz): Start, Navigation, Flag-Toggle, Blame-Prüfung, Save, sauberes Beenden ohne Terminal-Korruption.
- **Validierung:**
  - `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` grün.
  - Commit: `feat(tui): implement 3-pane ratatui interface with blame view and conflict quick-switch`

---

## Phase C: Finale Regression & Abschluss-Dokumentation

### T19 — Vollständige End-to-End-Regression & Gate-Prüfung
- **Schritte:**
  1. Alle Gates hintereinander auf dem finalen Stand ausführen:
     - `cargo fmt --check`
     - `cargo clippy --all-targets -- -D warnings`
     - `cargo test` (ohne HIL-Dataset)
     - `GENTOO_HIL_REPO=/tmp/gentoo-hil cargo test` (mit echtem Gentoo-Dataset)
  2. Prüfen, ob alle Quelldateien der `01_...rs`-Nummerierung entsprechen, keine Datei ~600 Zeilen sprengt und `lib.rs`/`main.rs` frei von Logik sind.
- **Validierung:** Alle Checks bestanden, 0 Warnungen, 0 fehlgeschlagene Tests.

### T20 — `plan_effort.md` und `walkthrough.md` erstellen
- **Schritte:**
  1. `./plan/20261113_01_gentoo/plan_effort.md` ausfüllen (Input/Output/Cached/Total Tokens, Turns, Subagents).
  2. `./plan/20261113_01_gentoo/walkthrough.md` strikt nach Vorgabe aus `prompt.txt` verfassen:
     - Zwingend auf **Deutsch**, didaktisch und flüssig lesbar.
     - Alle Abkürzungen (PMS, EAPI, CPV, RDEPEND, BDEPEND, VDB, HIL, TUI, etc.) bei erster Verwendung einführen und in einem **Glossar** erklären.
     - **Mermaid-Diagramme** für Architektur (`RepoIndex` → `Graph` → `Cascade`/`Delta` → `TUI`), Datenfluss und Kaskaden-Auflösung.
     - Konkrete Zahlenbeispiele aus den Host-Tests und dem HIL-Nachweis.
     - Pflichtkapitel: (1) Was exakt implementiert wurde, (2) Welche Architektur-Entscheidungen aufgrund von Tests/HIL angepasst werden mussten, (3) Learnings und mögliche Erweiterungen (z. B. empirische SQLite-DB aus `/var/db/pkg/*/*/SIZE` und `/var/log/emerge.log`, SAT-Solver), (4) Liste neuer Pakete für das Dockerfile.
- **Validierung:** Finale Prüfung auf Vollständigkeit; Commit: `docs(plan): add walkthrough and session effort report`
