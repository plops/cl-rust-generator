Nachfolgend findest du die fertigen, direkt einsetzbaren Artefakte für einen neuen Arbeitsauftrag (z. B. unter `plan/20261114_01_fixes/`):

1. **Konkrete Anweisungen für den Agenten** (Start-Prompt / `prompt.txt`)
2. **`plan.md`** (Implementierungsplan inkl. Architektur-Kontext, Instrumentierungs-Konzept und Commit-Plan)
3. **`tasks.md`** (strikt serielle Aufgabenliste: *Erst Baseline-Systemtest mit Instrumentierung → Performance-Optimierung mit Messvergleich → TUI-Scrolling & UX → Delta-Paketliste & `[[choice]]` → Portage-Fixes → verständliche Doku*)

---

# 1. Konkrete Anweisungen für den Agenten (`prompt_fixes.txt`)

```markdown
# Arbeitsanweisung: Behebung der Code-Review-Findings & Performance-Optimierung (`gentoo-dep-tui`)

Du übernimmst die Weiterentwicklung des Rust-Crates `gentoo-dep-tui` auf Basis des bestehenden Standes (Branch `feature/gentoo-dep-tui` bzw. abgeleitet von `master`).

## Verbindliche Regeln
1. **Arbeitsweise:** Arbeite strikt seriell gemäß `tasks.md`. Überspringe keinen Mess- oder Testschritt.
2. **Instrumentierung vor Optimierung:** Bevor du eine Performance-Änderung schreibst, implementiere und starte zuerst den instrumentierten Systemtest (`tests/perf_system.rs`), um exakte Baseline-Zahlen (Laufzeit, CPU-Auslastung/Speedup, Anzahl Graph-Auflösungen) festzuhalten. Nach jeder Optimierung misst du erneut und vergleichst gegen die Baseline.
3. **Rust-Standards & Datei-Regel:**
   - Rust Edition `2024`.
   - Jede Quelldatei unter `src/` trägt eine zweistellige Nummer in Datenfluss-Reihenfolge (`01_...rs` bis `15_...rs`), hat **genau eine Zuständigkeit** und bleibt **unter ~600 Zeilen**. Überschreitet ein Modul durch Erweiterungen ~550–600 Zeilen (Achtung bei `06_graph.rs`, `12_tui_state.rs`, `13_tui_view.rs`), teile es sauber in zwei nummerierte Dateien auf, ohne Verhalten zu verändern.
   - `lib.rs` und `main.rs` enthalten ausschließlich Modul-Deklarationen und oberste Verdrahtung.
4. **Qualitäts-Gates nach jedem Task:**
   - `cargo fmt --check`
   - `cargo clippy --all-targets -- -D warnings`
   - `cargo test`
   - `cargo test --release --test perf_system -- --nocapture` (System- & Performance-Tests)
   - Wenn `GENTOO_HIL_REPO=/tmp/gentoo-hil` gesetzt ist: `GENTOO_HIL_REPO=/tmp/gentoo-hil cargo test --release`
5. **Commits:** Conventional Commits (`<type>(<scope>): <kurz>`) mit strukturiertem Body (Was, Warum, gemessene Zahlen/Tests).
6. **Stopp-Regel:** Lässt sich ein technisches Problem (z. B. fehlende `Sync`-Eigenschaft einer Upstream-Crate oder ein unauflösbarer Fehler) in maximal **zwei gezielten Versuchen** nicht lösen, sofort anhalten und den exakten Befund dokumentieren.
7. **Dokumentation:** Aktualisiere `walkthrough.md` (und erstelle eine verständliche `README.md` im Crate `gentoo-dep-tui/`), sodass ein Leser ohne Vorwissen sofort versteht, wo Dateien gesucht werden, wie die TUI bedient wird und wie die konkreten Anwendungsfälle (`ffmpeg` JPEG→AV1, `openssh`-Alternativen, `nginx`-Alternativen) funktionieren.
8. **Aufwand:** Halte den Token-Verbrauch dieser Session in `plan_effort_fixes.md` fest.
```

---

# 2. Implementierungsplan (`plan.md`)

```markdown
# Implementierungsplan: Review-Findings, Multi-Core-Performance & TUI-Usability (`gentoo-dep-tui`)

## 1. Ausgangslage & Ziele

Das erste Review und reale Nutzungstests haben vier Problemfelder offengelegt:
1. **Performance (100 % CPU auf 1 Core):** `big_levers` und `package_deltas` in `08_delta.rs` rechnen seriell auf einem Kern; `flag_delta` berechnet die Baseline (`before`) pro Flag redundant neu; `resolve()` ruft bei jeder Simulation unnötigerweise `resolve_build_extra()` auf (obwohl `ResolvedGraph::totals()` `BDEPEND` gar nicht mitzählt); die TUI ruft bei jeder Cursor-Bewegung `package_deltas()`, bei jedem `Space` komplettes `big_levers()` und bei jedem Tippen im Filter `/` komplettes `refresh()` synchron im UI-Thread auf.
2. **TUI-Scrolling & Bedienung:** Keine Liste/Tabelle/Textansicht in `13_tui_view.rs` nutzt `ListState`, `TableState` oder `Paragraph::scroll`. Dazu fehlen das sichtbare Fokus-Tracking in der linken Spalte, das Umschalten von Big Levers auf Tab 2, ein interaktiver Switcher für `[[choice]]`-Alternativen aus `plan.toml` und die automatische Synchronisation vor dem Export (`E`).
3. **Anwendungsfall-Transparenz & Portage-Korrektheit:** `FlagDelta` weist nicht aus, *wie viele* und *welche* neuen Pakete ein Flag zusätzlich zieht (z. B. bei `ffmpeg` + `svt-av1`). `load_world` verschluckt fehlende Dateien still. `make.profile`-Symlink, `USE=` in `make.defaults`, `-flag`-Negation in `use.mask`/`use.force` sowie versionierte `package.use`-Einträge werden noch fehlerhaft oder gar nicht behandelt.
4. **Verständlichkeit der Doku:** `walkthrough.md` ist als technisches Protokoll geschrieben und erklärt Einstieg, Pfadsuche und konkrete Anwendungsfälle nicht ausreichend.

---

## 2. Kontextaufbau: Relevante Dateien für den Agenten

Der Agent muss zu Beginn folgende Dateien lesen:
- `gentoo-dep-tui/Cargo.toml` — Abhängigkeiten (`rayon`, `ratatui` 0.30, `portage-atom` 0.12, `portage-metadata` 0.11).
- `gentoo-dep-tui/src/01_types.rs` — Kerntypen (`SizeMetrics`, `FlagDelta`, `PkgId`, `FlagId`, `IdTable`).
- `gentoo-dep-tui/src/02_profile.rs` — Profilstapel, `read_world`, `read_flag_list`, `read_package_use`, `apply_use_defaults`.
- `gentoo-dep-tui/src/05_repo_index.rs` — `RepoIndex::load`, `Manifest`-Laden pro Version, `binhost_hit_rate`.
- `gentoo-dep-tui/src/06_graph.rs` — `Engine`, `ResolvedGraph`, `resolve`, `resolve_build_extra`, `totals()`.
- `gentoo-dep-tui/src/07_cascade.rs` — `resolve_with_cascade`, `CascadeStep`, `enforce_required_use`, `push_use_conflict`.
- `gentoo-dep-tui/src/08_delta.rs` — `flag_delta`, `big_levers`, `package_deltas` (Haupt-Engstelle!).
- `gentoo-dep-tui/src/09_plan_toml.rs` & `src/10_export_verify.rs` — `Plan`, `PlanChoice`, `export_portage`, `VerifyRunner`.
- `gentoo-dep-tui/src/11_cli.rs` — CLI-Defaults, `load_profile`, `load_world`, `export_plan`.
- `gentoo-dep-tui/src/12_tui_state.rs`, `src/13_tui_view.rs`, `src/14_tui_runner.rs` — TUI-State, Ratatui-Widgets, Event-Loop.
- `gentoo-dep-tui/tests/host_integration.rs` & `tests/hil_regression.rs` — bestehende Testsuiten.

---

## 3. Technische Lösungsarchitektur

### A. Instrumentierung & Systemtests (`tests/perf_system.rs` + `06_graph.rs`/`08_delta.rs`)
Um Optimierungen objektiv zu belegen, wird eine dedizierte, leichtgewichtige Instrumentierung eingeführt:
- **Zähler & Timer (`PerfMetrics`):** Erfasst pro Lauf/Operation:
  - `resolve_calls`, `resolve_build_extra_calls`, `cascade_rounds`
  - Wand-Uhr-Dauer (`wall_ms`) für `RepoIndex::load`, `package_deltas`, `big_levers`, `AppState::refresh`, `move_selection`, `filter_typing`
  - Effektiver Parallelitätsgrad / Speedup (`1 Thread` vs. `rayon` Global Pool).
- **Synthetischer Großgraph in `tests/perf_system.rs`:** Generiert reproduzierbar ein realistisches `tempfile`-Repo mit z. B. **400 Paketen**, Diamond-Abhängigkeiten, gemeinsamen Bibliotheken, `REQUIRED_USE`-Ketten und einem Flag-reichen Paket (`media-video/ffmpeg`-Simulator mit 60 USE-Flags).
- **End-to-End-TUI-Systemtest:** Simuliert in `perf_system.rs` komplette UI-Sequenzen über `AppState` (10× Cursor hoch/runter, 5× Tippen im Filter `/`, 3× Flag-Toggle `Space`, Wechsel auf Tab 2 `BigLevers`) und prüft harte Latenz-Budgets.

### B. Engine- & Multi-Core-Performance
1. **`Manifest`-Caching (`05_repo_index.rs`):** `Manifest` pro `(category, package)` nur einmal lesen (vor oder gruppiert nach `cpn`), statt für jede einzelne Ebuild-Version erneut von Platte zu parsen.
2. **Kein unnötiges `BDEPEND` bei Simulationen (`06_graph.rs`):**
   - Neue Funktion `resolve_with_options(engine, world, include_build: bool)` (bzw. `resolve_runtime_only`).
   - `resolve()` (für CLI `graph`) behält `include_build = true`.
   - `resolve_with_cascade` (soweit für `08_delta.rs` genutzt) sowie alle Simulationen in `08_delta.rs` laufen mit `include_build = false`, weil `ResolvedGraph::totals()` ausschließlich `graph.nodes` (Runtime) summiert.
3. **Eliminierung doppelter Baselines (`08_delta.rs`):**
   - Interne Funktion `flag_delta_with_base(engine, world, package, flag, base_graph, base_totals)`.
   - `package_deltas` berechnet `base_graph` genau **einmal** vor der Schleife.
   - `big_levers` berechnet `base_outcome` ebenfalls per `resolve_with_cascade` (konsistent mit den Flag-Läufen!).
4. **Multi-Core-Parallelisierung via `rayon` (`08_delta.rs`):**
   - Falls `portage-metadata::CacheEntry` oder `portage_atom::Dep` nicht `Sync` sind, legt `RepoIndex` (oder ein schlankes neues Modul `05b_compiled_repo.rs` / `06_graph.rs`) vorab `Send + Sync`-Datenstrukturen (oder gleich `PkgId`/`FlagId`-Arrays) an.
   - Sowohl `package_deltas` (`candidates.par_iter()`) als auch `big_levers` (`flags.par_iter()`) skalieren auf alle verfügbaren CPU-Kerne.
5. **Kompiler-Vorbereitung / Integer-IDs (`01_types.rs` / `06_graph.rs`):**
   - Häufig genutzte Flag-Lookups (`effective_use`) und Graph-Traversierung optimieren (vermeiden von wiederholtem `Dep::parse` in `resolve` und `world_atom_cpn` bei jedem Lauf).

### C. Asynchrone / Entkoppelte TUI-Updates & Scrolling
1. **Entkopplung der UI-Reaktionszeit (`12_tui_state.rs`, `14_tui_runner.rs`, ggf. `15_tui_worker.rs`):**
   - **Filter (`/`):** Filtert *nur* `self.left` in-memory (`rebuild_left_only()`), ohne `resolve_with_cascade` oder `big_levers` anzustoßen (< 1 ms).
   - **Cursor links (`j`/`k`):** `package_deltas` wird **nicht** mehr beim schnellen Durchscrollen blockierend für jedes Zwischen-Paket gerechnet – entweder Cache pro `(override_hash, cpn)` + asynchroner Hintergrund-Worker (`std::sync::mpsc` + `rayon` mit Debounce/Generation-ID) oder sofortiges Anzeigen gecachter Flags und asynchrones Nachliefern der Byte-Deltas.
   - **Big Levers (Tab 2):** Wird **lazy** beim ersten Wechsel auf Tab 2 (bzw. bei Bedarf im Hintergrund-Worker mit Lade-Indikator in der Statuszeile) neu berechnet, **nicht** bei jedem `Space` in Tab 1.
2. **Echtes Scrolling (`13_tui_view.rs`):**
   - `render_left`: `StatefulWidget` mit `ListState` (ausgewähltes Paket bleibt auch bei Fokuswechsel sichtbar markiert).
   - `render_middle`: Berechnet `scroll_offset` passend zur `middle_selected`-Zeile und setzt `Paragraph::scroll((offset, 0))`.
   - `render_right`: Scrollbar über `scroll_right: u16` (`Pane::Right` unterstützt `j`/`k`/`PageUp`/`PageDown`).
   - `render_levers`: `StatefulWidget` mit `TableState`.
3. **Fehlende TUI-Funktionen:**
   - **Tab 2 (`Big Levers`):** `Space` auf einem Flag in Tab 2 deaktiviert/aktiviert das Flag global über die betroffenen Pakete (`overrides`).
   - **`[[choice]]`-Switcher:** `[[choice]]`-Einträge aus `plan.toml` erscheinen als eigene Gruppe in der linken Spalte; `Enter` (oder Auswahl in der Mitte) schaltet zwischen den `[[choice.option]]`-Alternativen um und aktualisiert `world` + `Pros`/`Cons` + Graph live (ohne `plan.baseline.world` zu verfälschen).
   - **Export (`E`):** Ruft vor `export_plan` immer `sync_plan_from_state` auf.
   - **Default-Command:** Aufruf ohne Subcommand in `11_cli.rs` startet automatisch `tui`.

### D. Delta-Paketliste & Portage-Fixes
1. **`FlagDelta` mit Paket-Diff (`01_types.rs`, `08_delta.rs`, `11_cli.rs`, `13_tui_view.rs`):**
   - `FlagDelta` erhält `pub added_packages: Vec<String>` und `pub removed_packages: Vec<String>`.
   - `deltas`-CLI und die TUI-Mittel-/Rechtsspalte zeigen `(+N Pakete: pkg1, pkg2, ...)` bzw. `(-N Pakete)` klar an.
   - Fehlt `--binhost`, weist die UI explizit `bin: n/a` statt irreführend `0 B` aus.
2. **Portage-Semantik (`02_profile.rs`, `06_graph.rs`, `07_cascade.rs`, `10_export_verify.rs`, `11_cli.rs`):**
   - `load_world` meldet einen klaren Fehler, wenn `--world` nicht existiert (kein stilles `unwrap_or_default`).
   - `load_profile` liest `/etc/portage/make.profile` (Symlink-Auflösung relativ zu `profiles/`), falls `--profile` nicht gesetzt ist.
   - `load_profile_stack` liest auch `USE="..."` aus `make.defaults` und löst `${USE}` in `make.conf` auf.
   - `read_flag_list` verarbeitet `-flag` korrekt als *Entfernen* aus `use.mask`/`use.force`.
   - `package.use` matcht via `Dep::parse` auch versionierte/slotted Atome (`>=cat/pkg-1.0`, `cat/pkg:0`).
   - `resolve` berücksichtigt einschränkende `Dep`-Atome aus `RDEPEND` in der Queue (statt sie durch `index.select(cpn)` zu verwerfen).
   - `QuickSwitch` befüllt echte `changes` auch bei `REQUIRED_USE !flag`.
   - `export_plan` und `export_portage` werden vereinheitlicht; `VerifyRunner` korrigiert `PORTAGE_CONFIGROOT` vs. `@world`-Pfad.

---

## 4. Commit-Plan (Conventional Commits)

1. `test(perf): add instrumented system and benchmark suite for graph, delta and tui`
2. `perf(engine): parallelize deltas/big-levers with rayon, skip build_extra in sims and cache manifests`
3. `perf(tui): decouple filter/scroll from cascade refresh and compute deltas asynchronously`
4. `fix(tui): add stateful scrolling, choice switcher, big-lever toggle and safe export`
5. `feat(delta): track added and removed packages in FlagDelta and display binhost-na hint`
6. `fix(portage): fix world error handling, make.profile symlink, make.defaults USE and use.mask negation`
7. `docs(guide): rewrite walkthrough and add practical user guide with ffmpeg/openssh/nginx workflows`
```

---

# 3. Detaillierter Aufgabenplan (`tasks.md`)

```markdown
# Aufgabenplan zur Behebung der Findings (`tasks.md`)

Alle Tasks sind **strikt seriell** abzuarbeiten. Nach jedem Task laufen die Gates (`cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test`). Die gemessenen Instrumentierungswerte werden fortlaufend notiert.

---

## Phase 1: Baseline-Instrumentierung & Systemtests (vor den Optimierungen!)

### T01 — Instrumentierungs-Metriken in Engine einbauen
- **Dateien:** `src/01_types.rs`, `src/06_graph.rs`, `src/07_cascade.rs`, `src/08_delta.rs`
- **Schritte:**
  1. In `01_types.rs` eine schlanke Struktur `PerfStats` (`resolve_calls`, `build_extra_calls`, `cascade_rounds`, `delta_runs`) sowie einen optionalen Thread-lokalen oder expliziten Zähler einführen (zero-overhead bzw. `AtomicU64`-Zähler hinter Hilfsfunktionen in `06_graph.rs`/`07_cascade.rs`).
  2. Zähler in `resolve()`, `resolve_build_extra()` und `resolve_with_cascade()` hochzählen.
  3. Eine reset-/snapshot-Funktion `perf_snapshot_and_reset()` bereitstellen, damit Tests exakt zählen können, wie viele Aufrufe pro Nutzeraktion ausgelöst werden.
- **Validierung:** `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` grün.

### T02 — Instrumentierten Systemtest (`tests/perf_system.rs`) schreiben & Baseline messen
- **Dateien:** `tests/perf_system.rs`
- **Schritte:**
  1. Einen deterministischen Generator für ein großes synthetisches Repo bauen: **300 Pakete**, tiefe Diamond-Ketten, gemeinsame Bibliotheken (`openssl`, `zlib`, `zstd`), `BDEPEND`-Ketten (Compiler/Tools) sowie ein Flag-reiches Paket `media-video/ffmpeg` mit **60 USE-Flags** (inkl. `svt-av1`, `aom`, `dav1d`, `REQUIRED_USE`).
  2. Folgende **Systemtests mit Zeit- und Zähler-Ausgabe** implementieren:
     - `sys_perf_index_load`: Dauer von `RepoIndex::load`.
     - `sys_perf_package_deltas`: Dauer + Anzahl `resolve_calls` + `build_extra_calls` für `package_deltas(..., "media-video/ffmpeg")`.
     - `sys_perf_big_levers`: Dauer + `resolve_calls` für `big_levers(...)` über alle 300 Pakete.
     - `sys_perf_tui_workflow`: End-to-End-Simulation über `AppState` (Start → 10× `move_selection(1)` → Filter `/` mit 5 Zeichen → 3× `toggle_selected_flag()` → Tab-Wechsel zu `BigLevers`) inkl. getrennter Zeitmessung pro Schritt.
  3. Optionalen Test `sys_perf_hil_snapshot` hinzufügen (aktiv wenn `GENTOO_HIL_REPO` gesetzt ist, sonst sauber übersprungen).
  4. `cargo test --release --test perf_system -- --nocapture` ausführen und alle **Baseline-Werte** (Zeiten in ms, `resolve_calls`, `build_extra_calls`) als Referenzwerte im Test-Kommentar oder einer Tabelle festhalten.
- **Validierung:**
  - Baseline-Messung liegt vor (erwartet: `big_levers` & `tui_workflow` noch langsam, >100 `resolve_calls` bei `package_deltas`).
  - Commit: `test(perf): add instrumented system and benchmark suite for graph, delta and tui`

---

## Phase 2: Engine- & Multi-Core-Performance-Optimierung

### T03 — `Manifest`-Caching & `resolve_runtime_only` (kein `BDEPEND` bei Simulationen)
- **Dateien:** `src/05_repo_index.rs`, `src/06_graph.rs`, `src/07_cascade.rs`, `src/08_delta.rs`
- **Schritte:**
  1. In `05_repo_index.rs`: `Manifest` einmal pro `(category, package)` laden/cachen, bevor oder während `md5-cache`-Dateien parallel geparst werden (z. B. zuerst einzigartige Paketverzeichnisse sammeln und deren `Manifest` parallel in eine `HashMap<String, Manifest>` laden, dann an die Versionen übergeben). Toten Code in `binhost_hit_rate` entfernen.
  2. In `06_graph.rs`: `resolve_with_options(engine, world, include_build: bool)` einführen (`resolve()` ruft mit `true`). `resolve_build_extra` wird nur bei `include_build == true` ausgeführt.
  3. In `07_cascade.rs`: `resolve_with_cascade_opts(..., include_build: bool)` bzw. sicherstellen, dass innere Fixpunkt-Runden `include_build = false` nutzen und nur der allerletzte finale Graph (bei `include_build == true`) Build-Extras ermittelt. In `08_delta.rs` immer `include_build = false` übergeben.
  4. **Gegenmessen:** `cargo test --release --test perf_system -- --nocapture` ausführen und `build_extra_calls == 0` in Delta-Läufen sowie neuen Speedup dokumentieren.
- **Validierung:** Alle Unit-/Integrations-Tests grün; messbare Beschleunigung gegenüber T02-Baseline.

### T04 — Doppelte Baseline entfernen & `rayon`-Parallelisierung in `08_delta.rs`
- **Dateien:** `src/05_repo_index.rs`, `src/06_graph.rs`, `src/08_delta.rs` (ggf. `05b_compiled.rs`, falls `05`/`06` >550 Zeilen wachsen)
- **Schritte:**
  1. Prüfen, ob `Engine<'_>` (`RepoIndex`, `Profile`, `CacheEntry`, `Dep`) `Sync` implementiert (Compile-Check mit `fn assert_sync<T: Sync>() {}`). Falls eine Upstream-Struktur (`CacheEntry` / `Dep`) nicht `Sync` ist: kompakte eigene `Sync`-Repräsentation beim Laden von `RepoIndex` erzeugen.
  2. In `08_delta.rs`:
     - `flag_delta_with_base(...)` einführen, sodass `before_graph` und `before_totals` **nicht** innerhalb von `flag_delta` neu berechnet werden.
     - `package_deltas` berechnet `base` genau **einmal** und nutzt `candidates.into_par_iter().map(...)` (`rayon`).
     - `big_levers` berechnet `base` mit `resolve_with_cascade` (konsistent!) und nutzt `flags.into_par_iter().map(...)` (`rayon`).
  3. **Instrumentierter Systemtest-Vergleich:** `perf_system.rs` um einen Test erweitern, der die Laufzeit von `big_levers` mit `rayon::ThreadPoolBuilder::new().num_threads(1)` gegen den Standard-Pool (alle Kerne) direkt vergleicht und prüft, dass die Ergebnisse exakt identisch sind (Sortierung & Werte deterministisch!).
- **Validierung:**
  - `cargo test --release --test perf_system -- --nocapture` zeigt deutlichen Multi-Core-Speedup und Halbierung von `resolve_calls` bei `package_deltas`.
  - `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` grün.
  - Commit: `perf(engine): parallelize deltas/big-levers with rayon, skip build_extra in sims and cache manifests`

### T05 — TUI-Thread entkoppeln (Filter, Cursor-Navigation, Lazy Big-Levers, Background-Worker)
- **Dateien:** `src/12_tui_state.rs`, `src/14_tui_runner.rs` (bei Bedarf `src/15_tui_worker.rs` anlegen, um <= 600 Zeilen einzuhalten)
- **Schritte:**
  1. **Filter (`/`):** In `12_tui_state.rs` Methode `apply_filter_only()` einführen (ruft nur `rebuild_left()` auf, behält selektiertes Paket bei falls noch sichtbar). `14_tui_runner.rs` ruft beim Tippen nur `apply_filter_only()` statt `refresh()` auf.
  2. **Lazy `BigLevers`:** `AppState::refresh()` berechnet `big_levers` **nicht** mehr nach jedem `Space` in Tab 1! Stattdessen ein Flag `levers_dirty: bool` führen; erst wenn Tab 2 (`Tab::BigLevers`) aktiv ist (oder der Nutzer dorthin wechselt), werden `levers` neu berechnet (oder im Worker aktualisiert).
  3. **Asynchrone/gecachte `package_deltas` für flüssige Navigation:**
     - Entweder Worker-Thread (`mpsc`-Channel mit `generation: u64`) oder non-blocking Poll im `event_loop` (`14_tui_runner.rs`): Beim Drücken von `j`/`k` in der linken Spalte erscheinen die Flag-Namen + `[x]`/`[ ]`-Status *sofort* (0 ms), während ein Hintergrund-Job `package_deltas` (parallel via `rayon`) berechnet und bei Fertigstellung das Event-Loop-Rendering aktualisiert (veraltete `generation` wird verworfen).
     - `AppState` bekommt `deltas_loading: bool` für einen dezenten Hinweis in der Mittelspalte (`(rechne Deltas...)`).
  4. **Systemtest in `tests/perf_system.rs`:** Prüfen, dass 10× `move_selection` + 5× Filter-Tippen in der TUI jeweils unter **5 ms** Gesamtzeit bleiben (inkl. `AppState`-Assertions).
- **Validierung:**
  - Alle Gates + `perf_system` grün (UI blockiert nicht mehr bei Scroll/Filter/Toggle).
  - Commit: `perf(tui): decouple filter/scroll from cascade refresh and compute deltas asynchronously`

---

## Phase 3: TUI-Scrolling & Bedienungs-Verbesserungen

### T06 — Echtes Stateful-Scrolling in allen Panes & Tabellen
- **Dateien:** `src/12_tui_state.rs`, `src/13_tui_view.rs`, `tests/host_integration.rs`
- **Schritte:**
  1. `AppState`: `left_list_state: ListState` (oder Index + Scroll-Offset), `lever_table_state: TableState`, `middle_scroll: u16`, `right_scroll: u16` (inkl. `move_selection` Unterstützung für `Pane::Right` zum Scrollen langer Blame-Ketten) ergänzen.
  2. `13_tui_view.rs`:
     - `render_left`: Auf `frame.render_stateful_widget(list, area, &mut state_copy)` umstellen. Gewähltes Paket links bleibt auch markiert, wenn `Pane::Middle` oder `Pane::Right` aktiv ist (z. B. aktiver Fokus `DarkGray`, inaktiver Fokus `Blue` oder `> `-Präfix). Aktiven Suchfilter im Titel anzeigen (`Pakete (N) [Filter: ...]`).
     - `render_middle`: Scroll-Offset automatisch so nachführen, dass die Zeile von `middle_selected` immer sichtbar im Viewport (`area.height - 2`) bleibt (`Paragraph::scroll((offset, 0))`).
     - `render_right`: `Paragraph::scroll((state.right_scroll, 0))` anwenden.
     - `render_levers`: Auf `frame.render_stateful_widget(table, area, &mut table_state)` umstellen.
  3. **TestBackend-Tests (`13_tui_view.rs`):** Synthetische Liste mit 100 Paketen + Paket mit 80 USE-Flags + 100 Big-Levers-Einträge anlegen, Selektion jeweils auf Index `75` setzen, mit `TestBackend::new(80, 24)` rendern und im Buffer prüfen, dass Eintrag `75` tatsächlich sichtbar im gerenderten Text vorkommt (und Eintrag `0` herausgescrollt wurde)!
- **Validierung:** `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` grün.

### T07 — Tab-2-Toggle, interaktiver `[[choice]]`-Switcher, sicherer Export & Default-TUI
- **Dateien:** `src/11_cli.rs`, `src/12_tui_state.rs`, `src/13_tui_view.rs`, `src/14_tui_runner.rs`
- **Schritte:**
  1. **Tab 2 (`Big Levers`) Aktion (`12_tui_state.rs`, `14_tui_runner.rs`):** Bei `Tab::BigLevers` schaltet `Space` das selektierte Flag global für alle betroffenen Pakete aus (oder wieder an, falls global deaktiviert) und setzt `dirty = true` + aktualisiert Graph/Levers.
  2. **`[[choice]]`-Alternativen umschaltbar machen (`12_tui_state.rs`, `13_tui_view.rs`, `14_tui_runner.rs`):**
     - `LeftEntry::PlanChoice(String)` (Choice-ID) hinzufügen.
     - `effective_world` dynamisch aus `plan.baseline.world` + gewählten `plan.choice`-Optionen ableiten (wichtig: `sync_plan_from_state` darf `plan.baseline.world` NICHT mit gewählten Choice-Atomen überschreiben!).
     - Rechts neben `Pros`/`Cons` in der Mitte die Optionen auflisten; `Enter` (oder `Space`) wechselt zur nächsten Option und aktualisiert `world` + `Graph` sofort live.
  3. **Sicherer Export (`14_tui_runner.rs`):** Taste `E` ruft vor `export_plan` automatisch `sync_plan_from_state(&mut state.plan, &state.engine, &state.baseline_world)` auf, damit ungespeicherte Toggles immer im Export landen.
  4. **Default-Subcommand (`11_cli.rs`):** `#[command(subcommand)]` in `Cli` zu `Option<Commands>` machen; ist `command` `None`, wird `Commands::Tui` mit Default-`TuiArgs` ausgeführt.
  5. Unit-Tests in `12_tui_state.rs` für Big-Lever-Toggle, Choice-Switch (ohne `baseline.world`-Veränderung) und Export-Sync schreiben.
- **Validierung:**
  - `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` grün.
  - Commit: `fix(tui): add stateful scrolling, choice switcher, big-lever toggle and safe export`

---

## Phase 4: Paket-Deltas für Anwendungsfälle (`ffmpeg`, Alternativen)

### T08 — `FlagDelta` um hinzugefügte/entfernte Pakete & fehlenden-Binhost-Hinweis erweitern
- **Dateien:** `src/01_types.rs`, `src/08_delta.rs`, `src/11_cli.rs`, `src/13_tui_view.rs`, `tests/host_integration.rs`
- **Schritte:**
  1. `FlagDelta` in `01_types.rs` erweitern um:
     - `pub added_packages: Vec<String>` (sortiert)
     - `pub removed_packages: Vec<String>` (sortiert)
     - `pub has_binhost_data: bool` (oder in `AppState` / `FlagDelta` sichtbar, ob Binhost-Größen vorhanden sind).
  2. In `08_delta.rs` (`flag_delta_with_base`): Differenz der `graph.nodes.keys()` zwischen `before_graph` und `outcome.graph` berechnen und in `added_packages`/`removed_packages` speichern.
  3. In `11_cli.rs` (`cmd_deltas`): Ausgabe um `+N/-M Pakete` und aufgelistete Paketnamen ergänzen (z. B. `(+1: media-libs/svt-av1)`), sowie Hinweis ausgeben, falls keine Binhost-Daten geladen wurden (`0 B bin` → `n/a (ohne --binhost)`).
  4. In `13_tui_view.rs`: In der Mittelspalte hinter dem Delta kurz `(+N pkg)` anzeigen und in der rechten Spalte (oder Detailzeile unter den Paketen) für das gerade selektierte Flag die konkret hinzukommenden/wegfallenden Pakete auflisten.
  5. Host-Integrationstest schreiben: `ffmpeg`-ähnliches Testpaket mit Flag `svt-av1` -> zieht `media-libs/svt-av1`; prüfen, dass `added_packages == vec!["media-libs/svt-av1"]`.
- **Validierung:**
  - `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` grün.
  - Commit: `feat(delta): track added and removed packages in FlagDelta and display binhost-na hint`

---

## Phase 5: Portage-Korrektheit & CLI-Robustheit

### T09 — `world`-Fehlerbehandlung, `make.profile`-Symlink, `make.defaults` `USE=` & `use.mask`-Negation
- **Dateien:** `src/02_profile.rs`, `src/11_cli.rs`, `tests/host_integration.rs`
- **Schritte:**
  1. `11_cli.rs` (`load_world`): Fehlende `world`-Datei nicht mehr still mit `unwrap_or_default()` verschlucken, sondern mit sprechender Meldung abbrechen (`"world-Datei nicht gefunden unter ... — bitte mit --world <PFAD> angeben"`).
  2. `11_cli.rs` (`load_profile`): Wenn `--profile` nicht explizit übergeben wurde, prüfen ob `/etc/portage/make.profile` (oder ein konfigurierbarer Pfad) ein Symlink auf `<profiles_dir>/<profilname>` ist und diesen automatisch auflösen, bevor als letzter Fallback `default/linux/amd64/23.0` greift.
  3. `02_profile.rs` (`load_profile_stack`): Auch `USE="..."` aus `make.defaults` entlang des Profilstapels akkumulieren (inkl. `-flag` Negationen und `${USE}`-Erweiterung).
  4. `02_profile.rs` (`read_flag_list` / `extend_flag_list`): Bei `use.mask` und `use.force` bedeuten Zeilen mit `-flag` **Entmaskieren / Aufheben** (`set.remove(flag)`), nicht Hinzufügen! Bestehende Methode korrigieren + Unit-Test schreiben.
  5. `02_profile.rs` (`apply_use_defaults`): `package.use` nicht nur per exaktem String `cat/pkg` suchen, sondern auch Atome mit Version/Slot (`>=cat/pkg-1.0`, `cat/pkg:0`) gegen `PkgVersion` matchen.
- **Validierung:** Alle neuen Unit-Tests + `cargo test` grün.

### T10 — Graph-Version/Slot-Pins, `QuickSwitch`-Vollständigkeit & Export/Verify-Bereinigung
- **Dateien:** `src/06_graph.rs`, `src/07_cascade.rs`, `src/10_export_verify.rs`, `src/11_cli.rs`
- **Schritte:**
  1. `06_graph.rs`: In der Queue das volle einschränkende `Dep`-Atom mitführen, damit `select_for_dep(&dep)` statt `index.select(cpn)` genutzt wird, wenn ein Elternteil eine konkrete Version oder einen Slot fordert.
  2. `07_cascade.rs`: Bei `REQUIRED_USE` (`!flag` verletzt) im `ForceAction::Conflict` valide `QuickSwitch`-Änderungen befüllen (z. B. `vec![(cpn, flag, false)]`).
  3. `10_export_verify.rs` & `11_cli.rs`: Doppelte Export-Logik (`export_portage` vs. `export_plan`) zusammenführen; `VerifyRunner` sauber auf `PORTAGE_CONFIGROOT` + `--root` dokumentieren/korrigieren und per Mock-Test absichern.
- **Validierung:**
  - `cargo fmt --check`, `cargo clippy --all-targets -- -D warnings`, `cargo test` grün.
  - Commit: `fix(portage): fix world error handling, make.profile symlink, make.defaults USE and use.mask negation`

---

## Phase 6: Finale Systemtests, HIL-Abgleich & verständliche Dokumentation

### T11 — Finale Instrumentierungs-Messung & HIL-Regression
- **Schritte:**
  1. Alle Gates ausführen:
     - `cargo fmt --check`
     - `cargo clippy --all-targets -- -D warnings`
     - `cargo test`
     - `cargo test --release --test perf_system -- --nocapture`
  2. Wenn `/tmp/gentoo-hil` existiert:
     - `GENTOO_HIL_REPO=/tmp/gentoo-hil cargo test --release -- --nocapture`
     - Stichproben auf dem echten Snapshot ausführen:
       - `deltas media-video/ffmpeg` (Prüfen: Paketliste bei `svt-av1`/`aom`, Ausführungszeit)
       - `big-levers` (Prüfen: Multi-Core-Laufzeit vs. Vorher)
       - TUI-Smoke-Check
  3. Prüfen, dass **keine** Quelldatei in `src/` ~600 Zeilen sprengt (`wc -l gentoo-dep-tui/src/*.rs`).

### T12 — Verständliche Dokumentation (`README.md` + `walkthrough.md` Update) & `plan_effort_fixes.md`
- **Dateien:** `gentoo-dep-tui/README.md`, `plan/.../walkthrough.md` (bzw. neues Kapitel/Überarbeitung), `plan_effort_fixes.md`
- **Schritte:**
  1. `gentoo-dep-tui/README.md` (und `walkthrough.md` so überarbeiten, dass der Leser abgeholt wird) mit:
     - Kurzer Einleitung ohne Fachjargon + kleinem Glossar.
     - Klare Übersicht: **Wo sucht das Programm seine Dateien** (`world`, `make.conf`, `make.profile`, `package.use`, `repo`, `--binhost`) – getrennt nach *Gentoo-Host* und *Docker-Container*.
     - TUI-Schnellstart (Aufbau, alle Tasten, wie Scrolling/Filter/Tab-2/Choices/Export funktionieren).
     - **Drei Schritt-für-Schritt-Anwendungsfälle mit konkreten Befehlen & Beispielausgaben:**
       1. `ffmpeg`: JPEG→AV1-Support (`svt-av1`/`aom`) – Zusatzpakete & Größe ablesen.
       2. `openssh`-Alternativen vergleichen (`[[choice]]` in `plan.toml` + live in der TUI umschalten!).
       3. `nginx`-Alternativen finden & vermessen (Abgrenzung: warum `gentoo-dep-tui` keine eigene Empfehlungs-DB ist, sondern `||`- und `[[choice]]`-Alternativen vermisst, konkretes Beispiel `nginx` vs. `caddy` vs. `lighttpd` inkl. `BDEPEND`-Unterschied).
     - **Vorher-/Nachher-Tabelle der Instrumentierung** (exakte Zahlen aus `perf_system` und HIL: Laufzeiten, `resolve_calls`, Multi-Core-Speedup).
     - Mermaid-Diagramme für Architektur und TUI-Worker-Fluss.
  2. Token-Aufwand in `plan_effort_fixes.md` festhalten.
  3. Commit: `docs(guide): rewrite walkthrough and add practical user guide with ffmpeg/openssh/nginx workflows`
```

---

### Kurzer Hinweis zur Reihenfolge für dich
Der entscheidende Hebel in diesem Plan ist **T01 + T02 vor T03–T05**: Dadurch, dass der Agent sich zuerst einen **instrumentierten Systemtest** baut (`tests/perf_system.rs`), arbeitet er bei den Optimierungen nicht im Blindflug, sondern muss dir am Ende harte Vorher-/Nachher-Zahlen (z. B. *„`package_deltas`: 120 → 60 Resolves, 1 Kern vs. N Kerne, TUI-Scroll: X ms → <1 ms“*) schwarz auf weiß liefern.
