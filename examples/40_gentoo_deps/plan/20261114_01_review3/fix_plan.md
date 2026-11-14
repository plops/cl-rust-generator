Hier ist das **vollständige Review** (inkl. Auswertung aller Screenshots und Fragen aus `manual_screencaps.md`, Performance-, Architektur- und Doku-Analyse) sowie der **aktualisierte Umsetzungsplan (`review3`)** inklusive Anweisung zur Nutzung von **DeepWiki MCP**, **Shared-Blocker-Ersparnis-Analyse** (`gtk` bei `emacs` & Co.) und **skriptbarer Test-/Screencapture-Architektur**.

---

# Teil 1: Auswertung der Screenshots & Antworten auf deine Fragen

Deine manuellen Screenshots haben **vier echte Software-Bugs** aufgedeckt. Vieles, was in der Anzeige rätselhaft wirkte, war kein Verständnisfehler von dir, sondern ein Fehler im Tool.

### 1. Untere Statuszeile (`Binpkg 791.5 MB  Download 3318.1 MB  BDEPEND 243.2 MB  Pakete 555  Konflikte 0`)
Was diese Zahlen im aktuellen Code (`06_graph.rs`, `06_graph/build_extra.rs`, `13_tui_view/bottom.rs`) bedeuten:
* **`Binpkg 791.5 MB`**: Summe der komprimierten Binärpaket-Größen (`SIZE:` aus `--binhost Packages`) über alle 555 Runtime-Pakete (`RDEPEND` + `PDEPEND`). **Wichtig:** Pakete ohne Treffer im Binhost-Index (z. B. `sys-kernel/gentoo-sources` in Screenshot 1 mit `Binpkg: 0 B`) zählen dort mit **`0 B`** mit. Die echte Gesamtgröße wird also unterschätzt, sobald nicht 100 % der Pakete im Binhost liegen (bisher zeigt nur `index-check` diese Trefferquote, nicht die TUI).
* **`Download 3318.1 MB`**: Enthält **ausschließlich Quell-Tarballs** (`Manifest`-`DIST`-Summen der aktiven `SRC_URI`-Einträge aller 555 Runtime-Pakete) — **keine** Binärpakete. Der Name `Download` ist missverständlich.
* **`BDEPEND 243.2 MB`**: Enthält **weder nur `BDEPEND` noch Quellen**, sondern die **Binhost-`SIZE`-Summe** aller Build-Only-Pakete (`DEPEND` + `BDEPEND` + `IDEPEND` inkl. deren Transitiva minus Runtime). Der dazugehörige Quell-Download (`graph.build_extra.download_bytes`) wird zwar intern berechnet, aber nirgends in der TUI angezeigt.
* **Fehlend im Footer:** Hinweise auf `[/] Suche` und `[?] Hilfe`.

### 2. Screenshot 2 & 3 (`media-video/ffmpeg`): `Binpkg: 9.9 MB  Subgraph-DL: 11.2 MB` & seltsame Flags
Hier stecken **zwei Bugs**:
1. **Falsche Beschriftung `Subgraph-DL: 11.2 MB` (`13_tui_view/middle.rs`):** Dort wird `node.download` angezeigt. Das ist **nicht** der Download des gesamten `ffmpeg`-Subgraphen, sondern **nur der Quell-Tarball von `ffmpeg` selbst** (~11,2 MB). Das muss `Pkg-Quell-DL` heißen.
2. **`ffmpeg` zeigt die Flags von `sys-kernel/gentoo-sources`! (Filter-Bug in `12_tui_state.rs`):**
   Vergleiche Screenshot 1 (`gentoo-sources`) und Screenshot 2/3 (`ffmpeg`): Die Flag-Liste in der Mitte ist **identisch** (`build`, `experimental`, `symlink`, `vanilla`, `test-rust`, …)!
   **Ursache:** Vor dem Tippen von `/ffm` war links `sys-kernel/gentoo-sources` selektiert. `apply_filter_only()` filterte zwar die linke Liste auf `[media-video/ffmpeg]`, ließ `state.flag_deltas` aber explizit unangetastet (`// Mitte unangetastet`) und setzte `deltas_loading` nicht. Selbst nach `Enter` (`filtering = false`) wurde nicht neu geladen.
3. **Was bedeutet `[ ] build -6.8 MB bin / -46.2 MB dl (+0/-11 pkg)` (und die rechte Spalte)?**
   Das Flag stammt von `gentoo-sources`: Dort gibt es eine umgekehrte Bedingung `!build? ( bc bison flex make ... )`. Wenn `build` **aus** ist (`[ ]`), werden die 11 Kernel-Bauwerkzeuge installiert; schaltest du `build` **an**, fallen diese 11 Pakete weg (`+0/-11 pkg`), was die rechte Seite (`Flag build zieht: - app-alternatives/bc ...`) korrekt auflistet.

### 3. Warum so viele `0 B`-Werte, was bedeuten `abi_x86_64`/`cet` und warum hat `openmp` `555` Pakete?
Hier liegt der **schwerwiegendste Core-Bug** in `02_profile.rs` (`Profile::effective_defaults`):
* **Bug (`effective_defaults` ohne `IUSE`-Gate):** `global_use` (aus `make.defaults` / `make.conf`, z. B. `openmp`, `cet`, `iconv`, `test-rust`) und `use_force` werden **jedem** Paket im System hinzugefügt — **auch wenn das Paket das Flag gar nicht in `IUSE` deklariert!**
  * Folge 1: `gentoo-sources` und `emacs` zeigen Dutzende Phantom-Flags (`openmp`, `cet`, `llvm_targets_X86`, `abi_x86_64`, `elibc_glibc`, `kernel_linux`) mit **`0 B / 0 B`**.
  * Folge 2 (Screenshot 5): `Global Big Levers` zählt `openmp` auf **allen 555 Paketen** (`Pakete: 555`), obwohl es in Wahrheit nur sehr wenige Pakete in `IUSE` haben.
  * Folge 3: `llvm` (-112,0 MB) und `openmp` (-112,8 MB) treffen vermutlich dasselbe große Paket (z. B. `llvm` / `libomp` über ein Paket, das `openmp` tatsächlich in `IUSE` führt) — nur zeigt `Big Levers` aktuell leider nicht, *welche* Pakete betroffen sind und *welche* Pakete konkret wegfallen.
* **Legitime `0 B`-Gründe** (auch nach dem Fix):
  1. Das Flag schaltet nur Compile-Features ohne neue Runtime-Pakete/Distfiles.
  2. Die gezogene Bibliothek (z. B. `gtk+`, `openssl`) wird bereits von einem anderen Paket im Graphen gebraucht (**Shared Dependency**, `refcount >= 2`).
* **Bedeutung der Flags (`abi_x86_64`, `cet`):**
  * `abi_x86_64`: 64-Bit-x86-ABI (im `no-multilib`-Profil ohnehin per `use.force` / `USE_EXPAND_HIDDEN` fest).
  * `cet`: Intel/AMD Control-flow Enforcement Technology (Hardware-Exploit-Schutz).
  * Alle Beschreibungen stehen in Gentoo bereits **offline** in `<repo>/profiles/use.desc`, `use.local.desc` und `desc/*.desc` und können direkt in der TUI angezeigt werden.

### 4. Screenshot 4 (`app-editors/emacs`): Interpretation von `gui`/`Xaw3d`/`gtk`/`motif` & leere rechte Pane
* **Interpretation:**
  * `gui` ist `[x]` (an) und `Xaw3d` ist `[x]` (zieht `x11-libs/libXaw3d` ≈ 220 kB als Toolkit-Fallback). Schaltest du `gui` aus, entfällt `libXaw3d` (`-220.0 kB`).
  * `gtk` ist `[ ]` (aus). Schaltest du `gtk` an, kostet `gtk+` **`0 B`** (weil z. B. `firefox-bin` o. Ä. `gtk+` bereits im Graphen hält!) und der `Xaw3d`-Fallback entfällt (`-220.0 kB`).
  * **Dein neuer Wunsch passt hier exakt:** Wenn `gtk` an wäre (bzw. bei Shared-Flags allgemein), sieht man bisher *nicht*, wie viel Platz man **insgesamt** sparen könnte, wenn man `gtk` bei `emacs` **und** allen mitbetroffenen Paketen abschaltet bzw. welche `world`-Pakete dafür entfernt werden müssten. Das kommt als neues Feature in den Plan!
* **Warum ist die rechte Spalte in Screenshot 4 leer?**
  Ein UI-Bug: `state.right_scroll` wird beim Paketwechsel (`select_left_fast` / `rebuild_middle`) **nicht auf `0` zurückgesetzt**. Wurde vorher rechts gescrollt, startet das nächste Paket unterhalb seines Inhalts im Leeren.

### 5. `Verify`-Fehler (`ENOENT` auf `/tmp/gentoo-export`)
* **Ursache 1:** `[V] Verify` (`14_tui_runner.rs`, `10_export_verify.rs`) startet `emerge` mit `.current_dir("/tmp/gentoo-export")`. Wenn du nicht vorher `[E] Export` gedrückt hast, existiert das Verzeichnis nicht → `os error 2` noch vor `emerge`-Start.
* **Ursache 2:** Selbst nach `[E]` schreibt `export_plan` nur `world` und `package.use` nach `/tmp/gentoo-export/etc/portage/`, ohne `make.profile`, `make.conf` und `repos.conf`. Für `emerge --config-root /tmp/gentoo-export` fehlen demnach die System-/Repo-Definitionen des laufenden Gentoo.

---

# Teil 2: Architektur-, Performance- & Dokumentations-Review

### A. Performance-Engpässe („immer noch etwas langsam")
Gemessen auf 32-Kern-Host: `big-levers` braucht `7,7 s` Wandzeit / `218 s` CPU-Zeit (auf einem normalen 4–8-Kerner also ~30–60 s). Ursachen neben dem Phantom-Flag-Bug:

1. **Massives redundantes `Dep::parse` in Hot Loops:**
   * `Profile::package_use_for()` (`02_profile.rs`) sammelt, sortiert und parst (`Dep::parse` + `Version::parse`) **alle** `package.use`-Einträge bei **jedem** `effective_use()`-Aufruf neu → zig Millionen unnötige Parses bei `big-levers`.
   * `is_masked()` (`05_repo_index.rs`) parst **alle** `package.mask`-Zeilen für **jede** der ~40.000 `md5-cache`-Dateien beim Laden neu.
   * `resolve_with_options()` (`06_graph/resolve.rs`) serialisiert `dep.to_string()` für die BFS-Queue und ruft in `resolve_target()` direkt wieder `Dep::parse()` auf.
2. **Mehrfachberechnungen & unnötige Arbeit in `06_graph` / `07_cascade`:**
   * `CascadeStep` (`07_cascade/step.rs`, `enforce.rs`) ruft `effective_use()` mehrfach pro Knoten/Edge auf, obwohl `graph.nodes` die Flags bereits enthält, und klont `HashSet<String>` + `out_edges`.
   * Bei `!changed` und Konflikten macht `resolve_with_cascade_opts` (`07_cascade.rs`) am Schleifenende einen redundanten Extra-`resolve`.
   * Delta-Simulationen bauen jedes Mal `reverse`, `via_map` und `alternatives` für Blame auf, obwohl sie nur `totals()`, Knotennamen und `conflicts` brauchen.
3. **Fehlender exakter No-Op-Skip & fehlende Baseline-Wiederverwendung:**
   * Flags, die weder in `RDEPEND`/`PDEPEND`/`SRC_URI`/`REQUIRED_USE` des Pakets noch in eingehenden `use_deps` (`out_edges`) vorkommen, lösen trotzdem eine volle 570-Knoten-Kaskade aus, obwohl das Delta garantiert `0 B / 0 B / kein Konflikt` ist.
   * `AppState` verwirft `CascadeOutcome` nach `refresh()` und rechnet die Baseline bei `package_deltas` und `ensure_levers()` jeweils erneut.
4. **Ungenutztes `PkgId`/`FlagId` & Mutex-Contention:**
   * `01_types.rs` deklariert `PkgId`, `FlagId`, `IdTable`, nutzt sie aber nicht; alles arbeitet auf geclonten `String`-Maps mit linearem `.contains()` in `push_edge`/`push_reverse`.
   * `RepoIndex::load()` synchronisiert 32 Threads über einen einzelnen `Mutex` für `Manifest`-Reads.

### B. Architektur & Vereinfachungspotenzial
* **Stärken:** Die 14-stufige Datenfluss-Trennung (`01`..`14`) sowie die Trennung `12_tui_state` / `13_tui_view` / `14_tui_runner` sind sehr sauber und testbar.
* **Vereinfachungen:**
  1. **`Profile` vorparsen:** `package.mask` und `package.use` einmalig beim Aufbau nach `cpn` indiziert parsen statt pro Traversal.
  2. **Ein Atom→CPN-Kürzer:** `world_atom_cpn` (`12_tui_state.rs`, buggy bei `:slot`/`[use]`) und `world_cpn` (`06_graph/blame.rs`) durch eine zentrale Funktion `atom_cpn()` ersetzen.
  3. **Dichte IDs (`PkgId`) oder Referenz-Queue in `06`/`07`:** `PkgId` produktiv verdrahten (oder ungenutztes `FlagId`/`IdTable` entfernen, damit keine toten Versprechen im Code bleiben).
  4. **Optionale Instrumentierung via Cargo-Feature (`perf-metrics`):**
     Statt hart einkompilierter globaler Atomics in `01_types.rs` wandert jede Zähler- und Zeitmessung hinter `#[cfg(feature = "perf-metrics")]` (mit `#[inline(always)]`-No-Op-Stubs im Standard-Build). So entsteht im normalen Release **null Overhead**, für Review-Cycles erhältst du detaillierte Phasen-Reports (`--features perf-metrics`), und für ein finales Release lässt sich das Feature sauber deaktivieren oder entfernen.
  5. **Ein gemeinsamer Key-Dispatcher (`handle_key_event`) für TUI & Tests/Scripting:**
     Aktuell steckt die Tastatur-Logik direkt in der Terminal-Loop (`14_tui_runner.rs`), während Tests Methoden direkt auf `AppState` aufrufen. Ein extrahierter Key-Dispatcher ermöglicht **identisches Scripting & Screencapturing** im Integrationstest und per CLI.

### C. Dokumentations-Defizite
* `README.md` braucht einen Abschnitt „Zahlen & Metriken richtig lesen", Dokumentation der Suche (`/`), Hilfe (`?`), Flag-Doku (`use.desc`), `Verify`-Voraussetzungen (`[E]` + Configroot), Shared-Blocker-Analyse sowie den `--features perf-metrics`-Workflow und das Screencapture-Testskript.
* Modul-Docstrings in `02_profile.rs`, `05_repo_index.rs`, `08_delta.rs` und `14_tui_runner.rs` müssen den neuen Regeln angepasst werden; der tote Cast in `08_delta.rs` (Zeile 284) gehört entfernt.

---

# Teil 3: Umsetzungsplan (`review3`) für den AI-Agenten

## Verbindliche Arbeitsanweisung für den AI-Agenten (Tools & Abhängigkeiten)
> **DeepWiki MCP Pflicht:**
> Wann immer du bestehende Dependencies prüfst oder anpasst (`portage-atom`, `portage-metadata`, `ratatui`, `crossterm`, `rayon`, `clap`, `toml`) **oder** eine neue Dependency evaluierst/hinzufügst, **musst du zwingend das `deepwiki` MCP-Tool nutzen**, um die exakten APIs, Feature-Flags, EAPI-/PMS-Verhaltensweisen und Performance-Idiome der jeweiligen Crates zu verifizieren (kein Raten von APIs!). Prüfe vor dem Hinzufügen neuer Crates zunächst über `deepwiki`, ob die vorhandenen Dependencies das Problem bereits lösen.

---

### Phase 1: Korrektheits-, Filter- & State-Bugs beheben
*Ziel: Korrekte IUSE-Semantik, keine Phantom-Flags mehr, kein veraltetes Mittel-Pane beim Filtern, konsistente Navigation.*

* **Task 1.1 — `IUSE`-Gate, `USE_EXPAND_HIDDEN` & Mask/Force-Schutz (`02_profile`, `06_graph`)**
  * Prüfe via **DeepWiki MCP** (`portage-metadata`, `portage-atom`), welche `IUSE`-/Implicit-Flag-Felder oder Hilfsmethoden `CacheEntry` bietet.
  * In `Profile::effective_defaults` (`src/02_profile.rs`): `global_use`, `package_use_for` und `use_force` dürfen **nur** Flags setzen/entfernen, die in `iuse_defaults.contains_key(flag)` enthalten sind.
  * `ARCH`, `ELIBC`, `KERNEL` (plus `USE_EXPAND_IMPLICIT`) bleiben für `*DEPEND`/`SRC_URI`/`REQUIRED_USE`-Auswertung aktiv, werden in `08_delta.rs` aber strikt aus `package_deltas` und `big_levers` gefiltert.
  * In `src/02_profile/stack.rs`: `USE_EXPAND_HIDDEN` aus `make.defaults` einlesen (`Profile::use_expand_hidden: HashSet<String>`), sodass `08_delta.rs` auch versteckte Expand-Flags (z. B. `abi_x86_64`, `elibc_*`, `kernel_*`) aus der schaltbaren Liste ausblendet.
  * In `Engine::effective_use` (`src/06_graph.rs`): `overrides` darf `use_mask` und `use_force` nicht still aushebeln (Maske/Force behalten Vorrang).
  * *Tests:* Flag aus `global_use` ohne `IUSE`-Eintrag taucht weder in `node.use_flags` noch in `package_deltas` oder `big_levers` auf; `USE_EXPAND_HIDDEN` wird ausgeblendet.

* **Task 1.2 — Filter-Stale-Bug, `right_scroll`-Reset & `Esc`-Verhalten (`12_tui_state`, `14_tui_runner`)**
  * In `AppState::apply_filter_only()` (`src/12_tui_state.rs`): Vor `rebuild_left()` bisheriges `selected_package()` / `LeftEntry` merken; falls sich die Selektion danach geändert hat, `select_left_fast(self.left_selected)` aufrufen (leert `flag_deltas` und setzt `deltas_loading = true`), damit `ensure_deltas()` sofort die passenden Flags nachlädt.
  * `self.right_scroll = 0;` in `select_left_fast()` und beim Paketwechsel in `rebuild_middle()` zurücksetzen.
  * In `14_tui_runner.rs`: `Esc` im normalen Modus (`Pane::Left`, kein Modal, kein Filtering) löscht zuerst einen aktiven Filter (`!state.filter.is_empty()` → `filter.clear(); apply_filter_only()`), bevor ein weiteres `Esc` beendet (`q` beendet immer).

* **Task 1.3 — Einheitliches `atom_cpn()` (`01_types` / `06_graph/blame.rs` / `12_tui_state.rs`)**
  * `pub fn atom_cpn(atom: &str) -> String` zentral bereitstellen (`Dep::parse` mit String-Fallback), `world_atom_cpn` und `world_cpn` damit ersetzen (inkl. `:slot`/`[use]`-Tests).

* **Gate Phase 1:** `cargo test && cargo clippy --all-targets -- -D warnings`.

---

### Phase 2: Optionale Performance-Instrumentierung (`perf-metrics`)
*Ziel: Phasenweise Laufzeit- und Counter-Messung für zukünftige Review-Cycles, zero-cost im normalen Release-Build.*

* **Task 2.1 — Cargo-Feature `perf-metrics` in `Cargo.toml` & `src/00_perf.rs`**
  * `[features] default = []`, `perf-metrics = []`.
  * Zentrale Instrumentierung (`PerfStats`, `ScopeTimer`, atomare Zähler/Nanosekunden-Akkumulatoren) unter `#[cfg(feature = "perf-metrics")]` implementieren; unter `#[cfg(not(feature = "perf-metrics"))]` identische API als `#[inline(always)]` No-Op-Stubs.
  * Gemessene Phasen & Zähler:
    * `index_load_ns`, `index_mask_checks`, `index_dep_parses`
    * `effective_use_calls`, `effective_use_ns`, `package_use_match_calls`
    * `resolve_calls`, `resolve_ns`, `build_extra_calls`, `build_extra_ns`
    * `cascade_calls`, `cascade_rounds`, `cascade_ns`
    * `delta_runs`, `delta_noop_skips`, `delta_ns`
    * `big_levers_runs`, `big_levers_noop_skips`, `big_levers_ns`
    * `delta_cache_hits`, `delta_cache_misses`
  * Alte unkonditionale Atomics aus `01_types.rs` in `00_perf.rs` überführen.

* **Task 2.2 — CLI-Flag `--perf-report` & Integration in `tests/perf_system.rs`**
  * In `CommonArgs`: optionales `--perf-report` (druckt am Ende von `index-check`, `graph`, `deltas`, `big-levers` den Phasen-Report; ohne Feature kurzer Hinweis auf `--features perf-metrics`).
  * `tests/perf_system.rs` so aufbauen, dass `cargo test` immer grün läuft und `cargo test --release --features perf-metrics --test perf_system -- --nocapture` alle Phasen-Zeiten und Counter prüft & ausgibt.

* **Gate Phase 2:** `cargo test`, `cargo test --release --features perf-metrics --test perf_system -- --nocapture`, `cargo clippy --all-targets --all-features -- -D warnings`.

---

### Phase 3: Performance-Optimierung (Hot Paths & Algorithmik)
*Ziel: Massive Reduktion der CPU-Zeit bei `RepoIndex::load`, `deltas` und `big-levers` bei 100 % identischen Ergebnissen.*

* **Task 3.1 — `Profile` beim Laden vorparsen (`02_profile`, `05_repo_index`)**
  * `package.mask` und `package.use` in `Profile` nach `cpn` vorindiziert und mit `Dep::parse` genau **einmal** beim Laden parsen (inkl. Synchronisations-Helfer, falls Tests direkt mutieren).
  * `is_masked` und `package_use_for` machen nur noch einen `HashMap::get(cpn)`-Lookup gegen `PkgVersion.version` — null `Dep::parse` / `Version::parse` in Hot Loops.

* **Task 3.2 — Lockfreies `Manifest`-Laden (`05_repo_index.rs`)**
  * Eindeutige `(cat, pkg)`-Paare der Cache-Dateien vorab parallel einlesen (`HashMap<String, Manifest>`), danach ohne `Mutex` read-only an `parse_cache_file` übergeben.
  * `RepoIndex::has_any_binhost: bool` einmalig beim Laden vorberechnen.

* **Task 3.3 — `06_graph` & `07_cascade` entschlacken & `PkgId` sinnvoll nutzen**
  * `PkgId(u32)` in `RepoIndex` jedem `PkgVersion` (und optional `CpnId(u32)` je `cpn`) zuweisen oder BFS-Queue in `06_graph/resolve.rs` auf `VecDeque<&'a PkgVersion>` umstellen (eliminiert `dep.to_string()` + `Dep::parse()` pro BFS-Knoten komplett). Ungenutzte `FlagId`/`IdTable` entfernen, falls sie nicht verdrahtet werden.
  * In `resolve_with_options`: Parameter `collect_blame: bool` ergänzen (`false` bei Delta-/Big-Lever-Simulationen → `reverse`, `via_map` und `alternatives` überspringen).
  * In `07_cascade/step.rs` & `enforce.rs`: Aktive Flags aus dem bereits berechneten Knoten/Cache beziehen statt mehrfacher `effective_use`-Neuberechnung pro Edge; `resolve_with_cascade_opts` verzichtet bei `!changed` am Ende auf das doppelte `resolve_with_options`.

* **Task 3.4 — Exakter No-Op-Skip & Baseline-Wiederverwendung (`08_delta.rs`, `12_tui_state.rs`)**
  * In `08_delta.rs`: Exakter Check `flag_can_affect_graph(pkg, flag, incoming_use_deps)`:
    * Kommt `flag` in `pkg` (`rdepend`, `pdepend`, `src_uri`, `required_use`) und in keinem eingehenden `use_dep` von `base.graph.out_edges` vor, wird ohne `resolve_with_cascade_opts` sofort `0 B / 0 B / kein Konflikt` geliefert (und als `noop_skip` gezählt).
    * **Äquivalenz-Test:** Gegen ein Test-Set mit vollständigen Simulationen prüfen, dass `noop_skip` bit-identische Deltas und Big Levers liefert.
  * `package_deltas_with_base` einführen; `AppState` behält `last_outcome: CascadeOutcome` und reicht es an `package_deltas_with_base` und `big_levers_with_base` durch.
  * `big_levers` deterministisch nach `(binpkg_bytes, download_bytes, flag)` sortieren; toten Cast in `08_delta.rs:284` entfernen.

* **Gate Phase 3:** `cargo test --release --features perf-metrics --test perf_system -- --nocapture` + `cargo clippy --all-targets --all-features -- -D warnings`. (Optional auf Gentoo-Host mit `/tmp/gentoo-hil` Laufzeiten vor/nach Phase 3 messen.)

---

### Phase 4: Shared-Blocker-Ersparnis („Mit-Abschaltung"), USE-Flag-Doku & Big-Levers-Details
*Ziel: Die Software beantwortet „Wie viel spart `gtk` bei `emacs` wirklich — und wer müsste dafür weichen?" sowie „Was bedeutet dieses Flag?".*

* **Task 4.1 — Shared-Blocker- & Voll-Ersparnis-Analyse (`08_delta.rs` / neues Submodul `08_delta/shared.rs`, `12_tui_state.rs`, `13_tui_view/right.rs`)**
  * Neue On-Demand-Funktion `shared_removal_impact(engine, world, base, package, flag) -> Option<SharedRemovalImpact>` (nur für das aktuell selektierte Flag berechnet + im State gecacht, also **kein** Einfluss auf die Geschwindigkeit von `package_deltas` / `big_levers`!):
    * Ermittelt die durch `package[flag]` gezogenen Abhängigkeiten (z. B. `x11-libs/gtk+`), die beim alleinigen Abschalten auf `package` durch andere Pakete im Graphen gehalten würden.
    * Läuft über `base.graph.reverse` rückwärts und trennt die Blocker in:
      1. **`flag_blockers: Vec<(String, String)>`**: andere Pakete, bei denen ebenfalls nur ein bestimmtes USE-Flag abgeschaltet werden müsste (z. B. `app-text/evince [gtk]`),
      2. **`world_blockers: Vec<String>`**: `world`-Atome (oder Choice-Pakete), die diese Abhängigkeit *unbedingt* (`via_flag == None`) brauchen und daher entfernt/getauscht werden müssten (z. B. `www-client/firefox-bin`).
    * Simuliert **einmal exakt** über `resolve_with_cascade_opts`:
      * `without_flag_blockers`: Ersparnis (`SizeMetrics` + entfernte Pakete), wenn `package[flag]` + alle `flag_blockers` ausgeschaltet werden.
      * `without_all_blockers`: maximale Ersparnis (`SizeMetrics` + entfernte Pakete), wenn zusätzlich `world_blockers` aus einer Kopie von `world` weggenommen werden.
  * Anzeige in der rechten Pane (`right.rs`, unterhalb von `Flag <x> zieht:`):
    * Zeigt bei Bedarf kompakt:
      * `Voll-Ersparnis (ohne Flag-Blocker): -X MB bin / -Y MB dl (-N pkg)` + Liste `Flag aus bei: ...`
      * `Max-Ersparnis (inkl. world-Entfernung): -A MB bin (-M pkg)` + `world entfernen: ...`
  * Zusätzlich in CLI: `deltas <pkg> --deep` (oder eigenes Subcommand `impact <pkg> <flag>`), um denselben Bericht auch auf der Konsole abrufen zu können.
  * *Unit-Test:* Diamond (`app/a[ssl]` + `app/b[ssl]` + `app/c` unbedingt → `openssl`): Marginal bei `app/a` ist `0 B`, `flag_blockers` nennt `app/b[ssl]`, `world_blockers` nennt `app/c`, und `without_all_blockers` weist exakt die volle `openssl`-Größe aus!

* **Task 4.2 — Offline-USE-Flag-Beschreibungen aus `<repo>/profiles/` (`use.desc`, `use.local.desc`, `desc/*.desc`)**
  * `UseDescriptions` einführen: lädt fehlertolerant `profiles/use.desc`, `profiles/use.local.desc` und `profiles/desc/*.desc`.
  * In `13_tui_view/right.rs`: Beschreibung des aktuell in der Mitte selektierten Flags direkt anzeigen (lokale Beschreibung vor global/`USE_EXPAND`).

* **Task 4.3 — Big-Levers-Detailbereich (`01_types.rs`, `08_delta.rs`, `13_tui_view/bottom.rs`)**
  * `BigLever` um `affected_packages: Vec<String>` und `added_packages` / `removed_packages` ergänzen.
  * Tab 2 (`render_levers`) unterhalb der Tabelle um eine kompakte Detailbox (Beschreibung des selektierten Flags, betroffene Pakete, hinzugefügte/entfernte Pakete wie `llvm-core/llvm`) erweitern — löst sofort Fragen wie *„Warum `llvm` und welche Pakete sind das?"*.

* **Gate Phase 4:** Unit-Tests für `shared_removal_impact` und `UseDescriptions`, `TestBackend`-Render-Tests für die rechte Pane und Tab 2.

---

### Phase 5: Metrik-Klarheit, Hilfe-Overlay (`?`) & Verify-Workflow
*Ziel: Eindeutige Zahlen, eingebaute Hilfe, fehlertolerantes und selbsterklärendes `Verify`.*

* **Task 5.1 — Metrik-Auftrennung & Labels (`middle.rs`, `bottom.rs`, `11_cli/commands.rs`)**
  * `13_tui_view/middle.rs`: `Subgraph-DL:` → `Pkg-Quell-DL:`, Download-Deltas ebenfalls über `fmt_delta(..., "dl")` mit `+`-Vorzeichen bei Zunahme.
  * `13_tui_view/bottom.rs` & `cmd_graph`:
    * `Binpkg <X> (<hits>/<total> Hits)` (oder `n/a (ohne --binhost)`)
    * `Quell-DL <Y>`
    * `Build-Only <B_bin> bin / <B_src> dl`
  * Footer-Tastenleiste um `[/] Suche` und `[?] Hilfe` erweitern (ohne Clipping bei `80x24` testen).

* **Task 5.2 — Interaktives Hilfe-Modal (`?` / `F1`) (`12_tui_state`, `13_tui_view`, `14_tui_runner`)**
  * `Modal::Help` ergänzen; `?` oder `F1` öffnet, `Esc`/`?`/`q` schließt.
  * Erklärt kompakt: Metriken (`Binpkg`, `Quell-DL`, `Build-Only`, `0 B`-Gründe, `Voll-Ersparnis`), Navigation (`/` Suche, `Esc` Filter löschen, `Tab`, `Space`, `Enter`) und Export/Verify-Reihenfolge (`[S]`/`[E]` → `[V]`).

* **Task 5.3 — Robuster `Verify`-Workflow (`10_export_verify.rs`, `11_cli/commands.rs`, `14_tui_runner.rs`)**
  * `VerifyRunner::run()`: Vorab prüfen, ob `self.workdir` existiert (sonst klare Fehlermeldung: Verzeichnis fehlt → zuerst `[E] Export` bzw. `gentoo-dep-tui export` ausführen, Verweis auf `README.md`) und ob `emerge` im `PATH` liegt.
  * Bei `write_export_files` / `export_plan` (optional über `ExportContext` mit `--repo`, `--profile`, `--make-conf` bzw. Host-`/etc/portage`): `etc/portage/make.profile`, `etc/portage/make.conf` und `etc/portage/repos.conf` in `<out_dir>/etc/portage/` verlinken/ergänzen (falls nicht vorhanden), sodass `emerge --root <out> --config-root <out> @world` auf einem Gentoo-Host direkt gegen die laufenden Paketdefinitionen funktioniert.
  * In `14_tui_runner.rs` bei `V`: Wenn `export_default` noch nicht existiert oder `state.dirty`, vor dem Verify automatisch synchronisieren + exportieren (mit Statusmeldung), danach `VerifyRunner` ausführen.

* **Gate Phase 5:** Tests für `VerifyRunner` (fehlendes Dir, fehlendes Binary, Configroot-Symlinks), `TestBackend`-Tests für Footer & `Modal::Help`.

---

### Phase 6: Skriptbare TUI-Tests & Screencapture-Export für Reviews
*Ziel: Automatisierte End-to-End-TUI-Tests mit echtem Key-Dispatch + Export lesbarer Screencaptures für spätere Review-Cycles.*

* **Task 6.1 — Key-Dispatcher aus `14_tui_runner.rs` extrahieren**
  * Prüfe via **DeepWiki MCP** `ratatui` (`TestBackend`, `Buffer` rendering) und `crossterm` (`KeyEvent` / `KeyCode` Konstruktion).
  * Event-Verarbeitung aus `event_loop()` in eine reine, terminal-unabhängige Funktion `pub fn dispatch_key(state: &mut AppState<'_>, key: KeyEvent, ctx: &mut RunnerContext) -> anyhow::Result<LoopAction>` auslagern (inkl. synchronem Test-Helper `step_idle()` für `ensure_deltas()`).
  * Die echte TUI und der Test-/Script-Driver rufen ab sofort exakt dieselbe Funktion auf.

* **Task 6.2 — Script-Engine & Screencapture-Export (`tests/tui_screencaps.rs` + optional `--script` / `GENTOO_SCREENCAP_DIR`)**
  * Kleiner, wartbarer Test-/Script-Treiber (`run_script(&mut state, steps, width, height) -> Vec<Screencap>`), der Tastatur-Skripte (`Type("/ffm")`, `Key(Enter)`, `Idle`, `Key(Tab)`, `Key(Down)`, `Key(Space)`, `Key('2')`, `Key('?')`, `Capture("02_ffmpeg_filter")`) gegen `ratatui::backend::TestBackend` (z. B. `140x36` und `80x24`) ausführt.
  * Optional CLI-Unterstützung in `TuiArgs` (`--script <DATEI>` / `--screencap-out <DATEI>`), damit man auch direkt gegen einen echten Gentoo-Snapshot (HIL) automatisiert eine komplette Screencapture-Serie als Markdown-Datei erzeugen kann.
  * **Abschluss-Integrationstest `tests/tui_screencaps.rs`:**
    * Baut ein realitätsnahes synthetisches Repo (inkl. `gentoo-sources` mit `!build?`, `ffmpeg` mit `svt-av1` + `REQUIRED_USE`, `emacs` mit `gui`/`Xaw3d`/`gtk`, `firefox-bin` als Shared-Blocker für `gtk`, Konflikt-Szenario, `use.desc` + `use.local.desc`, `[[choice]]`).
    * Durchläuft automatisiert das komplette Szenario aus deinen Screenshots:
      1. Startansicht (`01_start.txt`)
      2. Suche `/ffm` + `Enter` (`02_ffmpeg_after_filter.txt`) — **prüft per Assertion**, dass *keine* `gentoo-sources`-Flags mehr stehen und `Pkg-Quell-DL` korrekt angezeigt wird!
      3. Suche `/emacs` + `Enter`, Navigation auf `gtk`/`gui` (`03_emacs_shared_blocker.txt`) — **prüft per Assertion**, dass Flag-Beschreibung und Shared-Blocker-/Voll-Ersparnis-Hinweis erscheinen und rechte Pane nicht leer ist!
      4. Tab `2` (`Big Levers`) (`04_big_levers.txt`) — **prüft**, dass keine Phantom-Flags (`555`-Bug) auftauchen und Detailbox unten sichtbar ist!
      5. Hilfe `?` (`05_help_modal.txt`)
      6. Konflikt-Modal (`06_conflict_modal.txt`)
    * Schreibt alle gerenderten Frames gesammelt in ein lesbares Review-Artefakt (z. B. `target/screencaps/review_screencaps.md`, oder bei gesetztem `GENTOO_SCREENCAP_OUT=<pfad>` direkt an den gewünschten Ablageort für das nächste Review).
    * Optional in `tests/hil_regression.rs` (wenn `GENTOO_HIL_REPO` gesetzt): läuft dasselbe Skript auch gegen den echten Gentoo-Snapshot und speichert `hil_screencaps.md`.

* **Gate Phase 6:** `cargo test --test tui_screencaps` grün, erzeugtes Screencapture-Markdown manuell gegenlesen und auf Sauberkeit prüfen.

---

### Phase 7: Dokumentation & Endabnahme
*Ziel: Vollständige Benutzer-/Entwicklerdoku und grüne Gates im Container sowie optional auf dem Gentoo-Host.*

* **Task 7.1 — `gentoo-dep-tui/README.md` erweitern**
  * Neuer Abschnitt **„Zahlen & Metriken richtig lesen"**: `Binpkg` (inkl. Binhost-Trefferquote & `0 B` bei Source-Paketen), `Quell-DL`, `Build-Only` (Bin vs. Src), `Pkg-Quell-DL`, `0 B`-Deltas, Shared-Blocker-/Voll-Ersparnis-Analyse, `Big Levers` (inkl. warum `dl` bei `!flag?` steigen kann).
  * Tastentabelle aktualisieren (`/` Suche, `?` / `F1` Hilfe, `Esc` Filter zurücksetzen).
  * Abschnitte zu `USE`-Flag-Doku (`use.desc`/`use.local.desc`), `Verify` (`[E]` + `/etc/portage`-Anbindung), `--features perf-metrics` sowie Ausführung der Screencapture-Suite (`tests/tui_screencaps.rs` / `--script`) ergänzen.

* **Task 7.2 — Modul-Docstrings & finale Gates**
  * Docstrings der geänderten Module (`00_perf`, `02_profile`, `05_repo_index`, `08_delta`, `10_export_verify`, `12_tui_state`, `14_tui_runner`) auf aktuellen Stand bringen.
  * Finale Verifikation:
    1. `cargo fmt --check`
    2. `cargo clippy --all-targets --all-features -- -D warnings`
    3. `cargo test`
    4. `cargo test --release --features perf-metrics --test perf_system -- --nocapture`
    5. `GENTOO_SCREENCAP_OUT=plan/20261114_01_review3/screencaps_after.md cargo test --test tui_screencaps` (speichert die Abschluss-Screencaptures direkt fürs nächste Review!)
    6. *(Falls Gentoo-Host verfügbar)* `GENTOO_HIL_REPO=/tmp/gentoo-hil cargo test --release --test hil_regression -- --nocapture` + HIL-Screencaptures prüfen.

---

**Hinweise zur nächsten Ausführung:**
Wenn du den Agenten startest, gib ihm genau diesen Plan + den Hinweis mit, in seinem Abschluss-Walkthrough neben den Test- und Performance-Zahlen auch auf das erzeugte `screencaps_after.md` zu verweisen — dann kannst du im nächsten Review direkt die fertigen Text-Screenshots prüfen, ohne sie manuell abfotografieren zu müssen.
