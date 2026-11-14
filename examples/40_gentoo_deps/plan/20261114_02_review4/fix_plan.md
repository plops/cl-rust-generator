# Ausführungsplan für AI-Agent: `gentoo-dep-tui` (Review-Fixes, Entschlackung, Source-Build-/Cut-Workflow & JSON-CLI)

## 0. Arbeitsumgebung, Werkzeuge & Systemtest-Setup (verbindlich)

* **Umgebung**: Du arbeitest innerhalb eines Docker-Containers.
* **Dependencies nachschlagen**: Nutze den **DeepWiki MCP** für Fragen zu externen Rust-Crates (`portage-atom`, `portage-metadata`, `ratatui`, `crossterm`, `rayon`, `clap`, `serde_json`), falls API-Details unklar sind.
* **Gentoo-Host für System-/HIL-Tests via SSH**:
  * Auf dem Host läuft ein Gentoo-System, das mit `/workspace/src/cl-py-generator/example/110_gentoo/openrc/Dockerfile` gebaut wurde.
  * Lies dieses Dockerfile bei Bedarf vorab, um Pfade, Profil (`default/linux/amd64/23.0/...`), Portage-Setup und SSH-Konfiguration zu verstehen.
  * Verbinde dich als Benutzer `kiel` mit dem Private Key `cl-rust-generator/id_lenovo_to_hpz6` (achte auf `chmod 600` bzw. korrekte Key-Rechte und `StrictHostKeyChecking=accept-new`, Host-Adresse aus Umgebung/SSH-Config/Netzwerk ermitteln):
    ```bash
    ssh -i cl-rust-generator/id_lenovo_to_hpz6 kiel@<HOST> "..."
    ```
  * Führe auf dem Host **echte Systemtests** mit dem gebauten Release-Binary (oder einem dorthin kopierten Binary / Snapshot) gegen `/var/db/repos/gentoo`, `/var/lib/portage/world` und `/etc/portage` aus (ohne Schreibzugriff auf `/etc/portage`).

---

## 1. Architektur-Leitplanken

* **Schlanke Modul-Pipeline (`00`–`14`) beibehalten**: Keine neuen Abstraktionsebenen, keine Traits um der Traits willen.
* **Flüssige TUI bewahren**:
  * Navigation/Filter (`j`/`k`, `/`) dürfen **nie** einen Graph-Resolve auslösen.
  * Cursor-Bewegungen in der Flag-Liste dürfen schnelle Tastendrücke nicht durch synchrone Multi-Resolves blockieren.
  * Performance-Regressionen mit `cargo test --release --features perf-metrics --test perf_system -- --nocapture` sowie gegen den echten Gentoo-Host prüfen.
* **Source-Build als Erstklasse-Metrik**: Alle Ausgaben (`deltas`, `big-levers`, `cut`, `graph`, TUI) müssen **auch ohne `--binhost`** volle Aussagekraft über Paketanzahl (`-pkg`), exklusive Abhängigkeiten und Quell-Download (`dl`) haben.

---

## 2. Phase 1 — Entschlackung (Komplexität entfernen)

Folgende Funktionen erzeugen viel Code und Komplexität ohne realen Nutzen im Workflow und sollen **entfernt bzw. radikal vereinfacht** werden:

1. **`plan.toml` / `[[choice]]` / `OrDecision`-Persistenz / `verify` entfernen**:
   * Entferne `src/09_plan_toml.rs` und `src/10_export_verify.rs` (inkl. `supplement_host_portage_config`, das ohnehin relative `make.profile`-Symlinks fehlerhaft kopiert).
   * Entferne `toml` aus `Cargo.toml` (behalte `serde` und ergänze `serde_json`).
   * Entferne die CLI-Subcommands `init`, `export`, `verify` sowie `--plan` aus `src/11_cli/args.rs` und `src/11_cli/commands.rs`.
   * Entferne `LeftEntry::PlanChoice`, `selected_choice`, `cycle_choice_option`, `sync_world_from_plan`, `plan_path` und die `Plan`-Felder aus `src/12_tui_state.rs`, `src/13_tui_view/*` und `src/14_tui_runner.rs`.
   * **Ersatz (minimal, ~30 Zeilen)**: Taste `[S]` in der TUI (und optional `--out-use <PFAD>` oder fester Pfad `/tmp/gentoo-dep-tui.package.use`) schreibt ausschließlich die vom Benutzer geänderten Flags im validen `/etc/portage/package.use`-Format (`cat/pkg flag -other`) und zeigt den Pfad in der Statuszeile an. `[E]` und `[V]` entfallen aus Footer und Hilfe.
   * *Hinweis*: `||`-Alternativen aus echten Ebuilds (`or_decisions` in `Engine` + `LeftEntry::OrDecision`) **beibehalten**, aber das Anzeige-Label in `left_label` kürzen (`|| <owner>: <erste_option>` statt der kompletten kommagetrennten Kandidatenliste).
2. **Toter Code / Ballast entfernen**:
   * `PkgId(pub u32)` in `src/01_types.rs` entfernen.
   * Ungenutztes `AppState::middle_scroll` in `src/12_tui_state.rs` entfernen.
   * In `src/04_binhost.rs`: `BinpkgEntry` auf `cpv: String` und `size: u64` reduzieren (oder `HashMap<String, u64>`). `use_flags`, `slot` und `rdepend` werden nirgends gelesen und kosten unnötig RAM + Parse-Zeit.

---

## 3. Phase 2 — Bugfixes & Performance-Fundament

1. **Leeres `Kette 1:` im Blame beheben (`src/06_graph/blame.rs`)**:
   * In `blame_dfs` bei `None`: `Self::close_chain` nur aufrufen, wenn `path.len() > 1`. In `close_chain` nur pushen, wenn `!links.is_empty()`.
   * Ergebnis: Welt-Pakete ohne Eltern zeigen sauber `(kein Pfad — Welt-Paket oder leer)`.
2. **`OnceLock`-Cache für Profil-Baseline-USE (`src/05_repo_index.rs` & `src/06_graph.rs`)**:
   * In den realen Messungen verbraucht `effective_use` >50 % der CPU-Zeit (bis zu >2 Mio. Aufrufe von `Version::parse` und `package_use_for`).
   * Füge `base_use: std::sync::OnceLock<HashSet<String>>` zu `PkgVersion` hinzu.
   * `Engine::effective_use(pkg)` liest `let base = pkg.base_use.get_or_init(|| self.profile.effective_defaults(&pkg.iuse_defaults, &pkg.cpn, &pkg.version_str, &pkg.slot));`.
   * Liegen für `&pkg.cpn` keine `self.overrides` vor (~99 % der Pakete), `base.clone()` direkt zurückgeben (Mask/Force sind in `effective_defaults` bereits angewendet). Nur bei vorhandenen Overrides klonen, Overrides anwenden und Mask/Force prüfen.
   * Achte darauf, dass `RepoIndex::load` und `test_helpers::PkgFixture` das Feld mit `OnceLock::new()` initialisieren.
3. **Binhost-Treffer vs. „lokal bauen“ sauber unterscheiden (`src/06_graph.rs`, `src/13_tui_view/middle.rs`)**:
   * `ResolvedNode` um `pub has_binpkg: bool` (`pkg.binhost_size.is_some()`) ergänzen.
   * In der Mitte der TUI: Wenn `--binhost` geladen ist (`engine.index.has_any_binhost()`) und `!node.has_binpkg`, zeige `Binpkg: kein Treffer (Source-Build)` statt `Binpkg: 0 B`.
   * Ohne `--binhost`: `Binpkg: n/a` und Fokus auf `Pkg-Quell-DL`.
4. **`shared.rs` „Transitive-Leaf-Bug“ beheben (`src/08_delta/shared.rs`)**:
   * **Problem**: `pulled_deps` läuft derzeit bis zu Basispaketen (`readline`, `ncurses`, `zlib`, `python`), sodass `mupdf[-javascript]` oder `emacs[-gtk]` unbeteiligte Welt-Pakete wie `sudo`, `docker`, `bash` als `world_blockers` meldet.
   * **Fix**:
     1) Starte bei den **direkten** Flag-Abhängigkeiten von `package` (Rückwärtskanten `(package, Some(flag))`).
     2) Behalte nur direkte Abhängigkeiten, die in `outcome_alone` **überleben** (was durch den Toggle allein schon entfernt wird — wie `dev-lang/mujs` — hat keine Blocker und darf nicht weiterverfolgt werden!).
     3) Falls ein direktes Kind überlebt (z. B. `x11-libs/gtk+`), suche Blocker **ab diesem überlebenden Kind** aufwärts (`collect_blockers`), statt durch dessen eigene Dependencies bis `zlib`/`readline` abzusteigen.
     4) *Optional präzise*: Nur wenn ein direktes Kind bereits wegen eines *anderen* Flags/Pakets verschwindet, prüfe keine fremden OS-Basispakete darunter.
5. **Rechte Pane & CLI-Konflikttransparenz (`src/13_tui_view/right.rs`, `src/11_cli/commands.rs`)**:
   * Überschrift bei `FlagDiff`: Wenn `delta.current == true`, schreibe `"Ausschalten von {} bewirkt:"`, bei `false`: `"Einschalten von {} bewirkt:"`.
   * In `render_shared_impact`: `fmt_delta` statt `fmt_bytes` nutzen (korrektes `+`-Vorzeichen).
   * Wenn das in der Mitte selektierte Flag `delta.conflict = Some(c)` trägt, zeige in der rechten Pane sofort `Toggle-Konflikt: {c.message}` (und `Verursacher`) — nicht erst nach `Space` im Modal.
   * In `cmd_deltas`: bei `CONFLICT` optional kurz den Grund (`c.message`) ausgeben.

---

## 4. Phase 3 — Neue Features für Source-Builds & `ncdu`-Workflow

### 4.1 Paket-Eliminierungs-Analyse (`cut <cat/pkg>` in CLI + TUI `[x]` / `[a]` / `[u]`)
Beantwortet direkt Fragen wie: *„Wie werfe ich `x11-libs/gtk+` (oder `sys-devel/llvm`, `dev-lang/rust`) raus, und wie viele Pakete / MB spare ich dadurch?“*

* **Engine (`src/08_delta/cut.rs` oder in `08_delta/shared.rs`)**:
  * Neue Funktion `cut_package(engine: &Engine<'_>, world: &[String], base: &CascadeOutcome, target: &str) -> EngineResult<CutPlan>`.
  * `CutPlan` enthält:
    * `target: String`
    * `flag_blockers: Vec<(String, String)>` — alle `(cpn, flag)`-Paare, deren Deaktivierung den Haltepfad zu `target` bricht (aufwärts über `reverse` suchen: bei `(parent, Some(f))` stoppen und `(parent, f)` sammeln — außer `f` ist profil-erzwungen, dann weiter aufsteigen; bei `(parent, None)` weiter aufsteigen bis zu `world`).
    * `or_blockers: Vec<(String, usize, String)>` — falls ein `||`-Knoten aktuell eine Option wählt, die `target` hält, obwohl eine andere gültige Option existiert.
    * `world_blockers: Vec<String>` — `world`-Pakete, die `target` unbedingt halten (z. B. `firefox-bin`).
    * `without_flag_blockers`: exakte simulierte Ersparnis (`binpkg_bytes`, `download_bytes`, `removed_packages`, `added_packages`, `conflicts`), wenn alle `flag_blockers` abgeschaltet werden.
    * `without_all_blockers`: exakte simulierte Ersparnis, wenn zusätzlich `world_blockers` aus `world` entfernt werden.
* **CLI**: Neues Subcommand `gentoo-dep-tui cut <cat/pkg>` (mit `--json`-Support).
* **TUI**:
  * In Tab 1 (linke Pane auf beliebigem Paket, z. B. `x11-libs/gtk+`): Taste **`x`** berechnet `CutPlan` für das selektierte Paket und zeigt ihn übersichtlich in der rechten Pane (`Flag aus bei:`, `Harte world-Blocker:`, `Ersparnis ohne Flag-Blocker: -N Pakete / -X MB dl`, `Ersparnis inkl. world-Entfernung`).
  * Taste **`a`** (nur aktiv, wenn ein `CutPlan` mit `flag_blockers` angezeigt wird): übernimmt alle `flag_blockers` (`false`) auf einmal in `engine.overrides` und ruft `refresh()` auf.
  * **Undo (`u`)**: Einfacher `undo_stack: Vec<UseOverrides>` (max. ~32 Einträge) in `AppState`; vor jedem Flag-Toggle, `a`-Apply oder Quick-Switch sichern, mit `u` zurückrollen.

### 4.2 `ncdu`-Größen & Sortierung für Pakete in Tab 1 (`[o]`)
Der Nutzer möchte neben der gewohnten Sortierung auch Pakete nach ihrer **Größe inkl. exklusiver Dependencies** sehen.

* **Berechnung auf `ResolvedGraph` (`src/06_graph.rs`)**:
  * Schneller Helper `ResolvedGraph::exclusive_costs(&self) -> HashMap<String, (usize, SizeMetrics)>` (einmal pro `refresh()` berechnet, <1 ms für ~600 Knoten ohne Kaskade):
    * Für jedes Paket `u` (oder insbesondere jedes `world`-Paket, und für transitive Pakete die Gesamtsumme ihres exklusiven Subgraphs unterhalb von `u` bzw. ohne andere Eltern): bestimme alle Knoten, die nach Entfernen von `u` von keinem `world`-Atom mehr erreichbar sind (einfaches BFS über `out_edges`).
    * Liefert je `world`-Paket: `(exclusive_pkg_count, SizeMetrics { binpkg_bytes, download_bytes })` (inkl. `u` selbst).
* **TUI-Integration (`src/12_tui_state.rs`, `src/13_tui_view.rs`)**:
  * Taste **`o`** in Tab 1 wechselt den Sortiermodus der linken Liste:
    1. `Standard` (World → `||` → Transitive, wie bisher)
    2. `Exkl. Pakete` (absteigend nach exklusiver Paketanzahl — zeigt sofort die größten Abhängigkeits-Treiber in `world`)
    3. `Exkl. Größe` (absteigend nach `binpkg_bytes` falls `--binhost`, sonst nach `download_bytes`)
  * In der linken Liste bei nicht-Standard-Sortierung (oder kompakt dahinter, wenn Platz ist) die exklusive Zahl anzeigen, z. B. `app-editors/emacs (14 pkg, 55 MB dl)`.

### 4.3 `Big Levers` verbessern (`src/08_delta.rs`, `src/13_tui_view/bottom.rs`, `src/11_cli/commands.rs`)
* `BigLever` erweitern um:
  * `pub turn_on: bool` (ob global ab- oder angeschaltet wurde)
  * `pub net_pkg_delta: i64` (`added_packages.len() as i64 - removed_packages.len() as i64`)
* **Standard-Fokus**: In `big_levers` standardmäßig nur aktive Flags global **abschalten** (`turn_on = false`), da das Ziel ein minimales System ist (optional `--include-enable` in CLI, falls jemand Einschalten testen will — deutlich mit `[AUS]`/`[AN]` kennzeichnen).
* **Sortierung ohne `--binhost`**:
  * Wenn `has_any_binhost()` wahr ist: sortieren nach `(binpkg_bytes, net_pkg_delta, download_bytes)`.
  * Wenn **kein `--binhost`** übergeben wurde: sortieren nach **`(net_pkg_delta, download_bytes, flag)`**, sodass die größten Paket-Einsparungen (und danach Download-Einsparungen) oben stehen statt alles bei `0 B`!
* **Tab 2 / CLI-Anzeige**: Spalte `Pkg-Delta` (`-12`, `+2`, `0`) ergänzen und in `Hebel-Details` unten die `removed_packages` / `added_packages` + Flag-Beschreibung klar anzeigen.

### 4.4 Portage-Konfiguration komfortabler laden (`src/11_cli/args.rs`, `src/11_cli/context.rs`, `src/02_profile.rs`)
* Damit der Nutzer nicht jede Datei einzeln übergeben muss:
  * Neues Flag `--portage-dir <PFAD>` (Default `/etc/portage`): Wenn übergeben, leiten sich `make.conf`, `package.use`, `package.accept_keywords`, `package.mask`, `package.unmask` und `make.profile` standardmäßig daraus ab (explizite `--make-conf`/`--package-use`-Flags überschreiben weiterhin).
  * Neues Flag `--snapshot <PFAD>` (optional, z. B. für `/tmp/gentoo-hil`): sucht automatisch `<PFAD>/repo`, `<PFAD>/world`, `<PFAD>/etc/portage` bzw. `<PFAD>/make.conf` + `<PFAD>/package.use`.
  * Zusätzlich `/etc/portage/package.accept_keywords` (Datei oder Verzeichnis), `package.mask` und `package.unmask` aus `--portage-dir` einlesen und beim `RepoIndex::load` berücksichtigen (so stimmen ausgewählte Versionen/Keywords noch genauer mit `emerge` überein).
  * Auch `make.conf` um `PORTAGE_BINHOST` oder Warnhinweis ergänzen? Nicht nötig (kein Netzwerk im Tool); lokale `--binhost`-Option bleibt genau wie bisher.

### 4.5 TUI-Idle-Verhalten glätten (`src/14_tui_runner.rs`)
* `ensure_shared_impact()` nicht sofort bei jedem einzelnen Tastendruck-Poll synchron abfeuern, sondern erst, wenn kein weiteres Tastatur-Event in der Queue ansteht (oder nur dann neu rechnen, wenn sich `(selected_package, selected_flag)` nach einer kurzen Idle-Pause ~80 ms beruhigt hat). Dadurch bleibt schnelles `j`/`k`-Scrollen durch lange Flag-Listen komplett flüssig.

---

## 5. Phase 4 — AI-Agent-Schnittstelle (`--json` & `simulate` / `batch`)

1. **`--json` Flag in `CommonArgs` (oder je Lese-Subcommand)** mit `serde_json`:
   * Unterstützt für: `index-check`, `graph`, `blame`, `deltas`, `cut`, `big-levers`, `simulate`.
   * **`graph --json`**: enthält `totals` (`packages`, `binpkg_bytes`, `download_bytes`, `has_binhost`, `missing_binhost_count`, `build_only`), `nodes` (inkl. `cpv`, `slot`, `binpkg`, `download`, `has_binpkg`, `use_flags`, `exclusive_packages`, `exclusive_download_bytes`, `exclusive_binpkg_bytes`), `conflicts`, `skipped`.
   * **`deltas --json`**: enthält `package`, `flags` (jeweils `flag`, `current`, `delta_binpkg`, `delta_download`, `added_packages`, `removed_packages`, `conflict` mit `message` + `options`, `description`).
   * **`cut --json`**: serialisiert `CutPlan` vollständig.
   * **`big-levers --json`**: serialisiert `BigLever` inkl. `turn_on`, `net_pkg_delta`, `affected_packages`, `added_packages`, `removed_packages`.
2. **`simulate` und `batch` Subcommands (`src/11_cli/`)**:
   * `simulate --set cat/pkg=flag1,-flag2 --drop cat/pkg2 --json`: lädt Index einmal, wendet Overrides/Drops an, löst Kaskaden auf und gibt das Ergebnis (`totals`, `diff_vs_baseline`, `conflicts`, `added_packages`, `removed_packages`) als JSON aus.
   * `batch`: lädt `RepoIndex` + `Profile` **einmal** (~500 ms) und liest zeilenweise JSON-Anfragen von `stdin` (z. B. `{"op": "simulate", "set": {"app-editors/emacs": ["gui", "Xaw3d", "-gtk"]}, "drop": []}`, `{"op": "cut", "package": "x11-libs/gtk+"}`, `{"op": "deltas", "package": "media-video/ffmpeg"}`), schreibt pro Zeile eine JSON-Antwort nach `stdout`. Dadurch kann ein AI-Agent Dutzende Flag-Kombinationen in Sekundenbruchteilen ohne erneutes `index_load` testen.

---

## 6. Phase 5 — Verifikation & Systemtests

1. **Lokale Unit- & Integrationstests**:
   * `cargo clippy --all-targets` ohne Warnungen.
   * `cargo test` (alle Unit-Tests, `host_integration`, `tui_screencaps`, `perf_system`).
   * `tests/tui_screencaps.rs` an die entfernten `plan.toml`/`choice`-Teile anpassen und um Tests für **`[x]` Cut-Analyse**, **`[o]` Sortierwechsel**, **Toggle-Konflikt-Anzeige in der rechten Pane** und **sauberen leeren Blame** erweitern.
   * `tests/perf_system.rs` mit `--features perf-metrics` ausführen und prüfen, dass `effective_use_calls` bzw. die Laufzeit deutlich gesunken ist.
2. **Systemtests auf dem echten Gentoo-Host via SSH**:
   * Binary bauen (oder auf dem Host bauen/ausführen) mit `ssh -i cl-rust-generator/id_lenovo_to_hpz6 kiel@<HOST> ...`.
   * Auf dem Host prüfen:
     1. `gentoo-dep-tui graph --json` gegen reales `/var/db/repos/gentoo` + `/var/lib/portage/world` + `/etc/portage` (ohne `--binhost` und mit `/tmp/Packages`).
     2. `gentoo-dep-tui cut x11-libs/gtk+ --json` → verifizieren, dass **keine** unbeteiligten Basispaket-Blocker (`sudo`, `docker`, `bash`) mehr auftauchen und exakt die echten `gtk+`-Halter (`emacs`, `wireshark`, `firefox-bin` etc.) inkl. realistischer Paket- und Download-Ersparnis ausgegeben werden.
     3. `gentoo-dep-tui simulate --set app-editors/emacs=gui,Xaw3d,-gtk --json` und `batch` mit mehreren Anfragen über `stdin` testen.
     4. `gentoo-dep-tui deltas media-video/ffmpeg --json` und `gentoo-dep-tui big-levers --json` prüfen (inkl. fehlendem `--binhost`).
     5. `GENTOO_HIL_REPO` / Screencaps gegen echten Snapshot laufen lassen und das Rendering prüfen (inkl. `gentoo-sources` ohne leere `Kette 1:`, `emacs` + `gtk`, `mupdf` mit Konfliktgrund).


# Detaillierter Plan mit Quelltext Beispielen


# Arbeitsauftrag: `gentoo-dep-tui` — Review-Fixes, Entschlackung, Cut-Analyse & JSON-CLI

## 0. Umgebung, Werkzeuge & Test-Setup (zuerst lesen!)

1. **Docker-Container & Dependencies**:
   - Du arbeitest in einem Docker-Container im Projektverzeichnis.
   - Falls du API-Details zu externen Rust-Crates nachschlagen musst (`portage-atom`, `portage-metadata`, `ratatui`, `crossterm`, `rayon`, `clap`, `serde_json`), nutze den **DeepWiki MCP**.
2. **Gentoo-Host für Systemtests via SSH**:
   - Auf dem Ziel-Host läuft ein Gentoo-System, das mit `/workspace/src/cl-py-generator/example/110_gentoo/openrc/Dockerfile` gebaut wurde. Lies dieses Dockerfile bei Bedarf, um Pfade, Profil und Systemaufbau zu verstehen.
   - Zugriff auf den Host erfolgt als Benutzer `kiel` mit dem SSH-Private-Key `cl-rust-generator/id_lenovo_to_hpz6`:
     ```bash
     chmod 600 cl-rust-generator/id_lenovo_to_hpz6
     ssh -i cl-rust-generator/id_lenovo_to_hpz6 -o StrictHostKeyChecking=accept-new kiel@<HOST> "..."
     ```
     (Hostnamen/IP aus `~/.ssh/config`, Umgebungsvariablen oder lokalem Netzwerk ermitteln.)
   - Führe **System-/HIL-Tests** auf dem Gentoo-Host read-only gegen `/var/db/repos/gentoo`, `/var/lib/portage/world` und `/etc/portage` aus (kopiere das gebaute `target/release/gentoo-dep-tui` auf den Host oder baue/teste dort in `/tmp`). Verändere **niemals** Dateien unter `/etc/portage` auf dem Host.

---

## 1. Feste Architektur-Vorgaben (strikt einhalten!)

- Behalte die bestehende flache Modul-Pipeline in `src/lib.rs` bei:
  `00_perf` → `01_types` → `02_profile` → `03_manifest` → `04_binhost` → `05_repo_index` → `06_graph` → `07_cascade` → `08_delta` → `11_cli` → `12_tui_state` → `13_tui_view` → `14_tui_runner`.
- **Keine neuen Abstraktionsschichten** (keine neuen Traits, kein ECS, keine globalen Mutexe für Business-Logik).
- **Flüssigkeit der TUI wahren**:
  - `j`/`k`/Filter `/` in der linken Pane dürfen **nie** `resolve` aufrufen.
  - `Right`-Pane-Nachberechnungen (`shared_removal_impact`) dürfen schnelles Scrollen durch Flags nicht blockieren.
  - Prüfe nach jeder größeren Änderung mit `cargo test --release --features perf-metrics --test perf_system -- --nocapture`, dass Performance und Zähler stimmen.

---

## 2. Phase 1 — Entschlackung (unnötige Komplexität entfernen)

Entferne ungenutzte `plan.toml`/`[[choice]]`/`verify`-Komplexität und toten Code:

1. **`Cargo.toml`**:
   - Entferne `toml = "0.8"`.
   - Füge `serde_json = "1"` hinzu (`serde` mit `derive` bleibt).
2. **Dateien löschen**:
   - Lösche `src/09_plan_toml.rs` und `src/10_export_verify.rs`.
   - Entferne `mod_09_plan_toml` und `mod_10_export_verify` aus `src/lib.rs`.
3. **CLI (`src/11_cli/args.rs`, `src/11_cli/commands.rs`)**:
   - Entferne die Subcommands `Init`, `Export`, `Verify` sowie `--plan` aus `TuiArgs`.
   - Behalte `||`-Entscheidungen (`Engine::or_decisions`) — die stammen direkt aus Ebuild-`RDEPEND`.
4. **TUI (`src/12_tui_state.rs`, `src/12_tui_state/actions.rs`, `src/13_tui_view/*`, `src/14_tui_runner.rs`)**:
   - Entferne `Plan`, `PlanChoice`, `LeftEntry::PlanChoice`, `selected_choice`, `cycle_choice_option`, `sync_world_from_plan`, `plan_path`, `middle_scroll` (ungenutzt), sowie `[E]`/`[V]`-Keybindings.
   - Ersetze `[S]` durch einen simplen `package.use`-Export (~25 Zeilen):
     Schreibt nur die in dieser TUI-Session gesetzten `engine.overrides` in Portage-Syntax (`cat/pkg flag1 -flag2`, sortiert) nach `--out-use` (Default: `/tmp/gentoo-dep-tui.package.use`) und setzt `state.status = format!("package.use geschrieben: {}", path.display())`.
   - Kürze `left_label` für `LeftEntry::OrDecision(group_id)` in `src/12_tui_state.rs`, sodass links kein endloser String steht:
     ```rust
     LeftEntry::OrDecision(group) => {
         let short = group.split("||").next().unwrap_or(group);
         format!("|| {short}")
     }
     ```
5. **Toter Code in Typen & Binhost**:
   - `src/01_types.rs`: `PkgId` löschen (wird nirgends genutzt). Auf allen Ergebnis-Typen (`SizeMetrics`, `SkippedDep`, `BlameLink`, `ConflictInfo`, `QuickSwitch`, `AlternativeChoice`, `FlagDelta`, `BigLever`) `#[derive(serde::Serialize)]` ergänzen.
   - `src/04_binhost.rs`: `BinpkgEntry` auf `cpv: String` und `size: u64` verschlanken (`use_flags`, `slot`, `rdepend` entfernen — spart RAM und Parse-Zeit).

---

## 3. Phase 2 — Konkrete Bugfixes & Performance-Fix

### 3.1 Bugfix: Leere `Kette 1:` im Blame (`src/06_graph/blame.rs`)
Aktuell wird bei `self.reverse.get(node) == None` auch bei `path.len() == 1` ein leeres `Vec<BlameLink>` in `chains` gepusht. Ändere `blame_dfs` und `close_chain`:

```rust
match self.reverse.get(node) {
    None => {
        if path.len() > 1 {
            Self::close_chain(self, path, chains);
        }
    }
    Some(parents) => {
        if world.contains(node) && path.len() > 1 {
            Self::close_chain(self, path, chains);
            return;
        }
        // ... unveraendert ...
    }
}
```
Und in `close_chain`:
```rust
if !links.is_empty() {
    chains.push(links);
}
```

### 3.2 Performance-Fix: `OnceLock`-Baseline-USE-Cache (`src/05_repo_index.rs`, `src/06_graph.rs`)
In den echten Profiling-Daten verbraucht `effective_use` >50 % der CPU-Zeit (2 Mio. Aufrufe von `Version::parse` und `package_use_for`).

1. In `src/05_repo_index.rs` in `PkgVersion` ergänzen:
```rust
pub struct PkgVersion {
    // ... bestehende Felder ...
    pub base_use: std::sync::OnceLock<HashSet<String>>,
}
```
Initialisiere `base_use: std::sync::OnceLock::new()` in `parse_cache_file` und in `src/test_helpers.rs` (`PkgFixture::build`).

2. In `src/06_graph.rs` in `Engine::effective_use`:
```rust
pub fn effective_use(&self, pkg: &PkgVersion) -> HashSet<String> {
    let base = pkg.base_use.get_or_init(|| {
        self.profile.effective_defaults(
            &pkg.iuse_defaults,
            &pkg.cpn,
            &pkg.version_str,
            &pkg.slot,
        )
    });
    let Some(explicit) = self.overrides.get(&pkg.cpn) else {
        return base.clone();
    };
    let mut active = base.clone();
    for (flag, on) in explicit {
        if *on {
            active.insert(flag.clone());
        } else {
            active.remove(flag);
        }
    }
    active.retain(|f| !self.profile.use_mask.contains(f));
    for flag in &self.profile.use_force {
        if pkg.iuse_defaults.contains_key(flag) {
            active.insert(flag.clone());
        }
    }
    active
}
```

### 3.3 Bugfix: Binhost-Miss vs. `0 B` (`src/06_graph.rs`, `src/06_graph/resolve.rs`, `src/13_tui_view/middle.rs`)
1. Ergänze `ResolvedNode` in `src/06_graph.rs` um:
   ```rust
   pub has_binpkg: bool,
   ```
   Setze in `resolve.rs`: `has_binpkg: pkg.binhost_size.is_some()`.
2. In `src/13_tui_view/middle.rs` bei der Paket-Kopfzeile:
   ```rust
   let bin_str = if !state.engine.index.has_any_binhost() {
       "n/a".to_string()
   } else if node.has_binpkg {
       fmt_bytes(node.binpkg as i64)
   } else {
       "kein Binpkg (Source-Build)".to_string()
   };
   ```

### 3.4 Bugfix: Rechte Pane (Toggle-Richtung, `+`-Vorzeichen, Toggle-Konflikt) (`src/13_tui_view/right.rs`, `src/11_cli/commands.rs`)
1. In `src/13_tui_view/bottom.rs`: mache `pub(super) fn fmt_delta(bytes: i64, unit: &str) -> String` für `right.rs` nutzbar.
2. In `src/13_tui_view/right.rs`:
   - Bei `render_shared_impact`: `fmt_delta` statt `fmt_bytes` verwenden, damit steigendes `dl` ein `+` erhält.
   - Überschrift beim Paket-Diff des selektierten Flags:
     ```rust
     let action = if delta.current { "Ausschalten von" } else { "Einschalten von" };
     lines.push(Line::styled(
         format!("{action} {} bewirkt:", delta.flag),
         Style::default().add_modifier(Modifier::BOLD),
     ));
     ```
   - Falls `delta.conflict` `Some(c)` ist, direkt in der rechten Pane anzeigen (nicht erst im Modal):
     ```rust
     if let Some(c) = &delta.conflict {
         lines.push(Line::from(""));
         lines.push(Line::styled(
             format!("Toggle-Konflikt ({}):", delta.flag),
             Style::default().add_modifier(Modifier::BOLD).fg(Color::Red),
         ));
         lines.push(Line::styled(format!("! {}", c.message), Style::default().fg(Color::Red)));
         for opt in c.options.iter().take(3) {
             lines.push(Line::from(format!("  Loesung: {}", opt.label)));
         }
     }
     ```
3. In `src/11_cli/commands.rs` (`cmd_deltas`): bei `d.conflict` auch `c.message` ausgeben: `CONFLICT ({})`.

### 3.5 Bugfix: `shared.rs` Transitive-Leaf-Bug + `!flag`-Bug (`src/08_delta/shared.rs`)
Im bisherigen `shared.rs` steckte ein doppelter Bug:
1. `pulled_deps` lief durch *entfernte* Zwischenknoten hindurch bis zu überlebenden Basispaketen (`readline`, `zlib`, `ncurses`), wodurch `mupdf[-javascript]` fälschlich `sudo`, `docker`, `bash` als Blocker meldete.
2. Bei `!minimal` (`via_flag == Some("!minimal")`) wurde `"!minimal" => false` in `overrides` eingetragen statt `"minimal" => true`!

Ersetze `src/08_delta/shared.rs` komplett durch die gemeinsame, exakte Blocker- und Cut-Analyse (wird sowohl von `shared_removal_impact` als auch von `cut_package` genutzt):

```rust
use std::collections::{BTreeSet, HashSet};
use serde::Serialize;

use crate::mod_01_types::{EngineResult, SizeMetrics, atom_cpn};
use crate::mod_06_graph::{Engine, ResolvedGraph};
use crate::mod_07_cascade::{CascadeOutcome, resolve_with_cascade_opts};
use crate::mod_08_delta::{is_hidden_expand_flag, is_platform_flag};

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct RemovalSavings {
    pub metrics: SizeMetrics,
    pub removed_packages: Vec<String>,
    pub added_packages: Vec<String>,
    pub conflicts: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct FlagBlocker {
    pub package: String,
    pub flag: String,
    /// `true` = einschalten (bei `!flag?`), `false` = abschalten (bei `flag?`).
    pub want_on: bool,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct CutPlan {
    pub target: String,
    pub flag_blockers: Vec<FlagBlocker>,
    pub world_blockers: Vec<String>,
    pub without_flag_blockers: RemovalSavings,
    pub without_all_blockers: RemovalSavings,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
pub struct SharedRemovalImpact {
    pub package: String,
    pub flag: String,
    pub shared_deps: Vec<String>,
    pub flag_blockers: Vec<FlagBlocker>,
    pub world_blockers: Vec<String>,
    pub without_flag_blockers: RemovalSavings,
    pub without_all_blockers: RemovalSavings,
}

/// Parst ein `via_flag` (`"gtk"` oder `"!minimal"`) in `(flag_name, want_on_to_break)`
/// und prueft, ob das Flag auf `parent` ueberhaupt schaltbar ist.
fn parse_breakable_flag(
    engine: &Engine<'_>,
    graph: &ResolvedGraph,
    parent: &str,
    via: &str,
) -> Option<(String, bool)> {
    let (flag, want_on) = match via.strip_prefix('!') {
        Some(f) => (f, true),
        None => (via, false),
    };
    if is_platform_flag(engine.profile, flag) || is_hidden_expand_flag(engine.profile, flag) {
        return None;
    }
    if want_on && engine.profile.is_masked(flag) {
        return None;
    }
    if !want_on && engine.profile.is_forced(flag) {
        return None;
    }
    let node = graph.nodes.get(parent)?;
    let pkg = engine.index.select_cpv(parent, &node.cpv)?;
    if !pkg.iuse_defaults.contains_key(flag) {
        return None;
    }
    Some((flag.to_string(), want_on))
}

/// Steigt ab `dep` aufwaerts ueber `reverse`-Kanten bis zu schaltbaren
/// Flag-Blockern oder `world`-Blockern (ignoriert optional `skip_pkg`).
fn collect_blockers_up(
    graph: &ResolvedGraph,
    engine: &Engine<'_>,
    world_cpns: &HashSet<String>,
    skip_pkg: Option<&str>,
    dep: &str,
    flag_blockers: &mut BTreeSet<FlagBlocker>,
    world_blockers: &mut BTreeSet<String>,
    visited: &mut HashSet<String>,
) {
    if !visited.insert(dep.to_string()) {
        return;
    }
    if skip_pkg != Some(dep) && world_cpns.contains(dep) {
        // Auch wenn `dep` in world steht, weiter aufsteigen, falls andere
        // Pakete `dep` ebenfalls halten.
        world_blockers.insert(dep.to_string());
    }
    let Some(parents) = graph.reverse.get(dep) else {
        return;
    };
    for (parent, via) in parents {
        if skip_pkg == Some(parent.as_str()) {
            continue;
        }
        if let Some(via_str) = via
            && let Some((flag, want_on)) = parse_breakable_flag(engine, graph, parent, via_str)
        {
            flag_blockers.insert(FlagBlocker {
                package: parent.clone(),
                flag,
                want_on,
            });
            continue;
        }
        // Unbedingt oder nicht schaltbares Profil-Flag: weiter aufsteigen.
        collect_blockers_up(
            graph,
            engine,
            world_cpns,
            skip_pkg,
            parent,
            flag_blockers,
            world_blockers,
            visited,
        );
    }
}

/// Paket-Eliminierungs-Analyse: Wie bekommt man `target` (z. B. `x11-libs/gtk+`)
/// aus dem Graphen?
pub fn cut_package(
    engine: &Engine<'_>,
    world: &[String],
    base: &CascadeOutcome,
    target: &str,
) -> EngineResult<Option<CutPlan>> {
    if !base.graph.nodes.contains_key(target) {
        return Ok(None);
    }
    let world_cpns: HashSet<String> = world.iter().map(|w| atom_cpn(w)).collect();
    let mut flag_blockers = BTreeSet::new();
    let mut world_blockers = BTreeSet::new();
    let mut visited = HashSet::new();
    collect_blockers_up(
        &base.graph,
        engine,
        &world_cpns,
        None,
        target,
        &mut flag_blockers,
        &mut world_blockers,
        &mut visited,
    );
    let flag_blockers: Vec<FlagBlocker> = flag_blockers.into_iter().collect();
    let world_blockers: Vec<String> = world_blockers.into_iter().collect();

    let without_flag_blockers = simulate_cut(engine, world, &base.graph, &flag_blockers, &[])?;
    let without_all_blockers =
        simulate_cut(engine, world, &base.graph, &flag_blockers, &world_blockers)?;

    Ok(Some(CutPlan {
        target: target.to_string(),
        flag_blockers,
        world_blockers,
        without_flag_blockers,
        without_all_blockers,
    }))
}

/// Shared-Blocker-Analyse fuer `package[flag]` (nur wenn Flag aktuell aktiv).
/// Prueft ausschliesslich **direkte** Flag-Kinder, die nach Toggle-Off im
/// Graphen ueberleben — steigt NICHT in entfernte Basispakete ab.
pub fn shared_removal_impact(
    engine: &Engine<'_>,
    world: &[String],
    base: &CascadeOutcome,
    package: &str,
    flag: &str,
) -> EngineResult<Option<SharedRemovalImpact>> {
    let Some(node) = base.graph.nodes.get(package) else {
        return Ok(None);
    };
    if !node.use_flags.contains(flag) {
        return Ok(None);
    }
    // 1. Allein abschalten simulieren.
    let mut alone = engine.fork();
    alone
        .overrides
        .entry(package.to_string())
        .or_default()
        .insert(flag.to_string(), false);
    let outcome_alone = resolve_with_cascade_opts(&mut alone, world, false, false)?;

    // 2. Nur DIREKTE Flag-Kinder, die in `outcome_alone` ueberleben
    //    und mindestens einen anderen Halter haben.
    let mut shared: Vec<String> = base
        .graph
        .reverse
        .iter()
        .filter(|(child, _)| child.as_str() != package)
        .filter(|(_, parents)| {
            parents
                .iter()
                .any(|(p, via)| p == package && via.as_deref() == Some(flag))
        })
        .filter(|(child, parents)| {
            outcome_alone.graph.nodes.contains_key(*child)
                && parents.iter().any(|(p, _)| p != package)
        })
        .map(|(child, _)| child.clone())
        .collect();
    shared.sort();
    if shared.is_empty() {
        return Ok(None);
    }

    let world_cpns: HashSet<String> = world.iter().map(|w| atom_cpn(w)).collect();
    let mut flag_blockers = BTreeSet::new();
    let mut world_blockers = BTreeSet::new();
    for dep in &shared {
        let mut visited = HashSet::new();
        collect_blockers_up(
            &base.graph,
            engine,
            &world_cpns,
            Some(package),
            dep,
            &mut flag_blockers,
            &mut world_blockers,
            &mut visited,
        );
    }
    let flag_blockers: Vec<FlagBlocker> = flag_blockers.into_iter().collect();
    let world_blockers: Vec<String> = world_blockers.into_iter().collect();

    let self_off = FlagBlocker {
        package: package.to_string(),
        flag: flag.to_string(),
        want_on: false,
    };
    let mut all_flags = vec![self_off];
    all_flags.extend(flag_blockers.iter().cloned());

    let without_flag_blockers = simulate_cut(engine, world, &base.graph, &all_flags, &[])?;
    let without_all_blockers =
        simulate_cut(engine, world, &base.graph, &all_flags, &world_blockers)?;

    Ok(Some(SharedRemovalImpact {
        package: package.to_string(),
        flag: flag.to_string(),
        shared_deps: shared,
        flag_blockers,
        world_blockers,
        without_flag_blockers,
        without_all_blockers,
    }))
}

fn simulate_cut(
    engine: &Engine<'_>,
    world: &[String],
    before: &ResolvedGraph,
    flags: &[FlagBlocker],
    drop_world: &[String],
) -> EngineResult<RemovalSavings> {
    let mut sim = engine.fork();
    for fb in flags {
        sim.overrides
            .entry(fb.package.clone())
            .or_default()
            .insert(fb.flag.clone(), fb.want_on);
    }
    let filtered_world: Vec<String> = if drop_world.is_empty() {
        world.to_vec()
    } else {
        world
            .iter()
            .filter(|w| !drop_world.contains(&atom_cpn(w)))
            .cloned()
            .collect()
    };
    let outcome = resolve_with_cascade_opts(&mut sim, &filtered_world, false, false)?;
    Ok(savings_vs(before, &outcome))
}

fn savings_vs(before: &ResolvedGraph, after: &CascadeOutcome) -> RemovalSavings {
    let before_totals = before.totals();
    let after_totals = after.graph.totals();
    let mut removed: Vec<String> = before
        .nodes
        .keys()
        .filter(|k| !after.graph.nodes.contains_key(*k))
        .cloned()
        .collect();
    let mut added: Vec<String> = after
        .graph
        .nodes
        .keys()
        .filter(|k| !before.nodes.contains_key(*k))
        .cloned()
        .collect();
    removed.sort();
    added.sort();
    RemovalSavings {
        metrics: SizeMetrics {
            binpkg_bytes: after_totals.binpkg_bytes - before_totals.binpkg_bytes,
            download_bytes: after_totals.download_bytes - before_totals.download_bytes,
        },
        removed_packages: removed,
        added_packages: added,
        conflicts: after.conflicts.len(),
    }
}
```
Passe `cmd_deltas_deep` (`11_cli/commands.rs`) und `render_shared_impact` (`13_tui_view/right.rs`) an `FlagBlocker` (`fb.want_on`: `flag` vs `-flag`) an.

---

## 4. Phase 3 — Features für Source-Builds, `cut` & `ncdu`-Sortierung

### 4.1 Exklusive Subgraph-Kosten je Paket (`src/06_graph.rs`)
Füge folgende Methode auf `ResolvedGraph` hinzu (reine Graphen-Erreichbarkeit über Integer-Indizes, < 1 ms ohne Kaskaden):

```rust
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, serde::Serialize)]
pub struct NodeCost {
    /// Anzahl Pakete (inkl. Paket selbst), die nicht mehr von `world`
    /// erreichbar sind, wenn dieses Paket aus dem Graphen faellt.
    pub exclusive_packages: usize,
    pub exclusive_metrics: SizeMetrics,
}

impl ResolvedGraph {
    pub fn node_costs(&self) -> HashMap<String, NodeCost> {
        let mut names: Vec<&String> = self.nodes.keys().collect();
        names.sort();
        let idx_of: HashMap<&str, usize> = names
            .iter()
            .enumerate()
            .map(|(i, s)| (s.as_str(), i))
            .collect();
        let n = names.len();
        let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n];
        for (i, name) in names.iter().enumerate() {
            if let Some(edges) = self.out_edges.get(name.as_str()) {
                for dep in edges {
                    if dep.blocker.is_none()
                        && let Some(&j) = idx_of.get(dep.cpn.to_string().as_str())
                    {
                        adj[i].push(j);
                    }
                }
            }
        }
        let world_idx: Vec<usize> = self
            .world
            .iter()
            .filter_map(|w| idx_of.get(crate::mod_01_types::atom_cpn(w).as_str()).copied())
            .collect();
        let mut seen = vec![0u32; n];
        let mut gen_id = 0u32;
        let mut queue = std::collections::VecDeque::new();
        let mut out = HashMap::with_capacity(n);

        for cut in 0..n {
            gen_id += 1;
            queue.clear();
            for &w in &world_idx {
                if w != cut && seen[w] != gen_id {
                    seen[w] = gen_id;
                    queue.push_back(w);
                }
            }
            while let Some(u) = queue.pop_front() {
                for &v in &adj[u] {
                    if v != cut && seen[v] != gen_id {
                        seen[v] = gen_id;
                        queue.push_back(v);
                    }
                }
            }
            let mut pkgs = 0usize;
            let mut metrics = SizeMetrics::zero();
            for (i, name) in names.iter().enumerate() {
                if seen[i] != gen_id {
                    let node = &self.nodes[name.as_str()];
                    pkgs += 1;
                    metrics.binpkg_bytes = metrics.binpkg_bytes.saturating_add(node.binpkg as i64);
                    metrics.download_bytes =
                        metrics.download_bytes.saturating_add(node.download as i64);
                }
            }
            out.insert(
                names[cut].clone(),
                NodeCost {
                    exclusive_packages: pkgs,
                    exclusive_metrics: metrics,
                },
            );
        }
        out
    }
}
```

### 4.2 `ncdu`-Sortierung `[o]`, `Cut` `[x]`/`[a]` & `Undo` `[u]` in der TUI (`12_tui_state`, `13_tui_view`, `14_tui_runner`)
1. In `AppState` ergänzen:
   ```rust
   #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
   pub enum LeftSort {
       #[default]
       Default,
       ExclusivePackages,
       ExclusiveSize,
   }
   ```
   Felder in `AppState`:
   - `pub node_costs: HashMap<String, crate::mod_06_graph::NodeCost>` (einmal pro `refresh()` via `self.graph.node_costs()` füllen)
   - `pub left_sort: LeftSort`
   - `pub cut_plan: Option<crate::mod_08_delta::CutPlan>` (bei Taste `x` on demand für `selected_package()` berechnet, bei `refresh()` zurückgesetzt)
   - `pub undo_stack: Vec<crate::mod_06_graph::UseOverrides>` (max. 32 Einträge)
2. **Sortierung `[o]`**:
   - Taste `o` schaltet `left_sort` zyklisch (`Default` → `ExclusivePackages` → `ExclusiveSize` → `Default`) und ruft `rebuild_left()` auf (kein Resolve!).
   - Bei `ExclusivePackages`: alle `WorldPkg` + `Transitive` absteigend nach `node_costs[cpn].exclusive_packages` (Tie-Break: `exclusive_metrics.download_bytes` absteigend, dann Name).
   - Bei `ExclusiveSize`: absteigend nach `binpkg_bytes` falls `has_any_binhost()`, sonst `download_bytes`.
   - In `render_left` (`src/13_tui_view.rs`): wenn `left_sort != LeftSort::Default`, zeige bei Paketen kompakt `(N pkg, X MB)` rechts vom Namen (oder falls Platz reicht, als zweite Spalte/Suffix).
3. **`Cut` `[x]` & `[a]`**:
   - Taste `x` auf `LeftEntry::WorldPkg(cpn)` oder `LeftEntry::Transitive(cpn)`: berechnet `cut_package(&self.engine, &self.world, base, &cpn)` und speichert in `state.cut_plan`, setzt `right_scroll = 0`.
   - `render_right` zeigt `state.cut_plan` (falls vorhanden und `target == selected_package`):
     - `Cut-Analyse fuer <target>:`
     - `Flag-Blocker (Taste [a] wendet alle an):` mit `<pkg> [+flag]` oder `<pkg> [-flag]`
     - `Harte world-Blocker:`
     - `Ohne Flag-Blocker: <bin> bin / <dl> dl (-N/+M pkg, K Konflikte)`
     - `Ohne alle Blocker: <bin> bin / <dl> dl (-N/+M pkg)`
   - Taste `a`: falls `cut_plan` nicht-leere `flag_blockers` hat, `push_undo()`, alle `flag_blockers` in `engine.overrides` setzen und `refresh()`.
4. **`Undo` `[u]`**:
   - Vor jeder Mutation (`toggle_selected_flag`, `toggle_big_lever`, `apply_quick_switch`, `apply_cut_blockers`) `self.undo_stack.push(self.engine.overrides.clone())` (auf max. 32 kappen).
   - Taste `u`: `pop()`, in `self.engine.overrides` zurückschreiben, `refresh()`.
5. **TUI-Idle-Entprellung (`src/14_tui_runner.rs`)**:
   - In `event_loop`: führe `state.ensure_deltas()?` nur aus, wenn `event::poll(timeout)?` `false` liefert (d. h. nach Idle-Pause), und erhöhe `timeout` auf `80 ms`, solange kein `deltas_loading` ansteht, sodass schnelles `j`/`k`-Tippen sofort ohne Hänger durchgeht.

### 4.3 `Big Levers` klarer & Source-Build-tauglich (`src/08_delta.rs`, `src/11_cli`, `src/13_tui_view/bottom.rs`)
1. `BigLever` erweitern:
   ```rust
   pub struct BigLever {
       pub flag: String,
       pub turn_on: bool,
       pub packages: usize,
       pub affected_packages: Vec<String>,
       pub total: SizeMetrics,
       pub net_pkg_delta: i64,
       pub conflicts: usize,
       pub added_packages: Vec<String>,
       pub removed_packages: Vec<String>,
   }
   ```
   (`net_pkg_delta = added_packages.len() as i64 - removed_packages.len() as i64`).
2. `big_levers_with_opts(engine, world, base, include_enable: bool)`:
   - Standardmäßig (`include_enable == false`) in TUI und CLI nur `turn_on = false` (globales Abschalten aktiver Flags — dadurch verschwinden verwirrende Einträge wie `kde +132 MB` aus der Spar-Liste). Mit `--include-enable` in CLI optional einschaltbar.
   - Sortierung:
     ```rust
     let has_bin = engine.index.has_any_binhost();
     levers.sort_by(|a, b| {
         if has_bin {
             a.total.binpkg_bytes.cmp(&b.total.binpkg_bytes)
                 .then_with(|| a.net_pkg_delta.cmp(&b.net_pkg_delta))
         } else {
             a.net_pkg_delta.cmp(&b.net_pkg_delta)
         }
         .then_with(|| a.total.download_bytes.cmp(&b.total.download_bytes))
         .then_with(|| a.flag.cmp(&b.flag))
     });
     ```
3. In Tab 2 (`render_lever_table` in `13_tui_view/bottom.rs`) und `cmd_big_levers`: Spalte `Pkg-Delta` (`{:+}`) ergänzen.

### 4.4 Einfachere Portage-Konfiguration (`src/11_cli/args.rs`, `src/11_cli/context.rs`, `src/02_profile.rs`, `src/05_repo_index.rs`)
1. In `CommonArgs`:
   - Neues Flag `--portage-dir <PFAD>`: Falls gesetzt (Default `/etc/portage`), werden nicht explizit gesetzte Pfade automatisch als `<portage_dir>/make.conf`, `<portage_dir>/package.use`, `<portage_dir>/package.mask`, `<portage_dir>/package.unmask`, `<portage_dir>/package.accept_keywords`, `<portage_dir>/make.profile` aufgelöst. (`--make-conf` und `--package-use` als `Option<PathBuf>` definieren, damit erkennbar ist, ob sie explizit übergeben wurden!)
   - Neues Flag `--snapshot <PFAD>`: falls gesetzt, setzt Standardpfade falls nicht überschrieben auf `<snapshot>/repo`, `<snapshot>/world`, und `<snapshot>/etc/portage` (oder `<snapshot>/make.conf` + `<snapshot>/package.use`).
   - Neues Flag `--out-use <PFAD>` in `TuiArgs` (Default `/tmp/gentoo-dep-tui.package.use`).
2. `package.mask`, `package.unmask`, `package.accept_keywords` aus `portage_dir` lesen (`src/02_profile.rs` & `05_repo_index.rs`):
   - `read_atom_lines(path)` (Datei oder sortiertes Verzeichnis, `#`-Kommentare raus):
     - `package.mask` an `profile.package_mask` anhängen.
     - `package.unmask` als `format!("-{line}")` an `profile.package_mask` anhängen (nutzt die bestehende `-atom`-Entmaskierlogik in `sync_pattern_caches`).
   - `package.accept_keywords`:
     In `Profile` ergänzen:
     ```rust
     pub package_accept_keywords_raw: Vec<String>,
     pub package_accept_keywords: HashMap<String, Vec<(Option<Dep>, HashSet<String>)>>,
     ```
     In `sync_pattern_caches()`: jede Zeile in `atom` + optionale Keywords zerlegen; sind keine Keywords auf der Zeile genannt, gilt `~{arch}` (Portage-Standard). In `05_repo_index.rs` `keywords_accepted`: zusätzlich prüfen, ob eine Regel in `profile.package_accept_keywords.get(cpn)` auf Version/Slot matcht und eines der Ebuild-Keywords (oder `**` / `*`) abdeckt.

---

## 5. Phase 4 — AI-Agent-Schnittstelle (`--json`, `simulate`, `batch`)

1. **`--json` Flag in `CommonArgs`**:
   - Unterstützt von: `index-check`, `graph`, `blame`, `deltas`, `cut`, `big-levers`, `simulate`.
   - **Wichtig**: Wenn `--json` aktiv ist, schreibe `--perf-report` nach `stderr` (oder nur ins JSON), niemals unformatierten Text auf `stdout`!
   - `graph --json`: Struktur mit `totals` (`packages`, `binpkg_bytes`, `download_bytes`, `has_binhost`, `missing_binhost_count`, `build_extra`), `nodes` (inkl. `cpn`, `cpv`, `slot`, `binpkg`, `download`, `has_binpkg`, `use_flags`, `exclusive_packages`, `exclusive_metrics`), `conflicts`, `skipped`.
   - `deltas --json`: `{ package, has_binhost, flags: Vec<FlagDelta> }` (optional + `descriptions` Map).
   - `cut <cat/pkg> --json`: `CutPlan` als JSON.
2. **`simulate` und `batch` Subcommands (`src/11_cli/`)**:
   - `SimulateArgs`:
     - `--set <ATOM=FLAG1,-FLAG2>` (mehrfach angebbar)
     - `--drop <CAT/PKG>` (mehrfach angebbar)
   - `Batch`: liest zeilenweise JSON von `stdin`, schreibt eine JSON-Antwort pro Zeile auf `stdout` (Index + Profil werden **einmal** geladen!):
     ```rust
     #[derive(serde::Deserialize)]
     #[serde(tag = "op", rename_all = "snake_case")]
     enum BatchReq {
         Graph,
         Deltas { package: String },
         Cut { package: String },
         BigLevers { #[serde(default)] include_enable: bool },
         Simulate {
             #[serde(default)]
             set: HashMap<String, Vec<String>>,
             #[serde(default)]
             drop: Vec<String>,
         },
     }
     ```
     Antworten via `serde_json::to_writer` + `\n` und `flush()` nach jeder Zeile. Fehler in einer Zeile als `{"error": "..."}` ausgeben (Batch läuft weiter).

---

## 6. Phase 5 — Verifikation & Systemtests (Checkliste)

1. **Tests aktualisieren & ausbauen**:
   - Unit-Tests in `06_graph` (`blame` für Welt-Pakete ohne leere Kette, `node_costs` auf Diamond-Graph), `08_delta/shared.rs` (`mupdf`-Fall ohne falsche OS-Blocker, `gtk+` mit echten Haltern, `!flag`-Behandlung), `02_profile` (`package.accept_keywords`, `package.unmask`).
   - `tests/host_integration.rs` und `tests/tui_screencaps.rs` auf entfernte `plan.toml`-Typen anpassen und Screencaps/Assertions ergänzen für:
     - `x` Cut-Analyse + `a` Apply + `u` Undo
     - `o` Sortierung (`Exkl. Pakete` / `Exkl. Größe`)
     - Toggle-Konflikt-Anzeige direkt in der rechten Pane
     - Saubere leere Blame-Anzeige (kein `Kette 1:` ohne Inhalt)
2. **Lokal ausführen**:
   ```bash
   cargo clippy --all-targets -- -D warnings
   cargo test
   cargo test --release --features perf-metrics --test perf_system -- --nocapture
   cargo test --test tui_screencaps
   ```
3. **Echter Systemtest auf dem Gentoo-Host via SSH (`kiel@<HOST>` mit `cl-rust-generator/id_lenovo_to_hpz6`)**:
   - Teste gegen die realen Gentoo-Daten (`/var/db/repos/gentoo`, `/var/lib/portage/world`, `/etc/portage`):
     1. `gentoo-dep-tui graph --json` (ohne `--binhost` und mit `/tmp/Packages`, falls vorhanden).
     2. `gentoo-dep-tui cut x11-libs/gtk+ --json` → verifizieren, dass nur echte `gtk+`-Blocker (`emacs`, `wireshark`, `firefox-bin` etc.) erscheinen und **keine** Basispakete wie `sudo` oder `docker`.
     3. `gentoo-dep-tui deltas media-video/ffmpeg --json` → prüfen, dass `opencolorio` den exakten Konfliktgrund liefert.
     4. `gentoo-dep-tui simulate --set app-editors/emacs=gui,Xaw3d,-gtk --json` und `gentoo-dep-tui batch` mit mehreren JSON-Zeilen testen.
     5. `gentoo-dep-tui big-levers --json` (ohne `--binhost` prüfen, dass sauber nach Paket- und DL-Ersparnis sortiert wird).
     6. `GENTOO_HIL_REPO=... cargo test --release --test tui_screencaps` bzw. `hil_regression` ausführen, falls Snapshot vorhanden.
   - Dokumentiere am Ende die gemessenen Zeiten (`--perf-report`) und das konkrete Ergebnis für `cut x11-libs/gtk+` + `emacs[gui,Xaw3d,-gtk]`.
