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
