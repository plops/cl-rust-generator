# Implementierungsplan: Gentoo Dependency & USE-Flag Size Optimizer (`gentoo-dep-tui`)

## 1. Zielsetzung & Architektur-Leitplanken

Wir entwickeln ein deterministisches Rust-CLI- und TUI-Werkzeug („`ncdu` für Gentoo-USE-Flags“), das ausgehend von einer Gentoo-`world`-Datei (oder einer strukturierten `plan.toml` mit AI-generierten Alternativen samt `pros`/`cons`) den Laufzeit-Abhängigkeitsgraphen auflöst, Marginal-Deltas pro USE-Flag und `|| ( a b )`-Alternative berechnet, `REQUIRED_USE`-/`pkg[flag]`-Kaskaden inklusive Konflikten aufdeckt und gültige Portage-Konfigurationen (`world`, `package.use`) exportiert sowie gegen `emerge --pretend` verifiziert.

### Zentrale Architekturentscheidungen (aus `research.md`)
1. **Strikte Trennung von `engine` und `cli`/`tui`:** Alle Parsing-, Graph-, Kaskaden-, Delta- und Export-Logiken bleiben vollständig TUI-unabhängig testbar und über dedizierte CLI-Modi aufrufbar.
2. **Trennung von `RDEPEND` vs. `BDEPEND`/`DEPEND`:** Für finale Container-Größen zählt primär `RDEPEND`; `BDEPEND`/`DEPEND` werden separat ausgewiesen (verhindert Fehlinterpretationen durch Build-Tools wie `rust`/`llvm`).
3. **Dual-Metrik (Binhost-`SIZE` + `Manifest`-Download):**
   - **Primäre Runtime-Proxy-Metrik:** `SIZE` aus der Gentoo-Binhost-`Packages`-Indexdatei (löst das `linux-headers`-Problem, bei dem 140 MB Kernel-Tarball nur 6 MB Header ergibt).
   - **Fein-Metrik für Flag-Deltas:** `Manifest` (`DIST`-Dateigrößen in Bytes) verknüpft mit `SRC_URI`-USE-Bedingungen, falls ein USE-Flag keine neuen Pakete zieht, aber optionale Distfiles ändert.
4. **Reference-Counting auf Integer-ID-Graph:** Pakete und USE-Flags werden intern auf `u32`-IDs (`PkgId`, `FlagId`) gemappt. Dadurch werden echte **Marginal-Deltas** berechnet (ein abgewähltes Flag auf Paket A spart bei gemeinsam genutztem Paket C exakt `0 B`, solange Paket B C weiterhin benötigt).
5. **Saubere Filterung vor Auflösung:** `KEYWORDS` (z. B. `amd64`), `package.mask` und Profil-Regeln (`use.mask`, `use.force`) werden vor der Versionswahl angewandt, damit keine `9999`-Live-Ebuilds ohne `Manifest`/`SIZE` gewählt werden.
6. **TOML statt Markdown-/Text-Kommentare:** `plan.toml` speichert `pros`, `cons`, `suggested_use` und `[[choice]]`-Blöcke als echte Datenfelder, sodass sie in der TUI angezeigt und beim Speichern nicht verloren gehen.

---

## 2. Kontextaufbau: Wesentliche Dateien für den ausführenden AI-Agenten

Vor der ersten Codezeile **muss** der Agent folgende Dateien lesen und auswerten, um den vollständigen Projektkontext aufzubauen:

### A. Steuerungs- und Planungsdateien (im Ordner `./plan/20261113_01_gentoo/`)
1. `./plan/20261113_01_gentoo/prompt.txt`
   - **Inhalt:** Verbindliche Arbeitsanweisungen: Git-Branch-Regel (Branch auf Basis von `master`), Conventional Commits, Docker-Umgebung, SSH-Zugang zum echten Gentoo-Host (`ssh kiel@localhost -i ~/.ssh/from_android`), Rust-2024-Standards, verpflichtende Datei-Aufteilungsregel (Dateien <= ~600 Zeilen, zweistellige Nummerierung `01_...rs` nach Datenflussreihenfolge, `lib.rs`/`main.rs` ohne Verhalten), Test-Gates (`cargo test`, `cargo clippy --all-targets -- -D warnings`, `cargo fmt --check`), `plan_effort.md`-Pflicht und strenge Struktur für `walkthrough.md`.
   - **Hinweis zur Bereinigung von Template-Altlasten:** `prompt.txt` enthält vereinzelte Reste aus anderen Projekten (`sbcl`, `01_clock.rs`, `02_usb_cdc.rs`, `Paketzensus`). Dieses Projekt ist **reines Rust 2024** ohne Common Lisp und ohne Embedded-Firmware. Die Architektur-/Aufteilungs- und Test-Prinzipien gelten jedoch uneingeschränkt sinngemäß.
2. `./plan/20261113_01_gentoo/deps.md`
   - **Inhalt:** Liste aller externen Crates und Gentoo-Referenzprojekte in `<organization>/<project>`-Notation für schnelle DeepWiki-MCP-Lookups.
3. `./plan/20261113_01_gentoo/tasks.md`
   - **Inhalt:** Serielle Schritt-für-Schritt-Aufgabenliste. Jeder Modus wird in der Reihenfolge **Implementierung → Host-Tests → HIL-Nachweis** abgeschlossen, bevor die TUI-Tasks beginnen.

### B. Fachliche Recherche & Algorithmus-Referenzen (im Ordner `./plan/misc/`)
4. `./plan/misc/research.md`
   - **Inhalt:** Vollständige Problem- und Architekturanalyse: Warum `md5-cache` statt Bash-Parsing, Fallstricke (Shared Dependencies, `linux-headers`-Tarball vs. Binhost-`SIZE`, `REQUIRED_USE`-Kaskaden, Konflikte, Performance durch Integer-IDs, `plan.toml`-Schema), priorisierte Features (Blame-View, transitive `|| ( a b )`-Alternativen, Big Levers, Konflikt-Quick-Switch, `emerge -p`-Verifikation) und Phasenplan (PoC → MVP → Komfort).
5. `./plan/misc/lu-zero__portage-metadata.md`
   - **Inhalt:** DeepWiki-Zusammenfassung der drei Kernalgorithmen in `portage-atom` (`Ord for Version`, `parse_dep_entry`/`is_use_conditional`, `DepEntry::evaluate_use`) und `portage-metadata` (Winnow-Ausdrucksparser für `REQUIRED_USE`/`SRC_URI`, `RawCacheEntry → ParseState → CacheEntry`, rekursive `dedup`).
6. `./plan/misc/pkgcraft.md`
   - **Inhalt:** Überblick über `pkgcraft` (inkrementelles EAPI-System, Bash-Ausführung über `scallop`, Restriction-Engine). Erklärt, warum `pkgcraft` als Referenz dient, für diesen schlanken PoC/MVP aber `portage-atom` + `portage-metadata` vorzuziehen sind.
7. `./plan/misc/pkgcore.md`
   - **Inhalt:** Referenz zur Gentoo-Konfigurations-Domäne (`domain.get_package_use_unconfigured`: IUSE-Defaults, `USE_EXPAND`, Keywords, `use.mask`/`use.force`, `package.mask`-Reihenfolge).

### C. Repo-Umgebung & Echte Gentoo-Host-Quellen (lesend inspizieren)
8. **Repository-Root (`README.md`, `.gitignore`, bestehende Crate-Ordner)**
   - **Zweck:** Verifizieren der Konvention „Jedes Beispiel-Crate steht für sich (kein Workspace-`Cargo.toml` im Root)“ und Festlegen des Crate-Verzeichnisses (z. B. `./gentoo-dep-tui/`).
9. **Echter Gentoo-Host (über `ssh kiel@localhost -i ~/.ssh/from_android`)**
   - `/var/db/repos/gentoo/metadata/md5-cache/` (reale Cache-Einträge)
   - `/var/db/repos/gentoo/<cat>/<pkg>/Manifest` (reale `DIST`-Größen)
   - `/var/db/repos/gentoo/profiles/` & `/etc/portage/make.profile` (aktives Profil, `use.mask`, `use.force`, `package.mask`)
   - `/etc/portage/make.conf`, `/etc/portage/package.use`, `/etc/portage/package.mask`, `/var/lib/portage/world`
   - Aktuelle Gentoo-Binhost-`Packages`-Datei (falls lokal auf Host vorhanden oder einmalig in einen HIL-Cache unter `/tmp/gentoo-hil/` heruntergeladen).

---

## 3. Verbindliche Dateistruktur (Rust 2024 & `01_...rs`-Regel)

Das eigenständige Crate (z. B. `gentoo-dep-tui/`) folgt strikt der Initialisierungs- und Datenfluss-Reihenfolge. Jede Datei hat genau eine klar abgrenzbare Zuständigkeit, bleibt unter ~600 Zeilen und enthält ihre zugehörigen Unit-Tests (`#[cfg(test)] mod tests`) direkt am Dateiende. `lib.rs` und `main.rs` enthalten ausschließlich Modul-Deklarationen und Verdrahtung.

```text
gentoo-dep-tui/
├── Cargo.toml
├── tests/
│   ├── host_integration.rs    # End-to-End Host-Tests auf synthetischen tempfile-Repos
│   └── hil_regression.rs      # Regressionstests auf echtem Gentoo-Snapshot (skip ohne Dataset)
└── src/
    ├── lib.rs                 # Nur pub mod-Deklarationen & Re-Exports (kein Verhalten)
    ├── main.rs                # Nur CLI-Parsing-Aufruf & Dispatch zu Modi/TUI (kein Verhalten)
    ├── 01_types.rs            # Basis-IDs (PkgId, FlagId), SizeMetrics, UseState, ConflictInfo, BlameChain, Fehler
    ├── 02_profile.rs          # Profil-Stack, make.conf, KEYWORDS, package.mask, use.mask, use.force, world-Reader
    ├── 03_manifest.rs         # Parser für Gentoo Manifest (DIST-Einträge & Dateigrößen in Bytes)
    ├── 04_binhost.rs          # Parser für Gentoo Binhost Packages-Index (CPV, USE, SIZE, RDEPEND)
    ├── 05_repo_index.rs       # RepoIndex-Kapselung über portage-metadata + portage-atom, rayon+walkdir, Versionswahl
    ├── 06_graph.rs            # RDEPEND-/BDEPEND-Graph, Ref-Counting, ||-Alternativen-Erkennung, Blame-Reverse-Edges
    ├── 07_cascade.rs          # REQUIRED_USE & pkg[flag]-Kaskaden, Fixpunkt-Solver, Konflikt- & Quick-Switch-Ermittlung
    ├── 08_delta.rs            # Marginal-Delta-Engine (Binpkg + Download), Big-Levers-Aggregation
    ├── 09_plan_toml.rs        # Konvertierung world -> plan.toml, Laden/Speichern inkl. [[choice]], pros/cons
    ├── 10_export_verify.rs    # Export (world, package.use) & Verify-Runner (emerge --pretend --emptytree)
    ├── 11_cli.rs              # CLI-Definition (clap) & Ausführung der Nicht-TUI-Modi
    ├── 12_tui_state.rs        # TUI-AppState, Tabs, Filter, Cursor, Toggle-/Quick-Switch-Logik
    ├── 13_tui_view.rs         # Ratatui 3-Pane-Layout, Blame-View, Big-Levers-Tab, Konflikt-Modal, Footer
    └── 14_tui_runner.rs       # Terminal-Lifecycle (ratatui::run/init/restore) & crossterm Event-Loop
