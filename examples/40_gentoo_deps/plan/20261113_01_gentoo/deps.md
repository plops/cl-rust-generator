# Abhängigkeiten (`deps.md`)

Dieses Dokument listet alle relevanten Rust-Crates und Gentoo-Referenzprojekte im Format `<organization>/<project>` auf, damit der **DeepWiki MCP** direkt für API-Dokumentation, Codebeispiele und Architekturfragen abgefragt werden kann.

> **Wichtiger Hinweis zu Rust-Versionen:** Alle Crates in `Cargo.toml` gemäß `prompt.txt` mit aktuellem Rust (Edition `2024`) einbinden und nach dem Hinzufügen bei Bedarf mit `cargo upgrade` auf den aktuellen Stand bringen.

---

## 1. Gentoo / Portage Parsing (Primär)

| Crate (crates.io) | DeepWiki MCP Slug (`<org>/<project>`) | GitHub URL | Rolle im Projekt & DeepWiki-Suchhinweise |
| :--- | :--- | :--- | :--- |
| `portage-atom` | `lu-zero/portage-atom` | https://github.com/lu-zero/portage-atom | PMS-9 Parser für Atoms, Versionen (`Ord for Version`), Slots, Blocker, `DepEntry` und `evaluate_use`. DeepWiki-Fokus: `DepEntry::evaluate_use`, `parse_dep_entry`, `UseDep`, `Version`. |
| `portage-metadata` | `lu-zero/portage-metadata` | https://github.com/lu-zero/portage-metadata | PMS-9 Parser für `metadata/md5-cache/` (`CacheEntry::parse`, `EbuildMetadata`, `RequiredUseExpr`, `SrcUriEntry`). DeepWiki-Fokus: `CacheEntry`, `RequiredUseExpr`, `SrcUriEntry`, `dedup`. |

> **Achtung:** Laut Upstream-README sind `portage-atom` und `portage-metadata` teilweise KI-generiert und noch nicht vollständig auditiert. Deshalb kapseln wir beide Crates hinter einem eigenen `RepoIndex`-Adapter (`05_repo_index.rs`) und sichern die Ergebnisse über Host-Tests + HIL-Abgleich gegen `emerge --pretend` ab.

---

## 2. Gentoo / Portage Ökosystem (Referenz / Optional für DeepWiki-Lookup)

| Projekt | DeepWiki MCP Slug (`<org>/<project>`) | GitHub URL | Rolle / Nutzen |
| :--- | :--- | :--- | :--- |
| `portage-cli` | `lu-zero/portage-cli` | https://github.com/lu-zero/portage-cli | Umbrella-Workspace von `lu-zero` (enthält u. a. `portage-repo`, `portage-binpkg`, `portage-vdb`, `portage-atom-pubgrub`). Nützlich als Referenz für Binhost-`Packages`- und Repo-/Profil-Handling. |
| `pkgcraft` | `pkgcraft/pkgcraft` | https://github.com/pkgcraft/pkgcraft | Vollständiges Gentoo-Ecosystem inkl. EAPI/Profile/Restrictions. **Bewusst nicht als Core-Dep gewählt**, da es Bash (`scallop`) einbettet und `cargo-nextest` erzwingt; dient aber als PMS-Referenz. |
| `pkgcore` | `pkgcore/pkgcore` | https://github.com/pkgcore/pkgcore | Python-Referenz für Konfigurations-Domänen (`domain`: `use.mask`, `use.force`, `package.mask`, `KEYWORDS`-Auswertungsreihenfolge). |

---

## 3. CLI, Parallelität, Serialisierung & Fehlerbehandlung

| Crate (crates.io) | DeepWiki MCP Slug (`<org>/<project>`) | GitHub URL | Rolle im Projekt |
| :--- | :--- | :--- | :--- |
| `clap` | `clap-rs/clap` | https://github.com/clap-rs/clap | Subcommands (`index-check`, `graph`, `blame`, `deltas`, `big-levers`, `init`, `export`, `verify`, `tui`) via `derive`-Feature. |
| `rayon` | `rayon-rs/rayon` | https://github.com/rayon-rs/rayon | Paralleles Einlesen tausender `md5-cache`-Dateien und schnelle parallele Delta-Neuberechnung. |
| `serde` | `serde-rs/serde` | https://github.com/serde-rs/serde | `#[derive(Serialize, Deserialize)]` für `plan.toml` und optionale JSON-/Diagnose-Ausgaben. |
| `toml` | `toml-rs/toml` | https://github.com/toml-rs/toml | Parsen und Schreiben des strukturierten `plan.toml`-Formats (inkl. `[[choice]]`, `pros`, `cons`, `suggested_use`). |
| `thiserror` | `dtolnay/thiserror` | https://github.com/dtolnay/thiserror | Strukturierte, typisierte Domänenfehler in der Engine. |
| `anyhow` | `dtolnay/anyhow` | https://github.com/dtolnay/anyhow | Ergonomische Fehlerweitergabe und Kontext-Annotationen in CLI und TUI. |
| `walkdir` | `BurntSushi/walkdir` | https://github.com/BurntSushi/walkdir | Rekursives Traversieren von `metadata/md5-cache/` und Paketverzeichnissen (`Manifest`). |

---

## 4. TUI-Stack

| Crate (crates.io) | DeepWiki MCP Slug (`<org>/<project>`) | GitHub URL | Rolle im Projekt & DeepWiki-Suchhinweise |
| :--- | :--- | :--- | :--- |
| `ratatui` | `ratatui/ratatui` | https://github.com/ratatui/ratatui | TUI-Layout (3-Pane-Ansicht, Tabs, Popups, Tabellen). DeepWiki-Fokus: `ratatui::run` / `init` / `restore` (0.30-API), `Frame::area()`, `TestBackend` für automatisierte UI-Tests. |
| `crossterm` | `crossterm-rs/crossterm` | https://github.com/crossterm-rs/crossterm | Terminal-Backend und Tastatur-/Resize-Events. |

---

## 5. Dev- & Test-Abhängigkeiten

| Crate (crates.io) | DeepWiki MCP Slug (`<org>/<project>`) | GitHub URL | Rolle im Projekt |
| :--- | :--- | :--- | :--- |
| `tempfile` | `Stebalien/tempfile` | https://github.com/Stebalien/tempfile | Erzeugen isolierter synthetischer Gentoo-Repos (`md5-cache`, `Manifest`, `Packages`, `profiles`, `world`) in Host-Unit- und Integrationstests. |
