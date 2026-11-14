# Walkthrough: Review-4-Fixes für `gentoo-dep-tui` (`plan/20261114_02_review4`)

Datum: 2026-11-14
Auftrag: [`fix_plan.md`](fix_plan.md)
Stand: alle Phasen 1–5 umgesetzt, Gates grün (Container **und** echter
Gentoo-Host via SSH)

Dieses Dokument holt dich didaktisch ab: erst was das Tool tut und was das
Review bemängelte, dann — Phase für Phase — was genau geändert wurde und
warum. Am Ende steht, wie du alles selbst nachprüfst. Die Bedienung des
fertigen Tools erklärt [`gentoo-dep-tui/README.md`](../../gentoo-dep-tui/README.md).
Sicherheitsnotiz: Verbindungsdetails zum Test-Host (Adressen, Keys) gehören
nicht ins Repo und stehen hier bewusst nicht.

## 1. Was ist `gentoo-dep-tui`?

Du änderst ein USE-Flag (z. B. `svt-av1` bei `ffmpeg`) und willst **vorher**
wissen: Wie viele Pakete kommen dazu, welche genau, wie groß wird es — und
löst das Flag einen `REQUIRED_USE`-Konflikt aus? Das Tool beantwortet das
offline anhand der Gentoo-Metadaten (`md5-cache`), ohne `emerge`-Lauf. Neu in
dieser Iteration: die umgekehrte Frage — „Wie werfe ich `x11-libs/gtk+`
raus, und was spare ich dadurch?“ (`cut`) — plus eine JSON-Schnittstelle
(`simulate`/`batch`), über die ein AI-Agent Dutzende Was-wäre-wenn-Fragen
pro Sekunde stellen kann.

## 2. Die Pipeline im Überblick

Der Code liegt in `gentoo-dep-tui/src/`, die Dateien folgen dem Datenfluss.
Lies sie in dieser Reihenfolge, dann verstehst du jede Phase:

| Modul | Aufgabe in einem Satz |
|---|---|
| `01_types.rs` | Kerndatentypen (`SizeMetrics`, `FlagDelta`, `BigLever` — jetzt serialisierbar, `PkgId` entfernt) |
| `02_profile/` | Profilstapel, `make.conf`, `world` — plus `package.mask`/`unmask`/`accept_keywords` aus dem Portage-Verzeichnis |
| `03_manifest.rs`, `04_binhost.rs` | Download-Größen; Binpkg-Index (geschrumpft auf CPV + SIZE) |
| `05_repo_index.rs` | `md5-cache` → versionierte Pakete, mit `OnceLock`-Baseline-Cache je Paket |
| `06_graph/` | Welt-Atome → Laufzeitgraph (mit `has_binpkg`, `node_costs`, repariertem Blame) |
| `07_cascade/` | Fixpunkt-Kaskaden (REQUIRED_USE, Use-Deps) |
| `08_delta.rs` + `08_delta/shared.rs` | Marginal-Deltas, Big Levers (`turn_on`, `net_pkg_delta`), Shared-Blocker- und Cut-Analyse über `FlagBlocker` |
| `11_cli/` | CLI: `graph`, `blame`, `deltas`, `cut`, `big-levers`, `simulate`, `batch`, `tui` — alle lesenden mit `--json` |
| `12_tui_state/` | TUI-Zustand ohne Plan-Ballast, mit Sortiermodus, Cut-Plan und Undo-Stapel |
| `13_tui_view/` | Ratatui-Darstellung (Pkg-Delta-Spalte, Cut-Box, Inline-Konflikte) |
| `14_tui_runner.rs` | Terminal-Loop mit 80-ms-Idle-Entprellung; `dispatch_key` steuert auch die Tests |

Drei Begriffe vorweg, weil alles darum kreist:

- **Marginal-Delta** = exakte Größenänderung *eines* Toggles (Toggle →
  Kaskade → Re-Resolve, vorher/nachher verglichen). `0 B` ist normal bei
  geteilten Abhängigkeiten.
- **Shared-Blocker** = Abhängigkeit, die beim alleinigen Abschalten woanders
  gehalten wird. Die Analyse sagt, was Abschalten *überall* bzw. mit
  Entfernen von Welt-Paketen sparen würde.
- **Cut-Plan** = umgekehrte Frage: Welche Flag- bzw. Welt-Blocker halten
  `target` im Graphen, und was spart deren Entfernung exakt?

## 3. Phase 1 — Entschlackung: `plan.toml` ist weg

Das Review fand: `plan.toml`/`[[choice]]`/`verify` kosteten viel Code
(zwei ganze Module, Export-/Verify-Logik, kaputte
`make.profile`-Symlink-Kopie) für einen Workflow, den niemand brauchte.
Also flog alles raus — ersatzlos bis auf einen Minimal-Ersatz:

- `src/09_plan_toml.rs`, `src/10_export_verify.rs` gelöscht;
  `init`/`export`/`verify`/`--plan` aus der CLI entfernt.
- `[S]` schreibt jetzt nur noch die in der Sitzung geänderten Flags im
  gültigen `package.use`-Format (`cat/pkg flag -other`), `[E]`/`[V]`
  entfallen aus Footer und Hilfe.
- Mit gestorben: `LeftEntry::PlanChoice`, `selected_choice`,
  `cycle_choice_option`, `middle_scroll`, `PkgId`, sowie `use_flags`/`slot`/
  `rdepend` in `BinpkgEntry` (RAM- und Parse-Ersparnis).

Die `||`-Entscheidungen aus echten Ebuilds bleiben — sie stammen aus
`RDEPEND`, nicht aus `plan.toml`. Nur ihr Anzeige-Label ist gekürzt
(`|| <owner>: <erste Option>` statt der endlosen Kandidatenliste).

## 4. Phase 2 — Fünf Bugfixes und ein Performance-Fundament

1. **Leere `Kette 1:` im Blame** (`06_graph/blame.rs`): Bei Welt-Paketen
   ohne Eltern wurde eine leere Kette gepusht. Jetzt schließt `blame_dfs`
   nur bei `path.len() > 1`, und `close_chain` pusht nur nicht-leere
   Ketten — Welt-Pakete zeigen sauber `(kein Pfad — Welt-Paket oder leer)`.
2. **`OnceLock`-Baseline-Cache** (`05_repo_index.rs`, `06_graph.rs`):
   `effective_use` fraß über 50 % CPU-Zeit (Millionen `Version::parse`- und
   `package_use_for`-Aufrufe). Die Profil-Baseline liegt jetzt je Paket
   einmalig im `OnceLock`; ohne eigene Overrides wird sie direkt
   zurückgegeben (der ~99-%-Fall).
3. **Binhost-Treffer vs. `0 B`** (`ResolvedNode.has_binpkg`,
   `middle.rs`): Ohne `--binhost` steht `n/a`, mit Binhost aber ohne
   Treffer `kein Binpkg (Source-Build)` — kein irreführendes `0 B` mehr.
4. **Transitive-Leaf-Bug** (`08_delta/shared.rs`, komplett neu): Die alte
   Analyse lief durch *entfernte* Zwischenknoten bis zu Basispaketen
   (`readline`, `zlib`) und meldete `sudo`/`docker`/`bash` als Blocker von
   `mupdf[-javascript]`. Jetzt werden nur **direkte** Flag-Kinder
   betrachtet, die nach dem Toggle überleben — und von dort aus aufwärts
   gesucht. Derselbe Rewrite fixt den `!flag`-Bug (`"!minimal" => false`
   wurde als Abschalten von `minimal` eingetragen statt als Einschalten);
   dafür gibt es `FlagBlocker { package, flag, want_on }` plus Tests.
5. **Rechte Pane & CLI-Transparenz**: korrekte Toggle-Richtung
   („Ausschalten/Einschalten von … bewirkt“), `+`-Vorzeichen via
   `fmt_delta`, Toggle-Konflikt mit Grund sofort in der Pane (nicht erst
   im Modal), Konfliktgrund auch in `cmd_deltas`.

## 5. Phase 3 — Source-Build- und `ncdu`-Workflow

- **`cut <cat/pkg>` + `[x]`/`[a]`/`[u]`** (`08_delta/shared.rs`,
  TUI-State/Views/Runner): exakte Antwort auf „Wie werfe ich X raus?“
  (`flag_blockers`, `world_blockers`, zwei simulierte Ersparnisse).
  `[a]` übernimmt alle Flag-Blocker, `u` rollt zurück (Stapel, max. 32).
- **`[o]`-Sortierung** (`node_costs()` in `06_graph.rs`): einmal pro
  Refresh berechnete exklusive Kosten je Paket (<1 ms bei ~600 Knoten,
  reine Graph-Erreichbarkeit ohne Kaskade) — `Default` →
  `Exkl. Pakete` → `Exkl. Größe`, mit `(N pkg, X)`-Suffix links.
- **`BigLever { turn_on, net_pkg_delta }`**: Standard nur Abschalten
  (minimales System als Fokus), Sortierung ohne Binhost nach
  `(net_pkg_delta, download)` statt lauter `0 B`-Zeilen, neue
  `Pkg-Delta`-Spalte in Tab 2 und CLI.
- **Portage-Komfort**: `--portage-dir` (Default `/etc/portage`) und
  `--snapshot` leiten alle Pfade ab; `package.mask`/`unmask`/
  `accept_keywords` fließen in Auswahl und Maskierung ein.
- **Idle-Entprellung**: `ensure_deltas()` läuft erst nach ~80 ms ohne
  Tastendruck — schnelles `j`/`k` durch lange Flag-Listen bleibt flüssig.

## 6. Phase 4 — AI-Schnittstelle

`--json` an allen Lese-Subcommands (Perf-Report geht dann nach `stderr`,
damit `stdout` reines JSON bleibt). `graph --json` liefert Totals
(inkl. `build_extra`, Trefferquote), Knoten mit `has_binpkg`,
USE-Flags und exklusiven Kosten, Konflikte und Skips.
`simulate --set cat/pkg=flag1,-flag2 --drop cat/pkg` beantwortet eine
Was-wäre-wenn-Frage; `batch` lädt Index + Profil **einmal** und
beantwortet dann zeilenweise JSON-Anfragen von `stdin` (je Zeile eine
JSON-Antwort, Fehler als `{"error": …}` ohne Abbruch). Beim Bauen fiel
übrigens ein echter Bug auf: `simulate` berechnete die Baseline erst
*nach* Anwenden der Overrides (Diff immer 0) — jetzt davor.

## 7. Phase 5 — Verifikation: lokal grün, Host vermessen

Lokal: `cargo clippy --all-targets -- -D warnings` sauber, alle 126
Tests grün (Unit + `host_integration` + `tui_screencaps` + `perf_system`),
`perf_system` auch mit `--features perf-metrics`. Die Screenshot-Tests
decken neu ab: `[x]`-Cut-Analyse, `[o]`-Sortierung, Inline-Konflikt und
sauberen leeren Blame.

Auf dem echten Gentoo-Host (read-only gegen Repo, `world`,
`/etc/portage`; Binary per Copy deployed): Index 13.690 Pakete in
645 ms; Graph 651 Knoten / 9847 MB DL (mit Binhost 1032 MB Binpkg,
561/651 Treffer); `cut gtk+` nennt nur echte Halter
(`firefox-bin`, `signal-desktop-bin`, `google-chrome`) bei −585 MB /
−7 Paketen; `deltas ffmpeg` 101 Flags / 63 wirksam mit exakten
Konfliktgründen; `big-levers` 391 Hebel sauber sortiert; 6 `batch`-Ops
in ~0,6 s Wandzeit.

## 8. Selbst nachprüfen

```bash
cd gentoo-dep-tui
cargo clippy --all-targets -- -D warnings
cargo test
cargo test --release --features perf-metrics --test perf_system -- --nocapture
cargo test --test tui_screencaps
```

Für den echten Host: Binary bauen (`cargo build --release`), auf den Host
kopieren und die Befehle aus Abschnitt 7 gegen sein Repo/`world` laufen
lassen (nur lesend; `/etc/portage` wird nie beschrieben). Die genauen
Befehle und erwarteten Befunde stehen in [`fix_plan.md`](fix_plan.md),
Abschnitt 6 — inklusive der HIL-Screenshots via `GENTOO_HIL_REPO`, falls
dort eine Rust-Toolchain verfügbar ist.
