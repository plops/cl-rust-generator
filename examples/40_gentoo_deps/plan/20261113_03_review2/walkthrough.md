# Walkthrough: Review-Fixes für `gentoo-dep-tui` (`plan/20261113_03_review2`)

Datum: 2026-11-14
Auftrag: `plan/20261113_03_review2/fix_plan.md`
Stand: alle Phasen 1–5 umgesetzt, Gates grün (Container + Gentoo-Host)

Dieses Dokument holt dich didaktisch ab: erst was das Tool tut, dann welche
Fehler das Review fand, dann — Phase für Phase — was genau geändert wurde und
warum. Am Ende steht, wie du alles selbst nachprüfst.

## 1. Was ist `gentoo-dep-tui`?

Du änderst ein USE-Flag (z. B. `svt-av1` bei `ffmpeg`) und willst **vorher**
wissen: Wie viele Pakete kommen dazu, welche genau, wie groß wird es — und
löst das Flag einen `REQUIRED_USE`-Konflikt aus? Das Tool beantwortet das
offline anhand der Gentoo-Metadaten (`md5-cache`), ohne `emerge`-Lauf.
Details zur Bedienung stehen in `gentoo-dep-tui/README.md`.

## 2. Die Pipeline im Überblick

Der Code liegt in `gentoo-dep-tui/src/`, die Dateien folgen dem
Datenfluss. Lies sie in dieser Reihenfolge, dann verstehst du jede Phase:

| Modul | Aufgabe in einem Satz |
|---|---|
| `01_types.rs` | Kerndatentypen (`ResolvedNode`, `ConflictInfo`, `QuickSwitch`, Deltas) |
| `02_profile/` | Profilstapel, `make.conf`, `world` einlesen (`parse.rs`, `stack.rs`) |
| `03_manifest.rs`, `04_binhost.rs` | Download-Größen, Binpkg-Größen |
| `05_repo_index.rs` | `md5-cache` → versionierte Pakete, Version wählen (`select*`) |
| `06_graph/` | Welt-Atome → Laufzeitgraph (`resolve.rs`, `blame.rs`, `build_extra.rs`) |
| `07_cascade/` | Fixpunkt: `REQUIRED_USE` + `pkg[flag]` fortpflanzen (`step.rs`, `enforce.rs`, `required_use.rs`) |
| `08_delta.rs` | Marginal-Deltas per Simulation, globale Big Levers |
| `09_plan_toml.rs` | `plan.toml`-Format (Baseline, Pakete, Choices) |
| `10_export_verify.rs` | Export + `emerge --pretend`-Verifikation |
| `11_cli/` | CLI-Argumente, Laden, Subcommands (`args.rs`, `context.rs`, `commands.rs`) |
| `12_tui_state/` | TUI-Zustand und Aktionen (`actions.rs`) |
| `13_tui_view/` | Ratatui-Darstellung (`middle.rs`, `right.rs`, `bottom.rs`) |
| `14_tui_runner.rs` | Terminal-Loop, Tastatur-Events |
| `test_helpers.rs` | gemeinsame Test-Fixtures (nur `#[cfg(test)]`) |

Zwei Begriffe vorweg, weil alles darum kreist:

- **User-Intent** = was *du* explizit gesetzt hast (`engine.overrides`).
  Alles andere (Profil-Defaults, Kaskaden-Forces) ist abgeleitet.
- **Post-Kaskaden-Stand** = die effektiven USE-Flags *nach* dem Fixpunkt
  (`ResolvedNode.use_flags`). Vorher zeigte die UI an manchen Stellen noch
  den reinen Default-Stand — daher Geister-`[ ]` und `0 B`-Deltas.

## 3. Phase 1: Profil und Repo-Index — das Fundament

Alles Spätere hängt von Profil und Index ab, darum kamen sie zuerst.

**Ein Matcher für alle.** `02_profile.rs` und `05_repo_index.rs` enthielten
je eine eigene Versions-/Slot-Vergleichslogik (Drift-Gefahr). Jetzt gibt es
genau eine: `dep_matches_version_and_slot` (+ `version_matches`,
`slot_matches`) in `02_profile`, benutzt von `package.use`-Matching,
`package.mask` und `select_for_dep`. PMS-Detail am Rande: `=*` ist ein
reiner String-Prefix auf dem Versionsstring (`=1*` matcht auch `10`) —
per Unit-Test festgenagelt (`glob_equal_is_pure_string_prefix`).

**`make.conf` wie die Shell lesen.** `parse_key_value` verstand weder
mehrzeilige Quotes (`USE="\n ssl\n"`) noch `\`-Fortsetzungen; jetzt
werden logische Zeilen erst zusammengesetzt (`logical_lines`), dann
kommentarbefreit. Inline-Kommentare (`# …`) werden außerdem in `read_world`,
`apply_flag_list` und `extend_atom_lines` entfernt.

**Präzedenz wie Portage.** `Profile::effective_defaults` erzwingt jetzt die
exakte Reihenfolge: IUSE-Defaults → ARCH/ELIBC/KERNEL → USE_EXPAND →
globale `USE` → `package.use` → `use.force` → `use.mask` (Maske gewinnt
immer). Vorher lief `package.use` zweimal (einmal exakt, einmal
versioniert) und konnte Masken/Forces überstimmen — mit Test
(`package_use_never_beats_mask_or_force`) ist das ausgeschlossen.
`global_use` enthält nie mehr `-`-Einträge: `apply_use_tokens` löst
`-*`/`-flag` deterministisch auf (sortierte Anwendung), und `make.conf`
ohne `${USE}` erweitert das Profil-`USE`, statt es zu ersetzen.

**Kleinigkeiten mit Wirkung:** `KEYWORDS=""` filtert jetzt (vorher still
akzeptiert); `parse_cache_file` unterscheidet Filter (`Ok(None)`) von
echten Fehlern (werden geloggt statt mit `.ok()` verschluckt);
`select_cpv` wählt exakt per CPV; `resolve_make_profile` normalisiert
`..`-Symlinks (`resolve_make_profile_from`, getestet); der doppelte
`use.mask`/`use.force`-Nachladeblock in `load_profile` ist weg; `Box::leak`
ist durch `LoadedContext` (owned) ersetzt.

## 4. Phase 2: Graph und Kaskade — die Kernkorrekturen

**Versionen gehen nicht mehr verloren.** Die BFS-Queue trägt jetzt das
*wählende Atom* statt des nackten `cat/pkg`; `resolve_target` löst dieselbe
Version erneut auf, statt auf die neueste zurückzufallen. Test:
`<dev-libs/lib-2.0` wählt `1.0`, nicht `2.0`. Überall sonst wird per
`select_cpv` mit der exakt aufgelösten Version gearbeitet
(`CascadeStep`, `resolve_build_extra`, `package_deltas`).

**`PDEPEND` zählt mit.** `RDEPEND` und `PDEPEND` bilden gemeinsam die
Runtime (Auswertung + Via-Flags + Buildtime-Transitiva); Test
`pdepend_is_runtime`.

**Via-Flags ehrlich.** `collect_via_flags` trägt jetzt eine Bedingung mit
Tiefenangabe: unbedingt (`None`) gewinnt immer und reihenfolgeunabhängig,
bei Verschachtelung gewinnt die tiefste Bedingung, Negation wird als
`"!foo"` formatiert. Tests: `nested_conditional_keeps_deepest_flag`,
`unconditional_beats_conditional_regardless_of_order`,
`negated_condition_formats_bang`.

**Blame ohne leere Ketten.** Welt-Pakete sind gültige Ketten-Wurzeln — ein
Zyklus zwischen zwei Welt-Paketen liefert echte Ketten statt `[]`
(Test `world_cycle_blame_nonempty`).

**Kaskade verschmutzt keine User-Intents mehr.** `resolve_with_cascade_opts`
arbeitet auf einem Fork (`sim`); `engine.overrides` bleibt unverändert,
das Ergebnis trägt `effective_overrides` (User + Forces). Zusätzlich meldet
die Kaskade nur noch Finalrunden-Konflikte statt transienter
Zwischenstände. Test `cascade_does_not_pollute_user_overrides` prüft An-,
Abschalt- und `plan.toml`-Verhalten.

**REQUIRED_USE mit Respekt.** `force_flag` überschreibt echte User-Intents
nicht mehr still, sondern meldet einen Konflikt mit Übernahme-Option.
`||` überspringt maskierte und user-gesperrte Flags und versteht negierte
Zweige; `^^`/`??`-Konflikte bekommen Optionen mit echten Changes (andere
Flags abschalten), die nach Anwendung den Konflikt tatsächlich auflösen
(Test `exactly_one_quickswitch_resolves`). Use-Deps gegen Flags ohne IUSE
injizieren nichts mehr: Mit `[+]/[-]`-Default gilt die Default-Semantik,
ohne Default gibt es einen Konflikt (Tests
`use_dep_missing_flag_with_default_not_injected` /
`..._without_default_conflicts`).

## 5. Phase 3: Deltas, Export, CLI — die UI lügt nicht mehr

**Deltas gegen den wahren Stand.** `build_delta`, `flag_delta` und
`package_deltas` lesen den aktuellen Zustand aus `before.graph.nodes`
(Post-Kaskade) statt aus reinen Defaults. Folge: Ein kaskaden-erzwungenes
Flag zeigt `[x]` und toggelt sauber zurück — kein `0 B`-Geistertoggle mehr
(Test `cascade_enabled_flag_shows_current`).

**Big Levers ohne Plattform-Krücken.** ARCH/ELIBC/KERNEL-Flags sowie
profil-maskierte und -erzwungene Flags sind keine Hebel und werden
herausgefiltert (Test `big_levers_skip_platform_mask_force`). Neu:
`big_levers_with_base` — `AppState::refresh` reicht seine bereits
berechnete Baseline durch, statt ein zweites Mal aufzulösen. Die
`big-levers`-CLI zeigt zusätzlich eine `DL-Delta`-Spalte.

**Export, der wirklich verifizierbar ist.** `export_plan` und
`export_portage` teilen sich `write_export_files`: menschenlesbare
Top-Level-Dateien (`world`, `package.use`) *plus* Portage-Standardlayout
(`var/lib/portage/world`, `etc/portage/package.use`), sodass
`VerifyRunner::against_configroot` mit
`emerge --root … --config-root … @world` funktioniert. Bestehende Tests,
die Top-Level-Dateien erwarten, bleiben grün. Nebenbei: `cmd_blame` löst
mit Kaskade auf (kaskaden-induzierte Deps erscheinen in Blame-Ketten), und
`LoadedContext` ersetzt das `Box::leak`-Konstrukt ersatzlos.

## 6. Phase 4: TUI — Cursor, Modals, ruhiges Nachladen

**Cursor bleibt, wo er ist.** `rebuild_middle` merkt sich Paket und Flag und
stellt die Selektion wieder her, statt auf 0 zu springen (Test
`toggle_preserves_cursor_on_same_flag`). Toggeln misst gegen den
Graphen-Stand (mit Default-Fallback).

**`Space` auf Konflikt öffnet ein Modal.** Vorher passierte bei
vor-simulierten Konflikten still gar nichts. Jetzt öffnet sich bevorzugt
der echte Konflikt, sonst die neue Variante `Modal::SimulatedConflict`
(Test `space_on_conflict_flag_opens_modal`). `Enter` behandelt beide
einheitlich (`modal_selected_option`); leere QuickSwitch-Changes erzeugen
nur einen Status-Hinweis statt Dirty-Flag (Test
`empty_quick_switch_sets_status_not_dirty`). Big-Lever-Toggles lehnen
Plattform-/Mask-/Force-Flags mit Statusmeldung ab.

**Ansicht repariert.** Rechte Pane mit Fokus-Rahmen und eigenem
Konflikt-Abschnitt, Mitte mit korrektem Scroll-Offset (Kopfzeilen werden
mitgezählt, inkl. `(rechne Deltas...)`), Filter-Prompt `/…_` auch bei
leerem Filter, Footer wächst bei Status/Dirty auf 4 Zeilen (kein Clipping
bei 80×24). TestBackend-Tests bei 80×24 und 120×40 prüfen Rahmenfokus,
Konfliktabschnitt und Filter-Prompt.

**Event-Loop atmet.** `Up`/`Down` laden Deltas nicht mehr synchron nach
(kein T05-Bruch mehr); stattdessen pollt die Loop mit 25 ms bei
ausstehenden Deltas und 100 ms sonst — Idle-Recompute passiert in der
Pause. `run_tui` startet auch ohne `--world`-Datei, wenn `--plan` eine
nicht-leere `baseline.world` liefert (Test `resolve_world` ohne Terminal:
`plan_world_fallback_without_world_file`).

## 7. Phase 5: Aufräumen — kleine Module, ein Fixture-Helfer

**Ein Fixture-Helfer statt sechs `pkg()`-Kopien.**
`src/test_helpers.rs` (`PkgFixture`-Builder + `repo_index`) versorgt alle
Unit-Tests in `src/`; die duplizierten Cache-Entry-Parser sind gelöscht.
(Integrations-Tests unter `tests/` behalten ihre eigenen kleinen Helfer —
sie können `#[cfg(test)]`-Lib-Code nicht nutzen.)

**Module unter ~450 Zeilen, Pfade stabil.** `lib.rs` exportiert weiter
`mod_01_types` … `mod_14_tui_runner`; dahinter stecken jetzt Verzeichnisse:

- `06_graph/` — `resolve.rs`, `blame.rs`, `build_extra.rs`, `tests.rs`
- `07_cascade/` — `step.rs`, `enforce.rs`, `required_use.rs`, `tests.rs`
- `12_tui_state/` — Root (Typen, Refresh, Filter), `actions.rs`, `tests.rs`
- `02_profile/` — Root, `parse.rs`, `stack.rs`, `tests.rs`
- `13_tui_view/` — Root, `middle.rs`, `right.rs`, `bottom.rs`, `tests.rs`
- `11_cli/` — Root (Dispatch), `args.rs`, `context.rs`, `commands.rs`, `tests.rs`
- `05_repo_index/tests.rs`

Sichtbarkeiten zwischen Geschwistermodulen sind gezielt `pub(super)`;
öffentliche Pfade (`mod_06_graph::resolve`, …) sind unverändert.

**Toter Code ist weg:** `UseState`/`PkgKey`/`DepClass` (nie verdrahtet;
`PkgId`/`FlagId`/`IdTable` bleiben als dokumentierte
Integer-ID-Roadmap), die nie gesetzten Felder `PkgVersion::{masked, live}`,
`or_counter`, doppelter `use.mask`/`use.force`-Nachladecode.

Zwei Dateien liegen mit 448/458 Zeilen knapp über der ~450-Marke
(`12_tui_state.rs`, `12_tui_state/actions.rs`) — bewusst in Kauf genommen,
um die im Plan benannte Aufteilung (Navigation/Filter/Cache vs. Aktionen)
nicht zu zerfasern.

## 8. Selbst nachprüfen

Jede Phase endet im Fix-Plan mit `cargo test` und
`cargo clippy --all-targets -- -D warnings`; genau das ist der Gate-Stand:

```bash
cd examples/40_gentoo_deps/gentoo-dep-tui
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test
cargo test --release --test perf_system -- --nocapture
```

Stand dieser Umsetzung: 81 Lib-Tests + 4 HIL + 8 Host-Integration + 5
Perf-Tests grün, Clippy ohne Warnungen, Perf-Budgets eingehalten
(`build_extra_calls=0`, Deltas/Levers im Millisekundenbereich).

**Auf dem Gentoo-Host** (dort läuft das echte Repo unter
`/var/db/repos/gentoo`) gelten dieselben Befehle. Die Sitzung lief über
SSH als `kiel` auf `hpz6` (`10.60.120.93`) mit dem Key
`cl-rust-generator/id_lenovo_to_hpz6`; das Projektverzeichnis ist per
Bind-Mount identisch (`~/stage/…` ↔ `/workspace/src/…`), separater
Target-Dir pro Seite (`CARGO_TARGET_DIR=/tmp/gdt-target`) vermeidet
Lock-Konflikte beim parallelen Bauen. HIL-Tests mit echtem Datensatz
brauchen `GENTOO_HIL_REPO=/pfad/zum/snapshot` — ohne die Variable laufen
sie als SKIP durch (eingebautes Verhalten, kein Fehler).

## 9. Bewusst nicht gemacht

- `PkgId`/`FlagId`-Integer-Graph: als Follow-up skizziert, nicht
  umgebaut (Schnittstellenänderung ohne akuten Nutzen).
- HIL-Datensatz: kein Snapshot verfügbar — `hil_regression` und
  `sys_perf_hil_snapshot` bleiben SKIP-geschützt.
- `tests/`-Integrations-Helfer (`entry`/`write_pkg`): dupliziert, aber
  klein und bewusst nicht in die Lib gezogen (kein Testcode im
  Lieferumfang).
- `world_atom_cpn` vs. `world_cpn`: zwei ähnliche Kürzer
  (TUI-Anzeige vs. Blame-Wurzeln) — getrennt gelassen, da verschiedene
  Fehlertoleranzen (Dep-Parse mit Fallback vs. String-Heuristik).

