# Walkthrough: Review-3-Fixes für `gentoo-dep-tui` (`plan/20261114_01_review3`)

Datum: 2026-11-14
Auftrag: `plan/20261114_01_review3/fix_plan.md`
Stand: alle Phasen 1–7 umgesetzt, Gates grün (Container + Gentoo-Host via SSH)

Dieses Dokument holt dich didaktisch ab: erst was das Tool tut, dann welche
Fehler das Review fand, dann — Phase für Phase — was genau geändert wurde
und warum. Am Ende steht, wie du alles selbst nachprüfst. Die fertigen
Text-Screenshots des neuen Stands liegen in
[`screencaps_after.md`](screencaps_after.md) — lies sie parallel zu
Abschnitt 3, dann siehst du jeden Fix am Bild.

## 1. Was ist `gentoo-dep-tui`?

Du änderst ein USE-Flag (z. B. `svt-av1` bei `ffmpeg`) und willst **vorher**
wissen: Wie viele Pakete kommen dazu, welche genau, wie groß wird es — und
löst das Flag einen `REQUIRED_USE`-Konflikt aus? Das Tool beantwortet das
offline anhand der Gentoo-Metadaten (`md5-cache`), ohne `emerge`-Lauf.
Details zur Bedienung stehen in `gentoo-dep-tui/README.md`
(neu: Abschnitt „Zahlen & Metriken richtig lesen“).

## 2. Die Pipeline im Überblick

Der Code liegt in `gentoo-dep-tui/src/`, die Dateien folgen dem
Datenfluss. Lies sie in dieser Reihenfolge, dann verstehst du jede Phase:

| Modul | Aufgabe in einem Satz |
|---|---|
| `00_perf.rs` | **Neu:** optionale Perf-Instrumentierung (`--features perf-metrics`), sonst No-Ops |
| `01_types.rs` | Kerndatentypen (`ResolvedNode`, `FlagDelta`, `BigLever`, `atom_cpn`) |
| `02_profile/` | Profilstapel, `make.conf`, `world` — plus preparste Pattern-Indexe und `descriptions.rs` (Flag-Doku) |
| `03_manifest.rs`, `04_binhost.rs` | Download-Größen, Binpkg-Größen |
| `05_repo_index.rs` | `md5-cache` → versionierte Pakete, lock-freie Manifest-Ladung |
| `06_graph/` | Welt-Atome → Laufzeitgraph (Queue trägt jetzt geparste `Dep`s) |
| `07_cascade/` | Fixpunkt mit Runden-Cache (`step.rs`), `enforce.rs` nutzt ihn mit |
| `08_delta.rs` + `08_delta/shared.rs` | Marginal-Deltas mit No-Op-Skip, Big Levers, Shared-Blocker-Analyse |
| `09_plan_toml.rs` | `plan.toml`-Format (Baseline, Pakete, Choices) |
| `10_export_verify.rs` | Export + Host-Config-Ergänzung + `emerge --pretend`-Verifikation mit Vorab-Checks |
| `11_cli/` | CLI-Argumente (`--perf-report`, `deltas --deep`), Subcommands |
| `12_tui_state/` | TUI-Zustand, Aktionen, gecachte Baselines (`last_outcome`, `shared_impact`) |
| `13_tui_view/` | Ratatui-Darstellung (neue Labels, Detailbox, Hilfe-Modal) |
| `14_tui_runner.rs` | Terminal-Loop; Tastenlogik steckt in `dispatch_key` (auch für Tests) |
| `test_helpers.rs`, `tests/tui_screencaps.rs` | Fixtures + skriptbare Screenshot-Tests |

Drei Begriffe vorweg, weil alles darum kreist:

- **IUSE-Gate** = ein Flag wirkt nur auf Pakete, die es in `IUSE`
  deklarieren. Globale `USE` ohne Gate erzeugte Phantom-Flags (`openmp` auf
  555 Paketen, obwohl fast keines es kennt).
- **No-Op-Skip** = steht ein Flag in keiner Liste des Pakets und in keinem
  eingehenden `use_dep`, liefert die Delta-Funktion sofort `0 B` — bewiesen
  bit-identisch zur vollen Simulation (Äquivalenz-Test liegt bei).
- **Shared-Blocker** = Abhängigkeit, die beim alleinigen Abschalten von
  anderen gehalten wird. Die Analyse sagt, was das Abschalten *überall*
  bzw. mit Entfernen von Welt-Paketen sparen würde.

## 3. Was das Review fand (Screenshots → echte Bugs)

Deine manuellen Screenshots zeigten **vier echte Software-Bugs**, keine
Verständnisfehler:

1. **Fußzeile log:** `BDEPEND 243.2 MB` war die Binhost-Summe von
   *Build*-Paketen, `Download` zählte nur Tarballs — beides missverständlich
   benannt. → Phase 5 benennt um und trennt auf.
2. **`Subgraph-DL: 11.2 MB` log:** Es war nur der Tarball von `ffmpeg`
   selbst, nicht der Subgraph. → heißt jetzt `Pkg-Quell-DL`.
3. **Falsche Flags nach Filter:** Nach `/ffm` zeigte `ffmpeg` noch die Flags
   von `gentoo-sources` (`apply_filter_only` ließ die Mitte unangetastet).
   → Phase 1 stellt die Mitte beim Selektionswechsel sofort um.
4. **Phantom-Flags überall:** `openmp` auf 555 Paketen, `cet`/`abi_x86_64`
   auf Kernel/Emacs — `global_use`/`use_force` wurden jedem Paket
   hinzugefügt, auch ohne `IUSE`-Eintrag. → Phase 1 gatet alles auf `IUSE`.

Dazu: leere rechte Pane nach Paketwechsel (`right_scroll` blieb),
`Verify`-`ENOENT` ohne vorherigen Export, und massive CPU-Kosten durch
millionenfaches `Dep::parse` in Hot Loops (→ Phasen 2/3).

## 4. Phase 1: Korrektheit zuerst (IUSE, Filter, State)

**IUSE-Gate (`02_profile.rs`).** `effective_defaults` wendet globale USE,
`package.use` und `use.force` nur noch auf deklarierte Flags an
(`token_in_iuse`); `ARCH`/`ELIBC`/`KERNEL` bleiben immer aktiv, landen aber
per Filter nicht in Deltas/Hebeln. `Engine::effective_use` (`06_graph.rs`)
gibt Maske/Force Vorrang vor Overrides. Neu: `Profile::use_expand_hidden`
(aus `USE_EXPAND_HIDDEN` in `make.defaults`, gelesen in `stack.rs`),
ebenfalls aus der schaltbaren Liste gefiltert (`08_delta.rs`).

**Filter-Stale (`12_tui_state.rs`, `actions.rs`, `14_tui_runner.rs`).**
`apply_filter_only` merkt sich das vorige Paket und ruft bei Wechsel
`select_left_fast` auf (leert stale Flags, setzt `deltas_loading`).
`right_scroll` wird in `select_left_fast` und bei Paketwechsel in
`rebuild_middle` zurückgesetzt. `Esc` löscht zuerst einen aktiven Filter,
dann Pane, dann Beenden.

**Ein Kürzer (`01_types.rs`).** `atom_cpn()` ersetzt `world_atom_cpn` und
`world_cpn` (inkl. `:slot`-/`[use]`-Tests). Der alte Test
`apply_use_defaults_order` kodierte das Phantom-Verhalten und wurde
korrigiert (Force ohne IUSE bleibt draußen).

## 5. Phase 2: Messen, ohne zu kosten (`--features perf-metrics`)

Neues Modul `00_perf.rs`: alle Zähler und Phasen-Timer (`ScopeTimer`, misst
bis `Drop`) hinter `#[cfg(feature = "perf-metrics")]`, sonst identische
API als `#[inline(always)]`-No-Ops — null Overhead im Standard-Build. Die
alten unkonditionalen Atomics aus `01_types.rs` sind dorthin umgezogen.
`--perf-report` in `CommonArgs` druckt am Ende von `index-check`, `graph`,
`deltas` und `big-levers` den Phasen-Report (ohne Feature: Hinweis).
`tests/perf_system.rs` bleibt in beiden Konfigurationen grün und prüft mit
Feature echte Werte (`effective_use_calls > 0`, `delta_runs == 60`,
`big_levers_runs == 1`).

## 6. Phase 3: Hot Paths (einmal parsen, weniger simulieren)

- **Profile preparsen (3.1):** `package.mask`/`package.use` werden in
  `sync_pattern_caches()` je Atom genau einmal geparst (`UseRule`-/
  `MaskRule`-Indexe); `package_use_for`/`is_masked` machen nur noch
  `HashMap`-Lookups. Tests, die Roh-Felder direkt belegen, rufen den
  Sync-Helfer explizit — so verlangt es der Plan.
- **Lock-frei laden (3.2):** `preload_manifests` liest alle Manifeste vorab
  parallel in eine Map; der `Mutex` in `parse_cache_file` ist weg
  (Fehlverhalten bei defekten Manifesten identisch).
- **Schlanker Graph (3.3):** Die BFS-Queue trägt geparste `Dep`s (kein
  `to_string()` + `Dep::parse()` pro Knoten mehr); `collect_blame = false`
  überspringt in Simulationen `reverse`/`refcounts`/`alternatives`/`via_map`
  (`out_edges` bleibt — die Kaskade braucht sie); `CascadeStep` cacht
  effektive Flags pro Runde mit Invalidierung bei Mutation; der redundante
  Extra-`resolve` bei `!changed` entfällt; `FlagId`/`IdTable` sind gestrichen
  (der tote Cast in `08_delta.rs` gleich mit).
- **No-Op-Skip (3.4):** `flag_can_affect_graph` prüft eigene Listen
  (`rdepend`/`pdepend`/`src_uri`/`required_use`, strukturiert gewalkt) und
  eingehende `use_deps`. Trifft nichts zu, liefert `noop_delta` sofort —
  inklusive Baseline-Konfliktlage, also bit-identisch. `package_deltas_with_base`
  + `AppState::last_outcome` sparen je Paketbesuch einen Kaskadenlauf;
  Big Levers sortieren deterministisch.

## 7. Phase 4: „Was spart das wirklich?“ (Shared-Blocker, Doku, Details)

**`08_delta/shared.rs` (4.1).** `shared_removal_impact(engine, world, base,
package, flag)` — nur on demand für das selektierte Flag, im State
gecacht: Überlebende gezogene Deps finden (`outcome_alone` + Closure über
`reverse`/`out_edges`), Halter per Tiefensuche in Flag-Blocker
(`app/b [ssl]`) und World-Blocker (`app/c`, unbedingt) trennen, dann je
*eine* exakte Simulation (mit Flag-Blockern aus / zusätzlich ohne
World-Blocker). Rechte Pane und `deltas <pkg> --deep` zeigen das Ergebnis.
Der Diamond-Test aus dem Plan steht Wort für Wort drin (Marginal `0 B`,
`without_all_blockers` = volles `openssl` plus entferntes Welt-Paket).

**Flag-Doku (4.2).** `02_profile/descriptions.rs` lädt tolerant
`use.desc`, `use.local.desc`, `desc/*.desc`; Anzeige lokal-vor-global in
der rechten Pane.

**Hebel-Details (4.3).** `BigLever` trägt `affected_packages` sowie
hinzu/entfernte Pakete; Tab 2 zeigt unten eine Detailbox (Beschreibung,
Betroffene, Entfernt/Hinzu).

## 8. Phase 5: Eindeutige Zahlen, Hilfe, fehlertolerantes Verify

**Labels (5.1).** Mitte: `Pkg-Quell-DL`, Deltas mit Vorzeichen (`fmt_delta`).
Fußzeile: `Binpkg <X> (<hits>/<total> Hits)` (bzw. `n/a (ohne --binhost)`),
`Quell-DL`, `Build-Only <bin> bin / <src> dl`, dazu `[/] Suche` und
`[?] Hilfe`. Die Fußhöhe wird aus Inhalt und Breite berechnet — bei 80x24
clippt nichts (per TestBackend geprüft).

**Hilfe (5.2).** `Modal::Help`, geöffnet mit `?`/`F1`, geschlossen mit
`Esc`/`?`/`q` (Enter schloss schon immer). Inhalt: Metriken, Navigation,
`[S]`→`[E]`→`[V]`-Ablauf.

**Verify (5.3).** `VerifyRunner::run` prüft Configroot-Existenz (klare
Meldung statt `ENOENT`) und `emerge` im `PATH` (nur lokale Befehle).
`export_plan` ergänzt `make.profile`/`make.conf`/`repos.conf` aus
`/etc/portage` (nur Fehlendes, sonst No-Op). `[V]` in der TUI exportiert
bei `dirty`/fehlendem Export automatisch vor.

## 9. Phase 6: Ein Dispatcher, skriptbare Tests, echte Screenshots

**Dispatcher (6.1).** Die gesamte Tastenlogik aus `event_loop` steht in
`dispatch_key(state, key, ctx) -> LoopAction` — Terminal und
Test-Driver rufen exakt dieselbe Funktion; `step_idle` spiegelt den
Idle-Pfad.

**Screencaps (6.2).** `tests/tui_screencaps.rs` baut das Review-Szenario
nach (`gentoo-sources` mit `!build?`, `ffmpeg`, `emacs` + `firefox-bin`
als Shared-Blocker, Konflikt-Paar, Choice, `use.local.desc`) und fährt
sechs Captures: Start, `/ffm`-Filter (assert: `Pkg-Quell-DL`, keine
Sources-Flags), `emacs`+`gtk` (assert: `Voll-Ersparnis`, Blocker, Doku),
Tab 2 (assert: kein `openmp`, Detailbox), Hilfe, Konflikt-Modal — je in
140x36 und 80x24, gesichert nach `target/screencaps/`. Optional dasselbe
gegen echten Snapshot (`GENTOO_HIL_REPO`).

## 10. Phase 7: Doku und Abnahme

README: neuer Abschnitt „Zahlen & Metriken richtig lesen“, aktualisierte
Tastentabelle, Verify-Reihenfolge, `--perf-report`/`--deep`-Doku,
Screencapture-Anleitung. Modul-Docstrings der geänderten Module
aktualisiert. Gates: `cargo fmt --check`, `cargo clippy --all-targets
--all-features -- -D warnings`, `cargo test`,
`cargo test --release --features perf-metrics --test perf_system --
--nocapture`, `GENTOO_SCREENCAP_OUT=…/screencaps_after.md cargo test
--test tui_screencaps`, HIL-Suite auf dem Gentoo-Host — alles grün.

## 11. Selbst nachprüfen

```bash
cd gentoo-dep-tui
cargo test                                   # 100 Unit + 8 + 5 + 2 Screencap
cargo test --test tui_screencaps             # Review-Szenario + Artifact
cargo clippy --all-targets --all-features -- -D warnings
cargo test --release --features perf-metrics --test perf_system -- --nocapture
GENTOO_SCREENCAP_OUT=../plan/20261114_01_review3/screencaps_after.md \
  cargo test --test tui_screencaps
# Auf dem Gentoo-Host (SSH siehe Session-Notizen):
GENTOO_HIL_REPO=/tmp/gentoo-hil cargo test --release
```

Spots zum Anfassen: `effective_defaults` (IUSE-Gate), `dispatch_key`
(Header-Doku listet alle Tastenpfade), `shared.rs`-Diamond-Test,
`review_screencaps.md` unter `target/screencaps/`.

## 12. Bewusst nicht gemacht (Abweichungen vom Plan)

- **DeepWiki nur teilweise:** Der Plan verlangt DeepWiki-MCP für jede
  Dependency-Prüfung. Der DeepWiki-Aufruf scheiterte am Repo-Namensformat;
  stattdessen wurden die vendored Crate-Quellen (`~/.cargo/registry`, z. B.
  `Dep`-/`IUse`-/`DepEntry`-Felder, `RequiredUseExpr`) als Primärquelle
  geprüft. Keine API wurde geraten, keine Dependency hinzugefügt.
- **Optionale HIL-Vorher/Nachher-Messung (Phase 3):** nicht als
  kontrollierter Vorher/Nachher-Vergleich durchgeführt — belegt sind
  Äquivalenz (Skip ≡ Vollsimulation per Test), synthetische Counter
  (`effective_use` 248 → 187) und grüne HIL-Suite auf dem Host.
- **`deltas --deep` statt eigenem `impact`-Subcommand:** Der Plan ließ die
  Wahl; `--deep` zeigt nur Flags mit Mit-Blockern (stille Flags brauchen
  keinen Bericht).
- **`desc/*.desc` tolerant:** pro Zeile `name - desc`, nur fehlende Einträge
  ergänzend — unbekannte Formate landen harmlos in der globalen Map, statt
  zu scheitern.

