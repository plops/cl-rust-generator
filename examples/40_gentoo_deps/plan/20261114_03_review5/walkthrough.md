# Walkthrough: Review-5-Fixes in `gentoo-dep-tui`

Dieser Text nimmt dich an die Hand und führt dich einmal durch alles, was im
Zuge von [fix_plan.md](fix_plan.md) geändert wurde — vom erlebten Problem über
die Idee bis zur Stelle im Code. Du brauchst kein Detailwissen über den
Code: Jeder Abschnitt startet beim Symptom, das du aus dem
[manuellen Testbericht](manual_test_report.md) kennst.

## 1. Wo wir standen

Der manuelle Test auf dem echten Gentoo-System zeigte sechs wiederkehrende
Ärgernisse:

1. **Cursor springt weg**: Nach `o`-Sortierung und einem Toggle landete der
   Cursor auf `git`, obwohl `tlp` gemeint war.
2. **Doppelte Einträge**: `sys-kernel/gentoo-sources` stand zweimal links.
3. **Export lügt**: Nach `[S]` blieb `*ungespeichert*` stehen, und
   `/etc/portage`-Flags (z. B. `pipewire`: `alsa dbus sound-server udev`)
   wurden nicht übernommen.
4. **Falsche CONFLICTs**: Unbeteiligte Flags zeigten `CONFLICT`, sobald
   irgendwo im System ein echter Konflikt existierte.
5. **Filter-Falle**: In der Filtersuche ging keine Navigation — erst `Enter`
   befreite die Tasten.
6. **Rauschen rechts**: `Voll-Ersparnis` schlug bei `pipewire[extra]` sogar
   `firefox-bin` zur Löschung vor.

Dazu kam ein Architekturwunsch: Weniger Simulation, klare Schichtentrennung
(`08_delta` rechnet, `13_tui_view` malt nur).

## 2. Die Landkarte der Änderung

Jedes Symptom hat genau einen Reparaturort. Die folgende Tabelle ist dein
Kompass — Details stehen in den Abschnitten darunter.

| Symptom | Ursache | Reparaturort |
|---|---|---|
| Cursor springt nach Toggle/Sort | Liste wurde per Index restauriert | `12_tui_state.rs`: `rebuild_left_preserving` |
| Doppeltes `gentoo-sources` | `seen`-Menge wurde zu spät geprüft | `12_tui_state.rs`: `rebuild_left_preserving` |
| Export unvollständig, `*ungespeichert*` bleibt | Export schrieb nur Overrides, `dirty` blieb | `12_tui_state.rs`: `export_package_use` + `refresh` |
| Falsche CONFLICTs | Baseline-Konflikte wurden auf jedes Delta kopiert | `08_delta.rs`: `noop_delta`, `build_delta` |
| Filter ohne Navigation | Filter-Modus kannte nur Tippen/Esc | `14_tui_runner.rs`: `dispatch_key` |
| `Voll-Ersparnis`-Rauschen | 3 Simulationen pro Flag, falsche Löschvorschläge | `08_delta/shared.rs`: `shared_flag_deps` statt `shared_removal_impact` |
| `package.use`-Reihenfolge | Keys wurden alphabetisch sortiert (`>` vor `a`) | `02_profile.rs` + `02_profile/stack.rs` |

## 3. Station für Station

### 3.1 `package.use`: Reihenfolge zählt (`02_profile`)

Portage wendet `package.use`-Zeilen der Reihe nach an: Spätere Zeilen
gewinnen. Der alte Code sammelte erst alle Atome in eine Map und sortierte
dann die Keys — dabei landete `>=app/foo-2.0` (Zeichen `>`, 0x3E) immer vor
`app/foo`, egal was in der Datei stand.

Jetzt gibt es zwei Schichten:

- `read_package_use_lines` (`02_profile/stack.rs`) liest Datei und Zeilen in
  exakter Reihenfolge ein — ein simpler `(Atom, Flags)`-Vektor.
- `Profile` trägt dieses Wissen als neues Feld `package_use_lines`.
  `sync_pattern_caches` baut den Index daraus; nur wenn das Feld leer ist
  (reine Unit-Tests ohne Datei), greift die alte alphabetische Ordnung.

Zusätzlich wurde `effective_defaults` in `effective_defaults_inner(...,
include_package_use)` ausgelagert. Der neue Zwilling
`effective_defaults_without_package_use` rechnet die Profil-Baseline **ohne**
`package.use` — das ist der Vergleichsmaßstab für den Export (nächster
Abschnitt).

*Selbst nachprüfen:* Der Test `package_use_file_order_precedence`
(`02_profile/tests.rs`) füttert beide Zeilenfolgen und prüft die
Token-Reihenfolge für Version 2.0 vs. 1.0.

### 3.2 Export, der die Wahrheit schreibt (`12_tui_state.rs`)

Der alte Export schrieb stumpf die TUI-Overrides (`extra` an → `extra` in die
Datei). Alles, was schon in `/etc/portage/package.use` stand (`sound-server`
bei `pipewire`), fiel unter den Tisch — und `dirty` blieb `true`, also klebte
`*ungespeichert*` ewig in der Fußzeile.

Der neue Export denkt andersherum. Für jedes angefasste Paket vergleicht er:

- `final_active` — was nach Profil **plus** `package.use` **plus** TUI-Toggles
  gerade gilt, gegen
- `profile_only` — was das reine Profil ohne `package.use` sagen würde.

Nur die Differenz landet in der Datei, nur für echte IUSE-Flags (tote Flags
wie `alsa`/`udev` fallen automatisch raus, maskierte/erzwungene und
Plattform-Flags ebenfalls). Ergebnis für das Pipewire-Szenario aus dem Bericht:
`media-video/pipewire extra sound-server` — exakt die lebenden Abweichungen.

Danach setzt der Export `dirty = false`. Und `refresh()` löscht einen Status,
der mit `package.use geschrieben:` beginnt, sobald du weiter toggelst — die
Fußzeile atmet wieder mit.

*Selbst nachprüfen:* `export_merges_valid_package_use_cleans_dead_and_clears_dirty`
spielt genau dieses Szenario durch, inklusive Folge-Toggle.

### 3.3 Ehrliche Konflikte (`08_delta.rs`)

Zwei Stellen logen:

- `noop_delta` (bewiesen wirkungslose Toggles) kopierte einfach den ersten
  Baseline-Konflikt auf das Delta → jetzt `conflict: None`.
- `build_delta` nahm den ersten Konflikt der Simulation — also auch solche,
  die schon vorher existierten → jetzt wird nur ein **neuer** Konflikt
  gemeldet (Vergleich über Paket + Nachricht gegen die Baseline).

Der Effekt aus dem Bericht: `python_single_target_python3_12 [ ] 0 B ...
CONFLICT` auf unbeteiligten Flags ist verschwunden; echte Toggle-Konflikte
(wie `gtk+[X]` vs. `firefox-bin`) bleiben sichtbar.

*Selbst nachprüfen:* `unrelated_conflict_does_not_poison_other_flags` baut den
Konflikt auf Paket A und prüft alle Flags von Paket B auf `conflict: None`.

### 3.4 Abschied von `Voll-Ersparnis` (`08_delta/shared.rs`, CLI)

`shared_removal_impact` rechnete pro Flag **drei volle Simulationen**, um eine
„maximale Ersparnis“ zu schätzen — und schlug dabei vor, `firefox-bin` zu
löschen, nur weil es zufällig dieselbe Lib hält. Das war teuer und falsch.

Ersatz ist `shared_flag_deps`: ein reiner 0-ms-Graph-Lookup. Er sammelt, welche
Abhängigkeiten unter `flag?` hereinkommen, und zeigt nur solche, die mindestens
ein **anderes** Paket ebenfalls hält — inklusive Halterliste. Keine Simulation,
keine Löschvorschläge, nur Fakten für die rechte Pane.

Mit der Analyse starben auch `SharedRemovalImpact`, `--deep` (`deltas --deep`
gibt es nicht mehr) und drei zugehörige Unit-Tests. Ein `cut_package`-Test auf
dem Diamond-Graphen sichert die Blocker-Logik weiter ab.

*Selbst nachprüfen:* `deltas --help` zeigt kein `--deep` mehr; die rechte Pane
zeigt `Geteilte Abhaengigkeiten` (siehe 3.7).

### 3.5 Der Cursor bleibt, wo du bist (`12_tui_state.rs`, `actions.rs`)

Herzstück ist `rebuild_left_preserving`: Vor jedem Neuaufbau wird der alte
Eintrag gemerkt, danach wird **derselbe Eintrag** gesucht und selektiert — nur
wenn er verschwunden ist (Filter!), wird per Index geclamp. Zwei Fliegen mit
einer Klappe:

- Doppelte Welt-Atome (`gentoo-sources:1.0` + `:2.0`) kollabieren durch
  `seen.insert` beim Aufbau auf genau einen Eintrag.
- Nach Toggle + `refresh()` in `ExclusivePackages`-Sortierung folgt der Cursor
  dem Paket an seine neue Position, statt auf dem Index kleben zu bleiben.

Dazu kommt Clamping statt Wrap-around überall (`move_clamped`): `PageDown` am
Listenende bleibt stehen, statt nach oben zu springen. `PageUp`/`PageDown`/
`Home`/`End` gibt es jetzt als `move_to_edge`/`page_step`
(`viewport_height − 9`, mindestens 3).

**Bewusste Abweichung vom Plan:** `apply_filter_only` restauriert per Index,
nicht per Paket. Der Filter ändert die sichtbare Menge — beim Tippen und
Löschen soll die Auswahl der Position folgen (so bleibt der bestehende Test
`filter_switch_selection_reloads_middle` grün). Paket-Restore gilt für
`refresh()` und Sortierung.

*Selbst nachprüfen:* `sort_toggle_keeps_selection_on_same_package` (Toggle
dreht die Sortierung um, Cursor bleibt) und
`duplicate_world_atoms_are_deduplicated`.

### 3.6 Tasten, die tun, was draufsteht (`14_tui_runner.rs`)

- **Im Filter-Modus** (`state.filtering`): `Up`/`Down`/`PageUp`/`PageDown`/
  `Home`/`End` navigieren links weiter (statt alles zu schlucken), `Tab`
  springt direkt nach `Mitte`, `Esc` leert den Filter komplett.
- **Normal-Modus**: `PageUp`/`PageDown`/`Home`/`End` wirken in allen Panes,
  `Space` links fokussiert nur nach `Mitte` (toggelt nichts mehr),
  `Space` in der Mitte toggelt wie bisher.
- Vor jedem Zeichnen übernimmt die Loop die echte Terminalhöhe in
  `viewport_height`, damit `page_step` stimmt.

*Selbst nachprüfen:* `filter_allows_navigation_tab_and_esc_clear`,
`space_on_left_focuses_middle_without_toggling`,
`page_up_down_clamps_without_wrap` (alle in `14_tui_runner.rs`).

### 3.7 Ruhige Mitte, ehrliche Rechte (`13_tui_view/`)

- **Mitte** (`middle.rs`): Der Scroll-Offset lebt jetzt in
  `state.middle_scroll` (eine `Cell`, damit das reine Rendern ihn pflegen
  kann). Die Auswahl wandert nur so weit wie nötig — kein Flackern mehr beim
  Durchsteppen.
- **Rechts** (`right.rs`): `render_shared_impact` ist ersatzlos gestrichen.
  Dafür gibt es den Abschnitt `Geteilte Abhaengigkeiten` aus
  `shared_flag_deps`, korrigierte Farben (`+` rot = Mehrkosten, `−` grün =
  Ersparnis) und eine Hervorhebung (`> `, fett) des in `Pane::Right`
  fokussierten Ziels — der Scroll folgt der Markierung in den sichtbaren
  Bereich. Sobald ein `cut_plan` vorliegt, teilt sich die Pane: oben
  Blame/Bewirkt/Doku, unten eine eigene `Cut-Analyse`-Box. Der alte
  `target != pkg`-Filter ist weg: Der Auto-Cut gilt für **jedes** in
  `Pane::Right` angesteuerte Paket (`compute_cut_plan` nutzt dort
  `selected_right_target`, `ensure_deltas` lädt im Idle nach).
- **Hilfe** (`bottom.rs`): Die `Voll-Ersparnis`-Zeile wurde ersetzt durch
  `Rechte Pane: j/k waehlt Pakete + Auto-Cut [x]; PgUp/PgDn blaettert`.

*Selbst nachprüfen:* `renders_shared_deps_and_flag_docs` (Doku + geteilte
Deps, kein `Voll-Ersparnis`) und das Screencap-Szenario
`03_emacs_shared_blocker` → `Tab` → `03b_cut_plan` (Auto-Cut ohne `x`).

## 4. Wie verifiziert wurde

1. **Lokal, debug + release**: `cargo test` und `cargo test --release`
   vollständig grün (111 Unit-Tests inkl. 9 neuer Regressionstests, dazu
   Screencap-, HIL-, Host- und Perf-Suiten). `cargo clippy` und
   `cargo build --release` ohne Warnungen.
2. **Echtdaten**: Im Container-Image `gentoo-z6-min-openrc` (echtes
   Gentoo-Repo, 114 Welt-Pakete) liefen `deltas media-video/ffmpeg`,
   `graph --json` (555 Pakete, valides JSON, Exit 0) und
   `cut x11-libs/gtk+ --json` (Exit 0) sowie die HIL-Suiten
   (`hil_regression`: 4/4, `tui_screencaps` HIL-Snapshot: 1/1).
   Plausibilitätsprüfung der Screencaps: `gentoo-sources` pro Frame genau
   einmal, kein `Voll-Ersparnis`, `Geteilte Abhaengigkeiten` vorhanden.

Hinweis zur Umgebung: Ein SSH-Zugang zum Gentoo-Host war von hier aus nicht
erreichbar (kein offener SSH-Port auf den Nachbaradressen); die
Systemtests liefen stattdessen per `docker exec` im Image-Container — mit
demselben Datensatz, den der Plan vorsieht. Es wurde keine IP ins Repo
geschrieben.

## 5. Was du dir als Nächstes ansehen könntest

- `cut_cache` + `cut_loading` als Muster für weitere Idle-Nachladungen.
- Die `Cell<u16>`-Tricks (`middle_scroll`) sind pragmatisch, aber kein
  Stilvorbild — falls der State mal unter `&mut`-Render umgebaut wird, dort
  zuerst aufräumen.
- `PkgFixture::slot` (nur für Tests) existiert einzig für den
  `:1.0`/`:2.0`-Dedup-Test.
