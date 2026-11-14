# gentoo-dep-tui — `ncdu` für Gentoo-USE-Flags

Du änderst ein USE-Flag (z. B. `svt-av1` bei `ffmpeg`) und willst **vorher**
wissen: Wie viele Pakete kommen dazu, welche genau, wie groß wird es — und
löst das Flag einen `REQUIRED_USE`-Konflikt aus? Genau das beantwortet
`gentoo-dep-tui` offline anhand der Gentoo-Metadaten (`md5-cache`), ohne dass
du `emerge` laufen lassen musst. Und wenn du fragst „Wie werfe ich
`x11-libs/gtk+` ganz raus, und was spare ich dadurch?“ — dafür gibt es die
Cut-Analyse.

## Wo sucht das Programm seine Dateien?

| Was? | Standard-Pfad | Flag zum Überschreiben |
| :--- | :--- | :--- |
| `world`-Datei | `/var/lib/portage/world` | `--world <PFAD>` |
| Gentoo-Repo | `/var/db/repos/gentoo` | `--repo <PFAD>` |
| Profil | `default/linux/amd64/23.0` (relativ zu `<repo>/profiles`; ohne `--profile` wird `<portage-dir>/make.profile` aufgelöst) | `--profile <NAME>` |
| Portage-Verzeichnis | `/etc/portage` (liefert `make.conf`, `package.use`, `package.mask`, `package.unmask`, `package.accept_keywords`, `make.profile`) | `--portage-dir <PFAD>` |
| `make.conf` | aus `--portage-dir` | `--make-conf <PFAD>` (gewinnt) |
| `package.use` | aus `--portage-dir` | `--package-use <PFAD>` (gewinnt) |
| Snapshot-Root | *(keiner)* | `--snapshot <PFAD>` (setzt `<PFAD>/repo`, `<PFAD>/world`, `<PFAD>/etc/portage` bzw. `make.conf`/`package.use`) |
| Binhost-Index | *(keiner — optional!)* | `--binhost <PFAD/zur/Packages>` |

**Ohne `--binhost`** steht bei `bin` überall `n/a (ohne --binhost)` bzw. in
der TUI `kein Binpkg (Source-Build)` — achte dann auf die `dl`-Werte
(Quell-Downloads aus den `Manifest`-Dateien) und auf die Paket-Deltas.
`BDEPEND`-Pakete (nur zur Bauzeit nötig) werden getrennt als `Build-Only`
gezählt und verfälschen die Runtime-Größe nicht.

### Empfohlener Default-Binhost

Das Tool liest eine lokale `Packages`-Indexdatei. Guter Default ist der
[offizielle Gentoo-Binhost](https://wiki.gentoo.org/wiki/Gentoo_Binary_Host_Quickstart)
(für amd64/23.0):

```bash
wget -O /tmp/Packages \
  https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/x86-64/Packages
gentoo-dep-tui tui --repo ... --world ... --binhost /tmp/Packages
```

Ob der Index zu deinem Repo-Stand passt, zeigt `index-check` als
`Binhost-Treffer`-Quote; unter ~80 % lohnt ein frischer Download.

**Im Docker-Container** (kein `/var/db/repos/gentoo`, keine `world`) musst du
die Pfade per Flag übergeben, z. B. auf einen Snapshot unter `/tmp/gentoo-hil`.
Prüfe zuerst mit `index-check`, ob alle Pfade stimmen:

```bash
cargo run --release -- index-check --snapshot /tmp/gentoo-hil \
  --profile default/linux/amd64/23.0/no-multilib
```

## TUI-Schnellstart

```bash
gentoo-dep-tui tui --repo ... --world ... [weitere Flags wie oben]
# Ohne Subcommand startet automatisch die TUI.
```

Aufbau: **links** deine `world`-Pakete, dahinter `||`-Alternativen und
transitive Abhängigkeiten. **Mitte**: alle USE-Flags des gewählten Pakets mit
Marginal-Delta (`bin`/`dl`, `(+N/-M Pakete)` bei Paket-Differenz) oder
`||`-Kandidaten. **Rechts**: Blame-Kette („Wer zieht das rein?"),
Konflikte, Paket-Differenz des selektierten Flags, Flag-Doku und die
Voll-Ersparnis-Analyse.

| Taste | Wirkung |
| :--- | :--- |
| `1` / `2` | Tab Pakete / Big Levers (Tab 2 berechnet lazy beim ersten Besuch) |
| `Tab` | Pane wechseln (links ↔ Mitte ↔ rechts) |
| `↑`/`↓`, `j`/`k` | Bewegen (rechts scrollt den Text; alle Listen folgen der Selektion) |
| `/` | Filtern (nur Liste, kein Re-Resolve — sofort; `Esc` löscht zuerst den Filter) |
| `o` | Sortierung links wechseln: Standard → Exkl. Pakete → Exkl. Größe (kein Re-Resolve; zeigt `(N pkg, X)` je Paket) |
| `?` / `F1` | Hilfe-Overlay (`Esc`/`?`/`q` schliesst) |
| `Space` | Flag toggeln (Mitte) · Big Lever global toggeln (Tab 2) |
| `Enter` | `||`-Alternative durchschalten · Konflikt öffnen |
| `x` | Cut-Analyse fürs selektierte Paket (rechte Pane) |
| `a` | Alle Flag-Blocker des Cut-Plans auf einmal anwenden |
| `u` | Letzte Änderung zurückrollen (Undo, bis 32 Schritte) |
| `S` | Geänderte Flags als `/etc/portage/package.use`-Syntax schreiben (Default `/tmp/gentoo-dep-tui.package.use`, per `--out-use` wählbar) |
| `Esc` | Filter löschen → Pane links → beenden |
| `q` | Beenden |

## Zahlen & Metriken richtig lesen

Fußzeile: `Binpkg <X> (<hits>/<total> Hits)` summiert komprimierte
Binhost-`SIZE`-Werte über alle Runtime-Pakete — Pakete **ohne** Treffer
zählen mit `0 B` (Trefferquote beachten; ohne `--binhost` steht `n/a`).
`Quell-DL` enthält ausschließlich Quell-Tarballs (`Manifest`-`DIST`-Summen).
`Build-Only` fasst Buildzeit-Pakete (`DEPEND`/`BDEPEND`/`IDEPEND`) zusammen.

Mitte: `Pkg-Quell-DL` ist der Quell-Tarball des Pakets **selbst** — nicht der
ganze Subgraph. `0 B`-Deltas sind normal bei geteilten Abhängigkeiten
(z. B. bleibt `gtk+` über `firefox-bin` im Graphen) oder bei reinen
Compile-Features ohne neue Pakete/Distfiles. `CONFLICT` markiert Flags, deren
Toggle einen Widerspruch auslösen würde — `Space` öffnet dann einen
Quick-Switch statt still zu scheitern.

Rechte Pane: Bei geteilten Abhängigkeiten zeigt **Voll-Ersparnis**, was das
Abschalten *überall* (`Flag aus bei: …`) bzw. zusätzlich mit Entfernen von
Welt-Paketen (`world entfernen: …`) sparen würde. Steht beim selektierten
Flag ein `Toggle-Konflikt`, siehst du Grund und Verursacher sofort — nicht
erst im Modal.

Tab 2 (`Big Levers`): standardmäßig nur **Abschalten** (`[AUS]`), sortiert
nach Paket-Ersparnis (`Pkg-Delta`), danach Download — ohne `--binhost`
stehen die größten Paket-Einsparungen oben statt lauter `0 B`-Zeilen. Die
Detailbox unten nennt Beschreibung, betroffene Pakete und hinzu/entfernte
Pakete. Hinweis: `dl` kann beim Abschalten *steigen* (`!flag?`-Bedingungen
ziehen dann Fallback-Quellen) — korrekt, kein Bug.

USE-Flag-Bedeutungen stehen offline in der TUI (rechte Pane): lokale
Beschreibung aus `<repo>/profiles/use.local.desc` vor globaler aus
`profiles/use.desc` bzw. `profiles/desc/*.desc`.

## Drei Anwendungsfälle Schritt für Schritt

### 1. `ffmpeg`: Was kostet AV1?

```bash
gentoo-dep-tui deltas media-video/ffmpeg --repo ... --world ...
# [ ] svt-av1  (n/a (ohne --binhost) / +8000 B dl (+1/-0 Pakete: media-libs/svt-av1))
```

`(+1/-0 Pakete: …)` nennt die exakte Paket-Differenz. In der TUI: `/ffmpeg`
→ `Space` auf `svt-av1` → Fußzeile (`Pakete N`) und rechte Spalte zeigen das
neue Paket. Hinweis: Die REQUIRED_USE-Kaskade `svt-av1? ( aom )` schaltet
`aom` ggf. mit ein — das Delta enthält dann beide Pakete.

### 2. `gtk+` rauswerfen: Was spart das wirklich?

```bash
gentoo-dep-tui cut x11-libs/gtk+ --repo ... --world ...
# Cut-Analyse fuer x11-libs/gtk+:
#   keine Flag-Blocker
#   Harte world-Blocker: www-client/firefox-bin, ...
#   Ohne alle Blocker: ... (-7/+0 pkg)
```

Niemand hält `gtk+` über ein schaltbares Flag (alle Kanten unbedingt) — also
gibt es keine Flag-Blocker, nur harte Welt-Blocker. In der TUI: Paket
selektieren, `x` zeigt den Plan rechts; `a` wendet alle Flag-Blocker an
(hier: nichts zu tun), `u` rollt zurück. Echte Messung auf einem
Desktop-System: nur echte Halter (`firefox-bin`, `signal-desktop-bin`,
`google-chrome`), **keine** unbeteiligten Basispakete — und −585 MB Download
bei 7 entfernten Paketen.

### 3. Schnittstelle für AI-Agenten: `simulate` und `batch`

Statt den Index pro Frage neu zu laden (`~650 ms` bei ~14.000 Paketen),
lädst du ihn einmal und stellst viele Fragen:

```bash
# Einzelne Was-wäre-wenn-Frage als JSON:
gentoo-dep-tui simulate --set app-editors/emacs=gui,Xaw3d,-gtk --json

# Dutzende Fragen in Sekundenbruchteilen (eine Zeile = eine Antwort):
printf '%s\n' \
  '{"op":"deltas","package":"media-video/ffmpeg"}' \
  '{"op":"cut","package":"x11-libs/gtk+"}' \
  '{"op":"simulate","set":{"app-editors/emacs":["-gtk"]}}' \
  | gentoo-dep-tui batch
```

Alle Lese-Subcommands (`index-check`, `graph`, `blame`, `deltas`, `cut`,
`big-levers`, `simulate`) kennen `--json` (maschinenlesbar; `--perf-report`
geht dann nach `stderr`, damit `stdout` reines JSON bleibt). `graph --json`
liefert Totals (inkl. `build_extra`, Trefferquote) plus je Knoten CPV, Slot,
`has_binpkg`, USE-Flags und exklusive Kosten.

## CLI-Überblick

`index-check` (Repo-Statistik) · `graph` (Gesamtgrößen + Paketliste) ·
`blame <pkg>` (Abhängigkeitsketten, Welt-Pakete zeigen sauber „kein Pfad") ·
`deltas <pkg>` (Flag-Deltas mit Paket-Diff; `--deep` zusätzlich mit
Shared-Blocker-Bericht) · `cut <pkg>` (Paket-Eliminierungs-Analyse) ·
`big-levers` (systemweite Hebel, multi-core; `--include-enable` zeigt auch
Einschalt-Hebel) · `simulate` (`--set cat/pkg=flag1,-flag2`, `--drop
cat/pkg`) · `batch` (JSON-Zeilen von `stdin`) · `tui` (Default).

Alle Lese-Subcommands kennen `--perf-report` (Phasen-Report; misst nur mit
`--features perf-metrics`, sonst Hinweis).

## Performance (gemessen, `tests/perf_system.rs`)

Synthetisches Repo: 125 Pakete, `ffmpeg`-Simulator mit 60 Flags. Release-Build:

| Test | Ergebnis |
| :--- | :--- |
| `index-load` | 1 ms, 125 Dateien |
| `package-deltas` (60 Flags) | 0 ms, 62 Resolves, **0 `build_extra`** (Runtime-only sims) |
| `big-levers` | 6 ms, 62 Hebel, **1 Thread ≡ Pool** (exakt identisch) |
| TUI-Workflow (10× Cursor, 5× Filter, 3× Toggle, Tab-Wechsel) | alles 0 ms, Budgets (<1 s) mit Reserve eingehalten |

Verfahren: Profil-Baseline je Paket einmalig gecacht (`OnceLock`),
Baseline einmalig pro Refresh (`node_costs` <1 ms bei ~600 Knoten),
Simulationen parallel (`rayon`), `Manifest` einmal je Paketverzeichnis,
Filter/Cursor/Sortierung ohne Resolve (Cache + 80-ms-Idle-Recompute), Big
Levers lazy. Vorher/Nachher-Vergleich und Latenz-Budgets stehen als Tests in
`tests/perf_system.rs`
(`cargo test --release --test perf_system -- --nocapture`).

Detaillierte Phasen-Zeiten und Counter gibt es mit
`cargo test --release --features perf-metrics --test perf_system --
--nocapture` (Feature `perf-metrics`: null Overhead im Standard-Build;
`--perf-report` an den Lese-Subcommands druckt denselben Report).

Skriptbare TUI-Tests mit Text-Screenshots: `cargo test --test tui_screencaps`
fährt das Review-Szenario (Suche, Shared-Blocker, Big Levers, Cut, Sort,
Hilfe, Konflikt) gegen `ratatui::TestBackend` und sichert alle Frames nach
`target/screencaps/review_screencaps.md` (Ablage via
`GENTOO_SCREENCAP_OUT=<pfad>` wählbar); gegen echten Snapshot optional mit
`GENTOO_HIL_REPO=…` (`hil_screencaps.md`).

**Echte Messung auf einem Gentoo-System** (32 Kerne, ~13.700 Pakete im
Index, 651 Knoten im Graphen):

| Befehl | Befund |
| :--- | :--- |
| `index-check` | 645 ms Ladezeit (33.945 Cache-Dateien) |
| `graph --json` | 651 Pakete, 9847 MB Quell-DL; mit Binhost 1032 MB Binpkg (561/651 Treffer) |
| `cut x11-libs/gtk+ --json` | nur echte Halter, −585 MB DL / −7 Pakete |
| `deltas media-video/ffmpeg --json` | 101 Flags, 63 mit Wirkung, exakte Konfliktgründe |
| `big-levers --json` | 391 Hebel, alle `[AUS]`, Top `rdw` −13 Pakete |
| `batch` (6 Ops, 1 Index-Load) | ~0,6 s Wandzeit |
