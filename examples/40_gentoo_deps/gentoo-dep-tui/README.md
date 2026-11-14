# gentoo-dep-tui — `ncdu` für Gentoo-USE-Flags

Du änderst ein USE-Flag (z. B. `svt-av1` bei `ffmpeg`) und willst **vorher**
wissen: Wie viele Pakete kommen dazu, welche genau, wie groß wird es — und
löst das Flag einen `REQUIRED_USE`-Konflikt aus? Genau das beantwortet
`gentoo-dep-tui` offline anhand der Gentoo-Metadaten (`md5-cache`), ohne dass
du `emerge` laufen lassen musst.

## Wo sucht das Programm seine Dateien?

| Was? | Standard-Pfad | Flag zum Überschreiben |
| :--- | :--- | :--- |
| `world`-Datei | `/var/lib/portage/world` | `--world <PFAD>` |
| Gentoo-Repo | `/var/db/repos/gentoo` | `--repo <PFAD>` |
| Profil | `default/linux/amd64/23.0` (relativ zu `<repo>/profiles`; ohne `--profile` wird `/etc/portage/make.profile` aufgelöst) | `--profile <NAME>` |
| `make.conf` | `/etc/portage/make.conf` | `--make-conf <PFAD>` |
| `package.use` | `/etc/portage/package.use` | `--package-use <PFAD>` |
| Binhost-Index | *(keiner — optional!)* | `--binhost <PFAD/zur/Packages>` |

**Ohne `--binhost`** zeigt das Programm bei `bin` überall `n/a
(ohne --binhost)` — achte dann auf die `dl`-Werte (Quell-Downloads aus den
`Manifest`-Dateien). Die Binpkg-Größe ist der bessere Maßstab für die echte
Laufzeitgröße; `dl` zählt nur herunterzuladende Quell-Tarballs.
`BDEPEND`-Pakete (nur zur Bauzeit nötig, z. B. `dev-lang/go` bei `caddy`)
werden getrennt als `BDEPEND-extra` gezählt und verfälschen die
Runtime-Größe nicht.

### Empfohlener Default-Binhost

Das Tool liest eine lokale `Packages`-Indexdatei. Guter Default ist der
[offizielle Gentoo-Binhost](https://wiki.gentoo.org/wiki/Gentoo_Binary_Host_Quickstart)
(für amd64/23.0):

```bash
wget -O /tmp/Packages \
  https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/x86-64/Packages
gentoo-dep-tui tui --repo ... --world ... --binhost /tmp/Packages
```

Hinweise: Der Pfad muss zum Profil passen (`23.0/x86-64` für Standard-amd64;
für `hardened`, `musl`, `no-multilib` o. Ä. gibt es eigene Unterverzeichnisse
bzw. keinen exakt passenden Binhost — die `SIZE`-Werte bleiben dann ein
Näherungs-Proxy). Ob der Index zu deinem Repo-Stand passt, zeigt
`index-check` als `Binhost-Treffer`-Quote; unter ~80 % lohnt ein frischer
Download. Die `USE`-Flags der Binpkgs müssen nicht deinen entsprechen —
für reine Größen-Deltas ist das egal.

**Im Docker-Container** (kein `/var/db/repos/gentoo`, keine `world`) musst du
die Pfade per Flag übergeben, z. B. auf einen Snapshot unter `/tmp/gentoo-hil`.
Prüfe zuerst mit `index-check`, ob alle Pfade stimmen:

```bash
cargo run --release -- index-check --repo /tmp/gentoo-hil/repo \
  --world /tmp/gentoo-hil/world --make-conf /tmp/gentoo-hil/make.conf \
  --package-use /tmp/gentoo-hil/package.use \
  --profile default/linux/amd64/23.0/no-multilib
```

## TUI-Schnellstart

```bash
gentoo-dep-tui tui --repo ... --world ... [weitere Flags wie oben]
# Ohne Subcommand startet automatisch die TUI.
```

Aufbau: **links** deine `world`-Pakete, dahinter `choice:`-Gruppen aus
`plan.toml`, `||`-Alternativen und transitive Abhängigkeiten. **Mitte**: alle
USE-Flags des gewählten Pakets mit Marginal-Delta (`bin`/`dl`,
`(+N/-M Pakete)` bei Paket-Differenz) oder `||`-Kandidaten bzw.
Choice-Optionen. **Rechts**: Blame-Kette („Wer zieht das rein?"), `Pros`/`Cons`
der Choice und die Paket-Differenz des selektierten Flags.

| Taste | Wirkung |
| :--- | :--- |
| `1` / `2` | Tab Pakete / Big Levers (Tab 2 berechnet lazy beim ersten Besuch) |
| `Tab` | Pane wechseln (links ↔ Mitte ↔ rechts) |
| `↑`/`↓`, `j`/`k` | Bewegen (rechts scrollt den Blame-Text; alle Listen folgen der Selektion) |
| `/` | Filtern (nur Liste, kein Re-Resolve — sofort; `Esc` löscht zuerst den Filter) |
| `?` / `F1` | Hilfe-Overlay (Metriken, Navigation, Ablauf; `Esc`/`?`/`q` schliesst) |
| `Space` | Flag toggeln (Mitte) · Big Lever global toggeln (Tab 2) · Choice-Option wechseln |
| `Enter` | `\|\|`-Alternative durchschalten · Choice-Option wechseln · Konflikt öffnen |
| `S` / `E` | In `plan.toml` speichern / als Portage-Dateien exportieren (`E` synct vorher) |
| `V` | Export synchron halten (bei Bedarf auto-`Export`) und gegen `emerge --pretend` verifizieren |
| `Esc` | Filter löschen → Pane links → beenden |
| `q` | Beenden |

## Zahlen & Metriken richtig lesen

Fußzeile: `Binpkg <X> (<hits>/<total> Hits)` summiert komprimierte
Binhost-`SIZE`-Werte über alle Runtime-Pakete — Pakete **ohne** Treffer im
Binhost-Index zählen mit `0 B` (die echte Größe wird dann unterschätzt;
Trefferquote beachten, ohne `--binhost` steht `n/a`). `Quell-DL` enthält
ausschließlich Quell-Tarballs (`Manifest`-`DIST`-Summen), **keine**
Binärpakete. `Build-Only` fasst Buildzeit-Pakete
(`DEPEND`/`BDEPEND`/`IDEPEND`) als `bin`/`src` zusammen.

Mitte: `Pkg-Quell-DL` ist der Quell-Tarball des Pakets **selbst** — nicht der
ganze Subgraph. `0 B`-Deltas sind normal bei geteilten Abhängigkeiten
(z. B. bleibt `gtk+` über `firefox-bin` im Graphen, Refcount ≥ 2) oder bei
reinen Compile-Features ohne neue Pakete/Distfiles.

Rechte Pane: `Flag <x> zieht:` listet hinzukommende/entfallende Pakete. Wird
eine gezogene Abhängigkeit von anderen gehalten, zeigt **Voll-Ersparnis**,
was das Abschalten *überall* (`Flag aus bei: …`) bzw. zusätzlich mit
Entfernen von Welt-Paketen (`world entfernen: …`) sparen würde —
`deltas <pkg> --deep` gibt denselben Bericht auf der Konsole.

Tab 2 (`Big Levers`): globales Abschalten je Flag, sortiert nach Ersparnis;
die Detailbox unten nennt Beschreibung, betroffene Pakete und
hinzu/entfernte Pakete. Hinweis: `dl` kann beim Abschalten *steigen*
(`!flag?`-Bedingungen ziehen dann Fallback-Quellen) — korrekt, kein Bug.

USE-Flag-Bedeutungen stehen offline in der TUI (rechte Pane): lokale
Beschreibung aus `<repo>/profiles/use.local.desc` vor globaler aus
`profiles/use.desc` bzw. `profiles/desc/*.desc`.

## Drei Anwendungsfälle Schritt für Schritt

### 1. `ffmpeg`: Was kostet JPEG→AV1?

```bash
gentoo-dep-tui deltas media-video/ffmpeg --repo ... --world ... [...]
# [ ] svt-av1  (n/a (ohne --binhost) / 8000 B dl (+1/-0 Pakete: media-libs/svt-av1))
```

`(+1/-0 Pakete: …)` nennt die exakte Paket-Differenz. In der TUI: `/ffmpeg`
→ `Space` auf `svt-av1` → Fußzeile (`Pakete N`) und rechte Spalte zeigen das
neue Paket. Hinweis: Die REQUIRED_USE-Kaskade `svt-av1? ( aom )` schaltet
`aom` ggf. mit ein — das Delta enthält dann beide Pakete.

### 2. `openssh`-Alternativen vergleichen (`[[choice]]`)

`plan.toml` anlegen (`init --world … --plan plan.toml`), Baseline-Welt ohne
Choice-Atome halten und Alternativen eintragen:

```toml
[[choice]]
id = "ssh-server"
selected = 0
[[choice.option]]
atom = "net-misc/openssh"
pros = "volles OpenSSH, SFTP"
cons = "zieht OpenSSL/zlib, groesser"
[[choice.option]]
atom = "net-misc/dropbear"
pros = "sehr klein, ideal fuer Container"
cons = "kein volles OpenSSH-Featureset"
```

In der TUI erscheint `choice: ssh-server` links; `Enter`/`Space` schaltet die
Option um und aktualisiert Welt, Graph und Deltas live. `baseline.world` wird
dabei nie mit Choice-Atomen vermischt (`S`/`E` bleiben sauber).

### 3. `nginx`-Alternativen vermessen

`gentoo-dep-tui` hat keine eingebaute „Was ersetzt was"-Datenbank (außer
`||`-Gruppen und `[[choice]]`, die **du** pflegst). Trage Kandidaten
(`www-servers/nginx`, `caddy`, `lighttpd`) als `[[choice]]` ein und vergleiche:
Achte auf `BDEPEND-extra` vs. Runtime — `caddy` braucht `dev-lang/go` nur zur
Bauzeit (fällt im Multi-Stage-Build weg), `nginx` behält seine C-Bibliotheken
in der Runtime.

## CLI-Überblick

`index-check` (Repo-Statistik) · `graph` (Gesamtgrößen + Paketliste) ·
`blame <pkg>` (Abhängigkeitsketten) · `deltas <pkg>` (Flag-Deltas mit
Paket-Diff; `--deep` zusätzlich mit Shared-Blocker-Bericht) ·
`big-levers` (systemweite Hebel, multi-core) · `init`
(`world` → `plan.toml`) · `export` · `verify` · `tui` (Default).

Alle Lese-Subcommands (`index-check`, `graph`, `deltas`, `big-levers`)
kennen `--perf-report` (Phasen-Report; misst nur mit
`--features perf-metrics`, sonst Hinweis).

### Verify-Reihenfolge: `[S]`/`[E]` → `[V]`

`verify` braucht einen Export als Configroot. Fehlt das Verzeichnis, gibt es
eine klare Meldung statt `ENOENT` (zuerst `[E]` bzw. `gentoo-dep-tui
export`). Beim Export ergänzt das Tool `make.profile`, `make.conf` und
`repos.conf` aus dem laufenden `/etc/portage` (nur falls dort vorhanden und
im Export fehlend), sodass `emerge --root <out> --config-root <out> @world`
auf einem Gentoo-Host direkt gegen die laufenden Paketdefinitionen läuft.
Außerhalb von Gentoo ist das ein No-Op. In der TUI hält `[V]` den Export
selbst synchron (bei `dirty` oder fehlendem Export erst Export, dann
Verify).

## Performance (gemessen, `tests/perf_system.rs`)

Synthetisches Repo: 125 Pakete, `ffmpeg`-Simulator mit 60 Flags. Release-Build:

| Test | Ergebnis |
| :--- | :--- |
| `index-load` | 1 ms, 125 Dateien |
| `package-deltas` (60 Flags) | 0 ms, 62 Resolves, **0 `build_extra`** (Runtime-only sims) |
| `big-levers` | 6 ms, 62 Hebel, **1 Thread ≡ Pool** (exakt identisch) |
| TUI-Workflow (10× Cursor, 5× Filter, 3× Toggle, Tab-Wechsel) | alles 0 ms, Budgets (<1 s) mit Reserve eingehalten |

Verfahren: Baseline einmalig (`flag_delta_with_base`), Simulationen parallel
(`rayon`), `Manifest` einmal je Paketverzeichnis, Filter/Cursor ohne Resolve
(Cache + Idle-Recompute), Big Levers lazy. Vorher/Nachher-Vergleich und
Latenz-Budgets stehen als Tests in `tests/perf_system.rs`
(`cargo test --release --test perf_system -- --nocapture`).
HIL-Abgleich auf echtem Snapshot: `GENTOO_HIL_REPO=/tmp/gentoo-hil cargo test
--release` (hier nicht verfügbar, Test skippt sauber).

Detaillierte Phasen-Zeiten und Counter gibt es mit
`cargo test --release --features perf-metrics --test perf_system --
--nocapture` (Feature `perf-metrics`: null Overhead im Standard-Build;
`--perf-report` an den Lese-Subcommands druckt denselben Report).

Skriptbare TUI-Tests mit Text-Screenshots: `cargo test --test tui_screencaps`
fährt das Review-Szenario (Suche, Shared-Blocker, Big Levers, Hilfe,
Konflikt) gegen `ratatui::TestBackend` und sichert alle Frames nach
`target/screencaps/review_screencaps.md` (Ablage via
`GENTOO_SCREENCAP_OUT=<pfad>` wählbar); gegen echten Snapshot optional mit
`GENTOO_HIL_REPO=…` (`hil_screencaps.md`).

**Echte Messung auf dem Gentoo-Host** (32 Kerne, 13.700 Pakete im Index,
156 `world`-Einträge → 570 Knoten im Graphen; Stand 2026-11-13):

| Befehl | Wandzeit | CPU-Zeit | Befund |
| :--- | :--- | :--- | :--- |
| `graph` | 0,4 s | 1,0 s | Basisauflösung unkritisch |
| `deltas media-video/ffmpeg` (~100 Flags) | 1,2 s | 24,5 s (~20× parallel) | pro Toggle spürbar, aber ok |
| `big-levers` | **83 s → 7,7 s** | 167 s → 218 s (~28×) | sequentielle Off-Flag-Schleife war der Engpass (Fix: alles parallel) |

Lehre: Die synthetischen 0–6 ms sagten nichts über den Ernstfall. Der
HIL-Lauf fand den echten Engpass (nicht-parallelisierte zweite Schleife),
den die Unit-Tests nicht sehen konnten.
