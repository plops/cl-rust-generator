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
| `/` | Filtern (nur Liste, kein Re-Resolve — sofort) |
| `Space` | Flag toggeln (Mitte) · Big Lever global toggeln (Tab 2) · Choice-Option wechseln |
| `Enter` | `\|\|`-Alternative durchschalten · Choice-Option wechseln · Konflikt öffnen |
| `S` / `E` | In `plan.toml` speichern / als Portage-Dateien exportieren (`E` synct vorher) |
| `V` | Export gegen `emerge --pretend` verifizieren |
| `q` / `Esc` | Beenden |

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
Paket-Diff) · `big-levers` (systemweite Hebel, multi-core) · `init`
(`world` → `plan.toml`) · `export` · `verify` · `tui` (Default).

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
