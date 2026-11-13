# Walkthrough: `gentoo-dep-tui` verstehen, bedienen, anwenden

Stand: 2026-11-13, Branch `feature/gentoo-dep-tui`.
Zielgruppe: Du hast ein Gentoo-System (oder einen Snapshot davon), aber keine
Lust, jede USE-Flag-Entscheidung per `emerge --pretend` auszuwürfeln.
Dieses Dokument holt dich ab: erst das Grundprinzip, dann Dateien, dann TUI,
dann drei echte Anwendungsfälle zum Nachmachen.

## 1. Was das Programm für dich tut

Jede USE-Flag-Änderung wirft drei Fragen auf:

1. Zieht das Flag neue Pakete rein — oder ist die Bibliothek eh schon da
   (dann kostet das Flag **0 Byte extra**)?
2. Wie groß ist der Unterschied — als **Quell-Download** (`dl`) und als
   **fertiges Binärpaket** (`bin`)?
3. Löst das Flag einen **`REQUIRED_USE`-Konflikt** aus?

`gentoo-dep-tui` beantwortet alle drei **offline** aus den Gentoo-Metadaten
(`md5-cache`), so wie `ncdu` Festplattenplatz erklärt — nur eben für
Abhängigkeiten und USE-Flags.

```text
  Deine Konfig                   Repo-Daten                    Ergebnis
  ───────────                   ──────────                    ────────
  world             ──┐
  make.conf         ──┼──▶  RepoIndex (md5-cache)  ──▶  Laufzeit-Graph (RDEPEND)
  package.use       ──┤     + Manifest (Download)        ├─ Deltas pro Flag
  Profil (amd64)    ──┘     + Binhost (Binpkg-Größe)     ├─ Blame („wer zieht das rein?")
                                                        └─ Export (world + package.use)
```

Zwei Größen musst du auseinanderhalten:

- **`dl` (Download):** Quell-Tarballs aus den `Manifest`-Dateien. Beispiel:
  Bei `sys-kernel/linux-headers` ist der Kernel-Tarball riesig, das fertige
  Paket winzig — `dl` übertreibt hier.
- **`bin` (Binpkg):** komprimierte Binärgröße aus der Binhost-`Packages`-Datei.
  Das ist der ehrlichere Maßstab für Laufzeitgröße. **Ohne `--binhost` steht
  überall `n/a` — achte dann auf `dl`.** Guter Default:
  `https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/x86-64/Packages`
  (per `wget` laden, per `--binhost` übergeben; Details in `README.md`).
- **`BDEPEND-extra`:** reine Bauzeit-Werkzeuge (`cmake`, `go`, …). Fallen im
  Multi-Stage-Build weg und verfälschen die Runtime-Zahl nicht.

## 2. Wo sucht das Programm seine Dateien?

| Was? | Standard-Pfad | Dein Flag |
| :--- | :--- | :--- |
| `world` | `/var/lib/portage/world` | `--world` |
| Gentoo-Repo | `/var/db/repos/gentoo` | `--repo` |
| Profil | `default/linux/amd64/23.0` (ohne `--profile` wird `/etc/portage/make.profile` aufgelöst) | `--profile` |
| `make.conf` | `/etc/portage/make.conf` | `--make-conf` |
| `package.use` | `/etc/portage/package.use` | `--package-use` |
| Binhost | keiner (optional!) | `--binhost` |

**Im Container** gibt es kein `/var/db/repos/gentoo` und keine `world` —
du *musst* die Pfade angeben (z. B. auf einen Snapshot unter `/tmp/gentoo-hil`).
Stolperfalle Nr. 1: Ohne `--world` bricht das Programm heute mit einer klaren
Fehlermeldung ab (früher lief es still mit 0 Paketen). Prüfe Pfade zuerst mit
`index-check` — es zeigt Paketzahl, Ladezeit und Binhost-Trefferquote.

## 3. Die TUI in fünf Minuten

Start: `gentoo-dep-tui tui --repo … --world … [--plan plan.toml]`
(ohne Subcommand startet die TUI von selbst).

- **Links:** deine `world`-Pakete, dahinter `choice:`-Gruppen, `||`-Alternativen
  und transitive Abhängigkeiten (nach Größe sortiert).
- **Mitte:** alle USE-Flags des gewählten Pakets mit Marginal-Delta
  (`(+N/-M Pakete)` nennt die exakte Paket-Differenz) — oder `||`-Kandidaten
  bzw. Choice-Optionen.
- **Rechts:** Blame-Kette („welches `world`-Paket zieht das über welches Flag
  rein?"), `Pros`/`Cons` der Choice und die Paket-Differenz des Flags.
- **Fußzeile:** Gesamt-`bin`, Gesamt-`dl`, `BDEPEND`, Paket- und Konfliktzähler.

| Taste | Wirkung |
| :--- | :--- |
| `1` / `2` | Pakete / Big Levers (Tab 2 rechnet erst beim ersten Besuch) |
| `Tab` | Pane wechseln |
| `↑`/`↓`, `j`/`k` | bewegen (alle Panes folgen; rechts scrollt der Blame-Text) |
| `/` | filtern — nur die Liste, kein Re-Resolve, sofort |
| `Space` | Flag toggeln · Big Lever global toggeln (Tab 2) · Choice wechseln |
| `Enter` | `\|\|` durchschalten · Choice wechseln · Konflikt öffnen |
| `S` / `E` | speichern / exportieren (`E` nimmt ungespeicherte Toggles mit) |
| `q` | beenden |

Alles bleibt flüssig, weil teure Arbeit entweder parallel läuft (`rayon`),
gecacht ist (Cursor) oder erst auf Abruf passiert (Tab 2, Filter ohne Resolve).

## 4. Anwendungsfall: `ffmpeg` — was kostet JPEG→AV1?

Du willst JPEG-zu-AV1-Konvertierung und fragst dich, was `svt-av1` (oder `aom`)
zusätzlich kostet.

**Schnell-Check über die CLI** (ohne TUI-Wartezeit):

```bash
gentoo-dep-tui deltas media-video/ffmpeg --repo ... --world ... [...]
# [ ] svt-av1  (n/a (ohne --binhost) / 8000 B dl (+2/-0 Pakete: media-libs/aom, media-libs/svt-av1))
```

Du liest: zwei neue Pakete — `svt-av1`, und per REQUIRED_USE-Kaskade
(`svt-av1? ( aom )`) gleich `aom` dazu. Genau diese Kaskade zeigt die TUI
live: `/ffmpeg` → `Space` auf `svt-av1` → Fußzeile (`Pakete N+2`) und rechte
Spalte listen die Neuzugänge.

**Exakter Diff per CLI** (wenn du es ganz genau wissen willst):

```bash
gentoo-dep-tui graph --repo ... --world ... > /tmp/vorher.txt
echo "media-video/ffmpeg svt-av1" >> /tmp/test_package.use
gentoo-dep-tui graph --repo ... --world ... \
  --package-use /tmp/test_package.use > /tmp/nachher.txt
diff -u /tmp/vorher.txt /tmp/nachher.txt
```

## 5. Anwendungsfall: `openssh`-Alternativen vergleichen

Zwei Fälle unterscheiden:

- **Fall A (`||`-Gruppe im Tree):** Taucht links als `|| …` auf —
  `Enter` schaltet den Kandidaten um, die Fußzeile zeigt sofort die neue
  Gesamtgröße.
- **Fall B (`openssh` steht direkt in `world`):** Dann kennt kein Ebuild eine
  „Alternative“. Dafür ist **`[[choice]]` in `plan.toml`** da:

```bash
gentoo-dep-tui init --world /pfad/zu/world --plan plan.toml
```

```toml
[[choice]]
id = "ssh-server"
selected = 0
[[choice.option]]
atom = "net-misc/openssh"
pros = "volles OpenSSH, SFTP, Kerberos/PAM"
cons = "zieht OpenSSL/zlib, groesser"
[[choice.option]]
atom = "net-misc/dropbear"
pros = "sehr klein, ideal fuer Container"
cons = "kein volles OpenSSH-Featureset"
```

Wichtig: `baseline.world` enthält **keine** Choice-Atome — Optionen kommen
ausschließlich aus `[[choice]]`, sonst vermischen sich Basis und Alternativen
(das sichert `sync_plan_from_state` ab). In der TUI steht `choice: ssh-server`
links; `Enter`/`Space` wechselt die Option und aktualisiert Welt, Graph und
Deltas **live**. Früher ging das nur über zwei getrennte `export`+`graph`-Läufe
mit `diff`.

## 6. Anwendungsfall: Was ersetzt `nginx`?

Ernüchternde Klarstellung zuerst: Das Tool hat **keine** eingebaute
„Was-ersetzt-was"-Datenbank (außer `||`-Gruppen aus Ebuilds und deinen
`[[choice]]`-Einträgen). **Du** (oder ein LLM) schlägst Alternativen in
`plan.toml` vor — das Tool rechnet **deterministisch** aus, was jede Variante
auf *deinem* System kostet. Typische Kandidaten unter `www-servers/`:
`nginx` (modular, C-Bibliotheken in der Runtime), `caddy` (Go, großes
Einzel-Binary, aber `dev-lang/go` nur in `BDEPEND-extra` — fällt im
Multi-Stage-Build weg), `lighttpd` (schlank), `apache` (schwergewichtig).

Vorgehen wie in Abschnitt 5: Favoriten als `[[choice]]` eintragen, per
`Space`/`Enter` durchschalten und in der Fußzeile vergleichen — achte dabei
gezielt auf die Zeile `BDEPEND-extra` vs. `Pakete`/`Binpkg`. Mit
`gentoo-dep-tui deltas www-servers/nginx` prüfst du danach, welche
`nginx`-Flags (`http2`, `ssl`, `pcre`, …) auf deinem System überhaupt noch
Pakete ziehen und welche längst kostenlos sind.

## 7. Was die Review-Runde geändert hat (kurz)

- **Schneller:** Baseline-einmalig statt je-Flag-neu, `rayon` über alle Kerne,
  kein `BDEPEND` in Simulationen, `Manifest` einmal je Paket, Filter/Cursor
  ohne Resolve, Big Levers lazy. Messwerte (`tests/perf_system.rs`):
  `package_deltas` (60 Flags) 62 Resolves bei 0 `build_extra`-Läufen,
  `big_levers` 1 Thread ≡ Pool, TUI-Navigation unter allen Budgets.
- **Bedienbarer:** alles scrollt mit (`ListState`/`TableState`,
  `Paragraph::scroll`), Fokus bleibt links sichtbar, Filter steht im Titel,
  Tab-2-Toggle und Choice-Switcher funktionieren, `E` exportiert den
  aktuellen Stand.
- **Korrekter:** `world`-Fehler statt Schweigen, `make.profile`-Symlink,
  `USE=` aus `make.defaults` inkl. `${USE}`, `-flag`-Negation in
  `use.mask`/`use.force`, versionierte `package.use`-Atome, echte
  `QuickSwitch`-Changes, `verify` mit `--root` + `--config-root`.
- **Nachgemessen (2026-11-13, Gentoo-Host, 32 Kerne, 570 Graph-Knoten):**
  `graph` 0,4 s, `deltas media-video/ffmpeg` 1,2 s (~20× parallel),
  `big-levers` erst 83 s (nur ~2× parallel) → sequentielle Off-Flag-Schleife
  als Engpass gefunden und parallelisiert → **7,9 s (~27×)**. Details in
  `gentoo-dep-tui/README.md`.
- **Offen:** `src/`-Dateien über ~600 Zeilen warten noch auf Aufteilung.
