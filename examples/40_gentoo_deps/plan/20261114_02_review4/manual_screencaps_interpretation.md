Die Ausgaben zeigen einen realen Testlauf von `gentoo-dep-tui` auf deinem Gentoo-System mit Binhost-Index (`/tmp/Packages`). Das Tool löst deinen aktuellen `world`-Abhängigkeitsgraphen offline auf und simuliert für einzelne oder globale USE-Flag-Änderungen die Auswirkungen auf Paketanzahl, Binärpaket-Größe (`bin`), Quell-Download-Größe (`dl`) und `REQUIRED_USE`-Konflikte.

Im Folgenden zunächst der Gesamtüberblick aus den CLI-Läufen, danach die detaillierte Erklärung der TUI-Screencaps (insbesondere des `emacs`-Falls) sowie einige Unstimmigkeiten/Auffälligkeiten, die direkt in den Captures sichtbar werden.

---

### 1. System-Überblick & CLI-Ausgaben (`index-check`, `graph`, `deltas`, `big-levers`)

#### `index-check` & `graph` (Basisdaten des Systems)
* **Portage-Repository**: `168` Kategorien, `13.674` Pakete, `33.625` `md5-cache`-Dateien (in ~`0,5 s` geladen).
* **Aufgelöster Runtime-Graph (`graph`)**:
  * **`554` Pakete** (in der TUI links stehen **`588` Einträge** — die Differenz von 34 sind `||`-Alternativgruppen und ggf. `choice:`-Einträge, die in der linken Navigationsliste mit angezeigt werden).
  * **`Binpkg: 758,8 MB`** komprimierte Binärpakete (`504/554 Hits` = **91,0 %** Trefferquote im Binhost-Index).
  * *Hinweis zur Trefferquote:* `index-check` meldet über das gesamte Repo nur **`49,1 %`** Binhost-Treffer, im tatsächlich aufgelösten Graphen sind es aber **91 %**. Die verbleibenden `50` Pakete ohne Binhost-Treffer (z. B. `gentoo-sources`, `ltrace`, `motif`, `libmysofa`, `rav1e`, `openh264`) zählen bei `bin` mit `0 B` — hier ist `dl` (Quell-Download) der verlässlichere Indikator.
  * **`Quell-DL: 3313,0 MB`** (Summe der Quell-Tarballs aller Runtime-Pakete).
  * **`Build-Only / BDEPEND-extra: 243,2 MB bin / 1761,3 MB dl`** (reine Bauzeit-Abhängigkeiten, getrennt gehalten).
  * **`Konflikte: 0`**, **`Übersprungen: 1`**: `sys-kernel/gentoo-sources:6.18.57` steht explizit mit diesem Slot in `world`, existiert aber nicht mehr im aktuellen Repo-Index und wird ignoriert.

#### `deltas media-video/ffmpeg` (Marginal-Deltas)
* Alle `~100` USE-Flags von `ffmpeg` werden parallel durchsimuliert (`real 1,07 s` bei `6,80 s` User-CPU-Zeit).
* **Einheiten-Hinweis**: Die CLI gibt exakte Bytes aus (`645120 B`), die TUI rechnet binär um (`1024 B = 1 kB`, also `630.0 kB`).
* **Aktive Flags (`[x]`) mit `0 B bin / 0 B dl`**: `drm`, `fontconfig`, `gnutls`, `gpl`, `lame`, `libass`, `truetype`, `xml`, `zlib`. Diese ziehen entweder keine Pakete oder deren Abhängigkeiten werden bereits von anderen Paketen im Graphen gehalten.
* **Einsparung bei aktivem Flag**: Nur `[x] dav1d` würde beim Ausschalten tatsächlich ein Paket entfernen (`-630 kB bin / -1,0 MB dl`, `-1 pkg: media-libs/dav1d`).
* **Teuerste inaktive Flags (`[ ]`)**:
  * `frei0r`: `+17,3 MB bin / +214,1 MB dl (+5 pkg)` + `KASKADE` (ändert via `REQUIRED_USE` weitere Flags mit).
  * `flite`: `+11,9 MB bin / +19,3 MB dl (+1 pkg)`.
  * `samba`: `+10,1 MB bin / +201,3 MB dl (+14 pkg)` + `KASKADE`.
  * `ocr`: `+6,9 MB bin / +29,9 MB dl (+3 pkg)`.
  * `lv2`: `+6,2 MB bin / +14,1 MB dl (+14 pkg)`.
  * `sofa` (`+198,8 MB dl`), `openh264` (`+57,5 MB dl`), `rav1e` (`+22,0 MB dl`): alle `0 B bin` wegen fehlender Binpkgs im Index.
  * `opencolorio`: markiert mit **`CONFLICT`** (Einschalten verletzt `REQUIRED_USE`).

#### `big-levers` (Systemweite Hebel)
* Durchläuft `700` Flag-Kaskaden / `2102` Resolves in **`3,83 s` Wandzeit** (`44,0 s` CPU-Zeit, ~11× Parallelisierung).
* **Größte Einsparungen (oben, negativ)**:
  * `openmp` (4 Pakete): `-118,3 MB bin / -99,7 MB dl`
  * `llvm` (1 Paket): `-117,5 MB bin / -334,4 MB dl`
  * `gui` (3 Pakete): `-25,7 MB bin / -12,9 MB dl` (aber **4 Konflikte**)
  * `icu` (1 Paket): `-20,9 MB bin / -28,0 MB dl`
  * `static-libs` (104 Pakete): nur `-276 KB bin` (und 3 Konflikte) — global fast wirkungslos für die Binärgröße.
  * `viewer` / `reference`: negatives `Bin-Delta`, aber leicht positives `DL-Delta` (`+163 KB` / `+34,5 KB`), weil `!flag?`-Bedingungen beim Abschalten Fallback-Tarballs ziehen.
* **Wichtige Beobachtung am Tabellenende (positiv)**: Dort stehen z. B. `kde (+132,5 MB)`, `plasma (+129,3 MB)`, `X (+111,3 MB, 16 Pakete, 8 Konflikte)`, `doc (+37,6 MB, 87 Pakete)` sowie `frei0r (+18,1 MB)` und `flite (+12,5 MB)` (exakt die Einschaltkosten aus `ffmpeg`). Entgegen der README-Beschreibung (*„globales Abschalten je Flag“*) testet `big-levers` bei global inaktiven Flags offenbar auch das **Einschalten** (bzw. das globale Toggeln) und sortiert von größter Ersparnis (oben) bis zu den größten Zusatzkosten (unten).

---

### 2. Interpretation der TUI-Screencaps

#### Screencap 1: Startansicht (`sys-kernel/gentoo-sources`)
* `Binpkg: 0 B` (Kernel-Sourcen sind nicht im Binhost), `Pkg-Quell-DL: 153...` MB (Linux-Kernel-Tarball).
* **Besonderheit `[ ] build -6.8 MB bin / -46.2 MB dl (+0/-11 pkg)`**: Das Flag ist *aus*, würde beim *Einschalten* aber 11 Pakete **einsparen**. Das liegt an Gentoo-Ebuilds wie `gentoo-sources`, bei denen `USE="build"` für Stage-Builds gedacht ist und Bauzeit-/Hilfsabhängigkeiten über `!build? (...)` unterdrückt.
* Rechts steht unter `Warum ist das hier?` eine leere `Kette 1:` (dazu unten mehr unter *Auffälligkeiten*).

#### Screencaps 2, 3 und 4: `app-editors/emacs` mit verschiedenen `gui`-/`gtk`-Einstellungen
Diese drei Screenshots zeigen sehr anschaulich, wie verschachtelte `REQUIRED_USE`- und bedingte Abhängigkeiten in Gentoo zusammenspielen:

1. **Zustand A (Screencap 2 — Ausgangszustand: `X [x]`, `Xaw3d [x]`, `gtk [ ]`, `gui [ ]`)**
   * Fußzeile: `554 Pakete`, `758,8 MB`.
   * Obwohl `Xaw3d` bereits `[x]` ist, hat es `0 B` Delta — denn solange `gui` aus ist, baut Emacs ohne grafisches Toolkit und `libXaw3d` wird gar nicht erst gezogen.
   * Erst `[ ] gui` zeigt `+220.0 kB bin / +470.7 kB dl (+1/-0 pkg)` und rechts: `Flag gui zieht: + x11-libs/libXaw3d` (weil `gui` das bereits aktive `Xaw3d` scharf schaltet).

2. **Zustand B (Screencap 3 — `gui` eingeschaltet `[x]`, `Xaw3d [x]`, `gtk [ ]`)**
   * Fußzeile springt wie vorhergesagt auf **`555 Pakete`**, **`759,0 MB`** (`+220 kB` = `x11-libs/libXaw3d` ist jetzt im Graphen).
   * Interessant sind jetzt die Deltas der anderen Toolkit-Flags:
     * `[x] gui` und `[x] Xaw3d` zeigen jeweils `-220.0 kB (-1 pkg)` beim Ausschalten.
     * **`[ ] gtk` zeigt `-220.0 kB bin / -470.7 kB dl (+0/-1 pkg)`**: Einschalten von `gtk` *spart* hier ein Paket! Warum? In `emacs` verdrängt `gtk` das Xaw3d-Toolkit (`libXaw3d` fällt raus: `-1 pkg`), während `x11-libs/gtk+` selbst bereits von anderen Paketen im Graphen (z. B. `firefox-bin`, `wireshark`) gehalten wird und daher `0 B` Zusatzkosten verursacht.
     * **`[ ] motif` zeigt `-220.0 kB bin / +7.9 MB dl (+1/-1 pkg)`**: Derselbe Tausch — `libXaw3d` fällt raus (`-1 pkg`, `-220 kB bin`), dafür kommt `x11-libs/motif` hinzu (`+1 pkg`, `+7,9 MB dl`; da `motif` im Binhost fehlt, ergibt sich beim Binpkg netto `-220 kB`).

3. **Zustand C (Screencap 4 — `gtk` ebenfalls eingeschaltet `[x]`, `gui [x]`, `Xaw3d [x]`)**
   * Fußzeile geht zurück auf **`554 Pakete`**, **`758,8 MB`** (`libXaw3d` ist wieder rausgeflogen).
   * `Xaw3d [x]` bleibt zwar aktiv, kostet aber wieder `0 B` (durch `gtk` übersteuert).
   * **`[x] gtk` zeigt nun `+220.0 kB bin / +470.7 kB dl (+1/-0 pkg)`** und rechts steht **`Flag gtk zieht: + x11-libs/libXaw3d`**: Das bedeutet **nicht**, dass `gtk` aktuell `libXaw3d` benötigt, sondern zeigt das Delta beim **Toggeln (Ausschalten)** von `gtk` — denn sobald man `gtk` wieder abschaltet, greift das immer noch aktive `Xaw3d` und zieht `libXaw3d` erneut rein.
   * **Rechts (`Voll-Ersparnis gtk [app-editors/emacs]`)**: Da `gtk+` im System stark geteilt wird, rechnet das Tool vor, wie viel erst beim Entfernen der Blocker zu sparen wäre:
     * *Ohne Flag-Blocker*: `-163,8 MB bin / 282,2 MB dl (-106 pkg)` (wenn z. B. `at-spi2-core [introspection]`, `sudo [sendmail]` usw. abgeschaltet werden).
     * *Inkl. `world`-Entfernung*: `-591,4 MB bin (-353 pkg)`, wenn zusätzlich `world`-Pakete wie `sudo`, `dpkg`, `docker`, `emacs` etc. entfernt werden (siehe auch Anmerkung unten zur Reichweite dieser Analyse).
   * **Nebenbefund in Zustand C**: `[x] X` zeigt `+320.0 kB (+2/-0 pkg)` — ein Ausschalten von `X` würde über Kaskade/Fallback 2 zusätzliche Pakete nachziehen.

#### Screencap 5: `media-video/ffmpeg`
* **Blame (rechts)**: Zeigt exakt die 4 `world`-Pakete, die `ffmpeg` im Graphen halten: `mpv`, `freerdp [+ffmpeg]`, `tigervnc [+viewer]` und `firefox-bin`. Ein Entfernen von nur einem dieser Pakete würde `ffmpeg` also nicht einsparen.
* **Mitte**: Entspricht den Byte-Werten der CLI-Ausgabe in lesbaren `kB`/`MB`-Werten; `opencolorio` ist als `CONFLICT` markiert.

#### Screencap 6: `dev-vcs/git`
* **Blame**: Wird durch `app-containers/docker -> dev-vcs/git` reingezogen.
* **`[ ] tk`**: `+4,9 MB bin / +11,1 MB dl (+2/-0 pkg)` — zieht laut rechter Pane `+ dev-lang/tcl` und `+ dev-lang/tk` (für `gitk`/`git gui`).
* **Weitere große Hebel**: `[x] perl` ausschalten spart `-810 kB bin / -1,1 MB dl (-12 pkg)`; `[ ] subversion` einschalten kostet `+34 pkg`; `[ ] cgi` kostet `+19 pkg`; `[ ] doc` nur `+1,7 MB dl` (keine neuen Pakete).
* **`[x] curl ... CONFLICT`**: Da `curl` aktiv ist, bedeutet `CONFLICT` hier, dass das **Ausschalten** von `curl` einen `REQUIRED_USE`-Konflikt auslöst (wahrscheinlich weil `[x] webdav` aktiv ist und `webdav? ( curl )` verlangt).

#### Screencap 7: `dev-debug/ltrace`
* `Binpkg: 0 B`, `Pkg-Quell-DL: 290,0 kB` (kein Binpkg im Index).
* `[ ] unwind` zieht `sys-libs/libunwind` (`+160,0 kB bin / +986,6 kB dl`, `+1 pkg`).
* Rechts wieder leere `Kette 1:`.

#### Screencap 8: `app-text/mupdf`
* `[x] javascript` spart beim Ausschalten `dev-lang/mujs` (`-210 kB bin / -2,2 MB dl`, `-1 pkg`), löst laut `CONFLICT`-Marker aber einen `REQUIRED_USE`-Konflikt aus.
* `[x] jpeg2k` (`-320 kB / -1 pkg`) und `[x] opengl` (`-260 kB / -1 pkg`) sind ohne Konflikt abschaltbar; `[ ] barcode` (`+590 kB`) und `[ ] brotli` (`+390 kB`) wären kleine optionale Erweiterungen.

---

### 3. Aufgefallene Unstimmigkeiten / potenzielle Bugs in den Screencaps

Beim genauen Lesen der Ausgaben fallen fünf Punkte auf, bei denen das Verhalten oder die Darstellung noch Ecken hat (falls gewünscht, kann ich das im Quelltext gezielt verifizieren):

1. **Über-aggressive `Voll-Ersparnis`-Analyse (`mupdf` & `emacs`)**:
   Bei `mupdf` wird `[x] javascript` bereits mit `(+0/-1 pkg: dev-lang/mujs)` direkt entfernt — `mujs` hat keinen Blocker. Trotzdem listet `Voll-Ersparnis javascript` `-7 pkg` (`fdupes [ncurses]`, `bash [readline]`, `python [ncurses]`, `perl [!minimal]`) bzw. `-53 pkg` inkl. `world`-Entfernung auf. Offenbar verfolgt `08_delta/shared.rs` die Blocker bis hinunter zu allgemeinen Basisbibliotheken im Subgraph von `mujs` (wie `readline`/`ncurses`) statt nur zu direkt betroffenen Paketen. Ähnlich erklärt sich bei `emacs` (`gtk`), warum `sudo`, `dpkg` und `docker` in der `world entfernen`-Liste stehen.
2. **Missverständliche Formulierung `Flag <x> zieht:` bei aktiven Flags**:
   Wenn ein Flag bereits `[x]` ist (wie `gtk` in Screencap 4), bedeutet `Flag gtk zieht: + x11-libs/libXaw3d` eigentlich „*Ausschalten* von `gtk` zieht `+ libXaw3d`“. Ebenso zeigt `[x] curl ... CONFLICT`, dass *Ausschalten* einen Konflikt erzeugt, obwohl aktuell `Konflikte 0` vorliegen.
3. **Leere Blame-Anzeige (`Kette 1:`)**:
   Bei `emacs` und `mupdf` steht korrekt `(kein Pfad — Welt-Paket oder leer)`, bei `gentoo-sources` und `ltrace` steht stattdessen `Kette 1:` gefolgt von einer leeren Zeile.
4. **Fehlendes Vorzeichen bei `Voll-Ersparnis` (`emacs`)**:
   In Screencap 4 steht `-163.8 MB bin / 282.2 MB dl (-106 pkg)` (ohne `+` oder `-` vor `282.2 MB`, während bei `mupdf` `-8.1 MB dl` steht). Entweder fehlt ein Vorzeichen oder `dl` steigt dort unerwartet stark an.
5. **`big-levers` zeigt nicht nur Abschalt-Ersparnisse**:
   Wie oben erwähnt, widerspricht der positive Teil der Liste (`kde`, `plasma`, `doc`, `frei0r`, `flite`) der README-Aussage, dass `big-levers` nur globales Abschalten prüft.

Wenn du möchtest, dass ich einen dieser Punkte (z. B. die `Voll-Ersparnis`-Logik in `src/08_delta/shared.rs` oder das `big-levers`-Verhalten in `src/08_delta.rs`) exakt erkläre, kannst du mir die entsprechenden Dateien gerne schicken.
