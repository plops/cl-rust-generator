# TUI-Screencaps (140x36, Review-Szenario)

## 01_start

```text
 [1] Pakete & Alternativen │ [2] Global Big Levers

────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
┌Pakete (11)─────────────────────────────┐┌Details / USE-Flags───────────────────────────────────┐┌Blame / Pros-Cons───────────────────────┐
│> sys-kernel/gentoo-sources             ││sys-kernel/gentoo-sources  Binpkg: 0 B  Pkg-Quell-DL: ││Warum ist das hier?                     │
│  media-video/ffmpeg                    ││                                                      ││Kette 1:                                │
│  app-editors/emacs                     ││[ ] build n/a (ohne --binhost) / -700 B dl (+0/-1 pkg)││                                        │
│  www-client/firefox-bin                ││[ ] test-rust n/a (ohne --binhost) / 0 B dl CONFLICT  ││Flag build zieht:                       │
│  app-a/a                               ││[ ] vanilla n/a (ohne --binhost) / 0 B dl CONFLICT    ││- sys-apps/bc                           │
│  app-c/c                               ││                                                      ││bin: n/a (ohne --binhost)               │
│  choice: decoder                       ││                                                      ││                                        │
│    sys-apps/bc                         ││                                                      ││                                        │
│    x11-libs/libXaw3d                   ││                                                      ││                                        │
│    dev-libs/libx CONFLICT              ││                                                      ││                                        │
│    x11-libs/gtk+                       ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
└────────────────────────────────────────┘└──────────────────────────────────────────────────────┘└────────────────────────────────────────┘
────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg n/a (ohne --binhost)  Quell-DL 119.0 kB  Build-Only 0 B bin / 0 B dl  Pakete 10  Konflikte 1
[Space] Toggle  [Tab] Pane  [1/2] View  [/] Suche  [?] Hilfe  [S] Save  [E] Export  [V] Verify  [q] Quit
```

## 02_ffmpeg_after_filter

```text
 [1] Pakete & Alternativen │ [2] Global Big Levers

────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
┌Pakete (1) [Filter: ffm]────────────────┐┌Details / USE-Flags───────────────────────────────────┐┌Blame / Pros-Cons───────────────────────┐
│> media-video/ffmpeg                    ││media-video/ffmpeg  Binpkg: 0 B  Pkg-Quell-DL: 10.9 kB││Warum ist das hier?                     │
│                                        ││                                                      ││Kette 1:                                │
│                                        ││[ ] aom n/a (ohne --binhost) / 0 B dl CONFLICT        ││bin: n/a (ohne --binhost)               │
│                                        ││[ ] dav1d n/a (ohne --binhost) / 0 B dl CONFLICT      ││                                        │
│                                        ││[ ] svt-av1 n/a (ohne --binhost) / +7.8 kB dl (+1/-0 p││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
└────────────────────────────────────────┘└──────────────────────────────────────────────────────┘└────────────────────────────────────────┘
────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg n/a (ohne --binhost)  Quell-DL 119.0 kB  Build-Only 0 B bin / 0 B dl  Pakete 10  Konflikte 1
[Space] Toggle  [Tab] Pane  [1/2] View  [/] Suche  [?] Hilfe  [S] Save  [E] Export  [V] Verify  [q] Quit
```

## 03_emacs_shared_blocker

```text
 [1] Pakete & Alternativen │ [2] Global Big Levers

────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
┌Pakete (1) [Filter: emacs]──────────────┐┌Details / USE-Flags───────────────────────────────────┐┌Blame / Pros-Cons───────────────────────┐
│> app-editors/emacs                     ││app-editors/emacs  Binpkg: 0 B  Pkg-Quell-DL: 8.8 kB  ││Warum ist das hier?                     │
│                                        ││                                                      ││Kette 1:                                │
│                                        ││[x] Xaw3d n/a (ohne --binhost) / -220 B dl (+0/-1 pkg)││bin: n/a (ohne --binhost)               │
│                                        ││[x] gtk n/a (ohne --binhost) / 0 B dl CONFLICT        ││                                        │
│                                        ││[x] gui n/a (ohne --binhost) / 0 B dl CONFLICT        ││gtk: GTK-Toolkit                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││Voll-Ersparnis gtk [app-editors/emacs]: │
│                                        ││                                                      ││ohne Flag-Blocker: 0 B bin / 0 B dl (-0 │
│                                        ││                                                      ││pkg)                                    │
│                                        ││                                                      ││inkl. world-Entfernung: 0 B bin (-2 pkg)│
│                                        ││                                                      ││world entfernen:                        │
│                                        ││                                                      ││www-client/firefox-bin                  │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
└────────────────────────────────────────┘└──────────────────────────────────────────────────────┘└────────────────────────────────────────┘
────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg n/a (ohne --binhost)  Quell-DL 119.0 kB  Build-Only 0 B bin / 0 B dl  Pakete 10  Konflikte 1
[Space] Toggle  [Tab] Pane  [1/2] View  [/] Suche  [?] Hilfe  [S] Save  [E] Export  [V] Verify  [q] Quit
```

## 04_big_levers

```text
 [1] Pakete & Alternativen │ [2] Global Big Levers

────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
┌Global Big Levers [Space toggelt]─────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│  Flag                                                                                       Pakete   Bin-Delta    DL-Delta     Konflikte │
│> build                                                                                      1        0 B          -700 B       1         │
│  Xaw3d                                                                                      1        0 B          -220 B       1         │
│  aom                                                                                        1        0 B          0 B          1         │
│  dav1d                                                                                      1        0 B          0 B          1         │
│  foo                                                                                        1        0 B          0 B          1         │
│  gtk                                                                                        1        0 B          0 B          1         │
│  gui                                                                                        1        0 B          0 B          1         │
│  test-rust                                                                                  1        0 B          0 B          1         │
│  vanilla                                                                                    1        0 B          0 B          1         │
│  svt-av1                                                                                    1        0 B          7.8 kB       1         │
│                                                                                                                                          │
│                                                                                                                                          │
│                                                                                                                                          │
│                                                                                                                                          │
│                                                                                                                                          │
│                                                                                                                                          │
│                                                                                                                                          │
│                                                                                                                                          │
│                                                                                                                                          │
│                                                                                                                                          │
│                                                                                                                                          │
│                                                                                                                                          │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
┌Hebel-Details─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│Flag build                                                                                                                                │
│Betroffene Pakete (1): sys-kernel/gentoo-sources                                                                                          │
│Entfernt: sys-apps/bc (0 B)                                                                                                               │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg n/a (ohne --binhost)  Quell-DL 119.0 kB  Build-Only 0 B bin / 0 B dl  Pakete 10  Konflikte 1
[Space] Toggle  [Tab] Pane  [1/2] View  [/] Suche  [?] Hilfe  [S] Save  [E] Export  [V] Verify  [q] Quit
```

## 05_help_modal

```text
 [1] Pakete & Alternativen │ [2] Global Big Levers

────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
┌Global Big Levers [Space toggelt]─────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│  Flag                                                                                       Pakete   Bin-Delta    DL-Delta     Konflikte │
│> build                                                                                      1        0 B          -700 B       1         │
│  Xaw3d                                                                                      1        0 B          -220 B       1         │
│  aom                                                                                        1        0 B          0 B          1         │
│  dav1d                                                                                      1        0 B          0 B          1         │
│  foo                                                                                        1        0 B          0 B          1         │
│  gtk                                ┌Hilfe────────────────────────────────────────────────────────┐  0 B          0 B          1         │
│  gui                                │Metriken lesen:                                              │  0 B          0 B          1         │
│  test-rust                          │Binpkg (Binhost, Trefferquote) / Quell-DL (Tarballs)         │  0 B          0 B          1         │
│  vanilla                            │Build-Only (BDEPEND, bin+src) / Pkg-Quell-DL (nur Paket)     │  0 B          0 B          1         │
│  svt-av1                            │0 B-Delta: geteilte Abh. oder Compile-only-Flag              │  0 B          7.8 kB       1         │
│                                     │Voll-Ersparnis: rechte Pane (Flag-Blocker/world-Entfernung)  │                                      │
│                                     │                                                             │                                      │
│                                     │Navigation:                                                  │                                      │
│                                     │/ Suche (Esc loescht Filter), Tab Pane, Space Toggle         │                                      │
│                                     │Enter Auswahl, 1/2 Tabs, j/k Scrollen                        │                                      │
│                                     │                                                             │                                      │
│                                     │Ablauf:                                                      │                                      │
│                                     │[S] Save, [E] Export, dann [V] Verify                        │                                      │
│                                     │                                                             │                                      │
│                                     │[Esc]/[?]/[q] schliesst Hilfe                                │                                      │
│                                     └─────────────────────────────────────────────────────────────┘                                      │
│                                                                                                                                          │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
┌Hebel-Details─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│Flag build                                                                                                                                │
│Betroffene Pakete (1): sys-kernel/gentoo-sources                                                                                          │
│Entfernt: sys-apps/bc (0 B)                                                                                                               │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg n/a (ohne --binhost)  Quell-DL 119.0 kB  Build-Only 0 B bin / 0 B dl  Pakete 10  Konflikte 1
[Space] Toggle  [Tab] Pane  [1/2] View  [/] Suche  [?] Hilfe  [S] Save  [E] Export  [V] Verify  [q] Quit
```

## 06_conflict_modal

```text
 [1] Pakete & Alternativen │ [2] Global Big Levers

────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
┌Pakete (1) [Filter: v-libs/libx]────────┐┌Details / USE-Flags───────────────────────────────────┐┌Blame / Pros-Cons───────────────────────┐
│>   dev-libs/libx CONFLICT              ││dev-libs/libx  Binpkg: 0 B  Pkg-Quell-DL: 500 B       ││Warum ist das hier?                     │
│                                        ││                                                      ││                                        │
│                                        ││[x] foo n/a (ohne --binhost) / 0 B dl CONFLICT        ││Konflikte:                              │
│                                        ││                                                      ││! app-c/c verlangt dev-libs/libx[foo] = │
│                                        ││                                                      ││aus, gesetzt ist das Gegenteil          │
│                                        ││                                                      ││Kette 1:                                │
│                                        ││                                                      ││app-a/a -> dev-libs/libx                │
│                                        ││                                                      ││Kette 2:                                │
│                                        ││                                                      ││app-c/c -> dev-libs/libx                │
│                                        ││                                                      ││bin: n/a (ohne --binhost)               │
│                   ┌Quick-Switch──────────────────────────────────────────────────────────────────────────────────────┐                   │
│                   │Konflikt: app-c/c verlangt dev-libs/libx[foo] = aus, gesetzt ist das Gegenteil                    │                   │
│                   │Verursacher: app-c/c, dev-libs/libx                                                               │                   │
│                   │                                                                                                  │                   │
│                   │> dev-libs/libx[foo] uebernehmen                                                                  │                   │
│                   │                                                                                                  │                   │
│                   │[Enter] uebernehmen  [Esc] abbrechen                                                              │                   │
│                   └──────────────────────────────────────────────────────────────────────────────────────────────────┘                   │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
│                                        ││                                                      ││                                        │
└────────────────────────────────────────┘└──────────────────────────────────────────────────────┘└────────────────────────────────────────┘
────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg n/a (ohne --binhost)  Quell-DL 119.0 kB  Build-Only 0 B bin / 0 B dl  Pakete 10  Konflikte 1
[Space] Toggle  [Tab] Pane  [1/2] View  [/] Suche  [?] Hilfe  [S] Save  [E] Export  [V] Verify  [q] Quit
```

