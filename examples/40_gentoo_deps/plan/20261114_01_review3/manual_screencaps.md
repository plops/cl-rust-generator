erklaere anhand der screenshots wie man die angezeigten informationen
interpretieren kann.  ausserdem koennen wir vielleicht zusaetzlich
features definieren die bei der anwendung der software helfen koennten

```
 [1] Pakete & Alternativen │ [2] Global Big Levers

──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
┌Pakete (592)──────────────────────────────────────┐┌Details / USE-Flags─────────────────────────────────────────────────┐┌Blame / Pros-Cons─────────────────────────────────┐
│> sys-kernel/gentoo-sources                       ││sys-kernel/gentoo-sources  Binpkg: 0 B  Subgraph-DL: 153.0 MB       ││Warum ist das hier?                               │
│  app-admin/sudo                                  ││                                                                    ││Kette 1:                                          │
│  app-admin/sysklogd                              ││[x] abi_x86_64 0 B bin / 0 B dl                                     ││                                                  │
│  app-arch/dpkg                                   ││[ ] build -6.8 MB bin / -46.2 MB dl (+0/-11 pkg)                    ││                                                  │
│  app-containers/docker                           ││[x] cet 0 B bin / 0 B dl                                            ││                                                  │
│  app-containers/docker-buildx                    ││[x] elibc_glibc 0 B bin / 0 B dl                                    ││                                                  │
│  app-containers/docker-cli                       ││[ ] experimental 0 B bin / 0 B dl                                   ││                                                  │
│  app-crypt/p11-kit                               ││[x] iconv 0 B bin / 0 B dl                                          ││                                                  │
│  app-editors/emacs                               ││[x] kernel_linux 0 B bin / 0 B dl                                   ││                                                  │
│  app-misc/fastfetch                              ││[x] llvm_targets_X86 0 B bin / 0 B dl                               ││                                                  │
│  app-misc/fdupes                                 ││[x] openmp 0 B bin / 0 B dl                                         ││                                                  │
│  app-misc/jq                                     ││[ ] symlink 0 B bin / 0 B dl                                        ││                                                  │
│  app-misc/mc                                     ││[x] test-rust 0 B bin / 0 B dl                                      ││                                                  │
│  app-misc/tmate                                  ││[ ] vanilla 0 B bin / 0 B dl                                        ││                                                  │
│  app-misc/tmux                                   ││                                                                    ││                                                  │
│  app-pda/ifuse                                   ││                                                                    ││                                                  │
│  app-portage/cpuid2cpuflags                      ││                                                                    ││                                                  │
│  app-portage/eix                                 ││                                                                    ││                                                  │
│  app-portage/gentoolkit                          ││                                                                    ││                                                  │
│  app-shells/bash-completion                      ││                                                                    ││                                                  │
│  app-shells/gentoo-zsh-completions               ││                                                                    ││                                                  │
│  app-shells/zsh                                  ││                                                                    ││                                                  │
│  app-text/mupdf                                  ││                                                                    ││                                                  │
│  app-text/tree                                   ││                                                                    ││                                                  │
│  dev-build/ninja                                 ││                                                                    ││                                                  │
│  dev-debug/gdb                                   ││                                                                    ││                                                  │
│  dev-debug/ltrace                                ││                                                                    ││                                                  │
│  dev-debug/strace                                ││                                                                    ││                                                  │
│  dev-libs/nss                                    ││                                                                    ││                                                  │
│  dev-lisp/sbcl                                   ││                                                                    ││                                                  │
│  dev-python/btrfs                                ││                                                                    ││                                                  │
│  dev-python/uv                                   ││                                                                    ││                                                  │
│  dev-vcs/git                                     ││                                                                    ││                                                  │
│  media-fonts/wqy-zenhei                          ││                                                                    ││                                                  │
│  media-gfx/feh                                   ││                                                                    ││                                                  │
│  media-gfx/scrot                                 ││                                                                    ││                                                  │
│  media-libs/fontconfig                           ││                                                                    ││                                                  │
│  media-sound/pulsemixer                          ││                                                                    ││                                                  │
│  media-video/mpv                                 ││                                                                    ││                                                  │
│  media-video/pipewire                            ││                                                                    ││                                                  │
│  media-video/wireplumber                         ││                                                                    ││                                                  │
└──────────────────────────────────────────────────┘└────────────────────────────────────────────────────────────────────┘└──────────────────────────────────────────────────┘
──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg 791.5 MB  Download 3318.1 MB  BDEPEND 243.2 MB  Pakete 555  Konflikte 0  [Space] Toggle  [Tab] Pane  [1/2] View  [S] Save  [E] Export  [V] Verify  [q] Quit

```

erklaere die eintraege in der unteren zeile.  enthaelt Binpkg alle
binare packete und nicht die von source gebauten?  enthaelt Download
sowohl binaerpackete und sourcen? was ist BDEPEND sourcen und
binaerpackete?  ich finde diese informationen sollten aufgetrennt
werden. ausserdem sollte es eine hilfe geben die diese fragen erklaert
(kann uauch in readme geschenh)


das man mit `/` suchen kann sollte auch angezeigt werden

hier habe ich nach "ffm" gesucht:

```
 [1] Pakete & Alternativen │ [2] Global Big Levers

──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
┌Pakete (1) [Filter: /ffm_]────────────────────────┐┌Details / USE-Flags─────────────────────────────────────────────────┐┌Blame / Pros-Cons─────────────────────────────────┐
│>   media-video/ffmpeg                            ││media-video/ffmpeg  Binpkg: 9.9 MB  Subgraph-DL: 11.2 MB            ││Warum ist das hier?                               │
│                                                  ││                                                                    ││Kette 1:                                          │
│                                                  ││[x] abi_x86_64 0 B bin / 0 B dl                                     ││media-video/mpv -> media-video/ffmpeg             │
│                                                  ││[ ] build -6.8 MB bin / -46.2 MB dl (+0/-11 pkg)                    ││Kette 2:                                          │
│                                                  ││[x] cet 0 B bin / 0 B dl                                            ││net-misc/freerdp -> media-video/ffmpeg [+ffmpeg]  │
│                                                  ││[x] elibc_glibc 0 B bin / 0 B dl                                    ││Kette 3:                                          │
│                                                  ││[ ] experimental 0 B bin / 0 B dl                                   ││net-misc/tigervnc -> media-video/ffmpeg [+viewer] │
│                                                  ││[x] iconv 0 B bin / 0 B dl                                          ││Kette 4:                                          │
│                                                  ││[x] kernel_linux 0 B bin / 0 B dl                                   ││www-client/firefox-bin -> media-video/ffmpeg      │
│                                                  ││[x] llvm_targets_X86 0 B bin / 0 B dl                               ││                                                  │
│                                                  ││[x] openmp 0 B bin / 0 B dl                                         ││                                                  │
│                                                  ││[ ] symlink 0 B bin / 0 B dl                                        ││                                                  │
│                                                  ││[x] test-rust 0 B bin / 0 B dl                                      ││                                                  │
│                                                  ││[ ] vanilla 0 B bin / 0 B dl                                        ││                                                  │
│                                                  ││                                                                    ││                                                  │
│                                                  ││                                                                    ││                                                  │
│                                                  ││                                                                    ││                                                  │
└──────────────────────────────────────────────────┘└────────────────────────────────────────────────────────────────────┘└──────────────────────────────────────────────────┘
──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg 791.5 MB  Download 3318.1 MB  BDEPEND 243.2 MB  Pakete 555  Konflikte 0  [Space] Toggle  [Tab] Pane  [1/2] View  [S] Save  [E] Export  [V] Verify  [q] Quit
```

erklaere die zahlen in media-video/ffmpeg Binpkg: 9.9 MB Subgraph-DL:
11.2 MB

ich frage mich was die use flags abi_x86_64 und cet bedeuten. gibt es
doku fuer useflag die das tui tool anzeigen koennte? ich denke das
waere eine hilfreiche verbesserung

warum sind so viele der reporteten werte 0?

was bedeutet build use flag, erklaere die rechte seite

```
 [1] Pakete & Alternativen │ [2] Global Big Levers

──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
┌Pakete (1) [Filter: ffm]──────────────────────────┐┌Details / USE-Flags─────────────────────────────────────────────────┐┌Blame / Pros-Cons─────────────────────────────────┐
│>   media-video/ffmpeg                            ││media-video/ffmpeg  Binpkg: 9.9 MB  Subgraph-DL: 11.2 MB            ││Warum ist das hier?                               │
│                                                  ││                                                                    ││Kette 1:                                          │
│                                                  ││[x] abi_x86_64 0 B bin / 0 B dl                                     ││media-video/mpv -> media-video/ffmpeg             │
│                                                  ││[ ] build -6.8 MB bin / -46.2 MB dl (+0/-11 pkg)                    ││Kette 2:                                          │
│                                                  ││[x] cet 0 B bin / 0 B dl                                            ││net-misc/freerdp -> media-video/ffmpeg [+ffmpeg]  │
│                                                  ││[x] elibc_glibc 0 B bin / 0 B dl                                    ││Kette 3:                                          │
│                                                  ││[ ] experimental 0 B bin / 0 B dl                                   ││net-misc/tigervnc -> media-video/ffmpeg [+viewer] │
│                                                  ││[x] iconv 0 B bin / 0 B dl                                          ││Kette 4:                                          │
│                                                  ││[x] kernel_linux 0 B bin / 0 B dl                                   ││www-client/firefox-bin -> media-video/ffmpeg      │
│                                                  ││[x] llvm_targets_X86 0 B bin / 0 B dl                               ││                                                  │
│                                                  ││[x] openmp 0 B bin / 0 B dl                                         ││Flag build zieht:                                 │
│                                                  ││[ ] symlink 0 B bin / 0 B dl                                        ││- app-alternatives/bc                             │
│                                                  ││[x] test-rust 0 B bin / 0 B dl                                      ││- app-alternatives/lex                            │
│                                                  ││[ ] vanilla 0 B bin / 0 B dl                                        ││- app-alternatives/yacc                           │
│                                                  ││                                                                    ││- dev-build/make                                  │
│                                                  ││                                                                    ││- sys-devel/bc                                    │
│                                                  ││                                                                    ││- sys-devel/binutils-config                       │
│                                                  ││                                                                    ││- sys-devel/bison                                 │
│                                                  ││                                                                    ││- sys-devel/flex                                  │
│                                                  ││                                                                    ││(...)                                             │
│                                                  ││                                                                    ││                                                  │
└──────────────────────────────────────────────────┘└────────────────────────────────────────────────────────────────────┘└──────────────────────────────────────────────────┘
──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg 791.5 MB  Download 3318.1 MB  BDEPEND 243.2 MB  Pakete 555  Konflikte 0  [Space] Toggle  [Tab] Pane  [1/2] View  [S] Save  [E] Export  [V] Verify  [q] Quit

```

bei emacs verstehe ich nicht wie ich `gui` useflag interpretieren soll

```
┌Pakete (4) [Filter: emacs]─────────────────────┐┌Details / USE-Flags────────────────────────────────────────────┐┌Blame / Pros-Cons──────────────────────────────┐
│> app-editors/emacs                            ││app-editors/emacs  Binpkg: 35.4 MB  Subgraph-DL: 55.3 MB       ││                                               │
│  || virtual/editor||app-editors/nano,app-edito││                                                               ││                                               │
│    app-emacs/emacs-common                     ││[x] X 0 B bin / 0 B dl                                         ││                                               │
│    app-eselect/eselect-emacs                  ││[x] Xaw3d -220.0 kB bin / -470.7 kB dl (+0/-1 pkg)             ││                                               │
│                                               ││[x] abi_x86_64 0 B bin / 0 B dl                                ││                                               │
│                                               ││[ ] acl +30.0 kB bin / 0 B dl (+1/-0 pkg)                      ││                                               │
│                                               ││[ ] alsa 0 B bin / 0 B dl                                      ││                                               │
│                                               ││[ ] athena 0 B bin / 0 B dl                                    ││                                               │
│                                               ││[ ] cairo 0 B bin / 0 B dl                                     ││                                               │
│                                               ││[x] cet 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[ ] dbus 0 B bin / 0 B dl                                      ││                                               │
│                                               ││[ ] dynamic-loading 0 B bin / 0 B dl                           ││                                               │
│                                               ││[x] elibc_glibc 0 B bin / 0 B dl                               ││                                               │
│                                               ││[ ] games +20.0 kB bin / 0 B dl (+1/-0 pkg)                    ││                                               │
│                                               ││[ ] gfile 0 B bin / 0 B dl                                     ││                                               │
│                                               ││[ ] gif 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[x] gmp 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[ ] gpm +200.0 kB bin / 811.9 kB dl (+1/-0 pkg)                ││                                               │
│                                               ││[ ] gsettings 0 B bin / 0 B dl                                 ││                                               │
│                                               ││[ ] gtk -220.0 kB bin / -470.7 kB dl (+0/-1 pkg)               ││                                               │
│                                               ││[x] gui -220.0 kB bin / -470.7 kB dl (+0/-1 pkg)               ││                                               │
│                                               ││[ ] gzip-el 0 B bin / 0 B dl                                   ││                                               │
│                                               ││[ ] harfbuzz 0 B bin / 0 B dl                                  ││                                               │
│                                               ││[x] iconv 0 B bin / 0 B dl                                     ││                                               │
│                                               ││[ ] imagemagick +8.3 MB bin / 9.7 MB dl (+1/-0 pkg)            ││                                               │
│                                               ││[x] inotify 0 B bin / 0 B dl                                   ││                                               │
│                                               ││[ ] jit 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[ ] jpeg 0 B bin / 0 B dl                                      ││                                               │
│                                               ││[x] kernel_linux 0 B bin / 0 B dl                              ││                                               │
│                                               ││[ ] lcms 0 B bin / 0 B dl                                      ││                                               │
│                                               ││[x] libxml2 0 B bin / 0 B dl                                   ││                                               │
│                                               ││[ ] livecd 0 B bin / 0 B dl                                    ││                                               │
│                                               ││[x] llvm_targets_X86 0 B bin / 0 B dl                          ││                                               │
│                                               ││[ ] m17n-lib 0 B bin / 0 B dl                                  ││                                               │
│                                               ││[ ] mailutils -70.0 kB bin / -68.2 kB dl (+0/-2 pkg)           ││                                               │
│                                               ││[ ] motif -220.0 kB bin / 7.9 MB dl (+1/-1 pkg)                ││                                               │
│                                               ││[x] openmp 0 B bin / 0 B dl                                    ││                                               │
│                                               ││[ ] png 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[ ] sound 0 B bin / 0 B dl                                     ││                                               │
│                                               ││[ ] source 0 B bin / 0 B dl                                    ││                                               │
│                                               ││[ ] sqlite 0 B bin / 0 B dl                                    ││                                               │
│                                               ││[ ] ssl 0 B bin / 0 B dl                                       ││                                               │
└───────────────────────────────────────────────┘└───────────────────────────────────────────────────────────────┘└───────────────────────────────────────────────┘
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg 791.7 MB  Download 3318.6 MB  BDEPEND 243.2 MB  Pakete 556  Konflikte 0  [Space] Toggle  [Tab] Pane  [1/2] View  [S] Save  [E] Export  [V] Verify  [q] Quit
*ungespeichert*
```

wenn ich verify druecke bekomme ich diesen fehler: Verify-Fehler: io
error at emerge --pretend --emptytree --verbose --root
/tmp/gentoo-export --config-root /tmp/gentoo-export @world: No such
file or directory (os error 2)

der sollte genauer sagen wo in der readme die vorgehensweise definiert
ist (oder explizit angeben wenn z.b. /tmp/gentoo-export
fehlt). idealerweise sollte es die packetdefinitionen vom laufenenden
gentoo finden


in `Global Big levers` sehe ich:

```
┌Global Big Levers [Space toggelt]────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│  Flag                                                                                                              Pakete   Bin-Delta    DL-Delta     Konflikte │
│> openmp                                                                                                            555      -112.8 MB    -95.1 MB     0         │
│  llvm                                                                                                              1        -112.0 MB    -319.0 MB    0         │
│  gui                                                                                                               5        -24.5 MB     -12.3 MB     4         │
│  icu                                                                                                               1        -20.0 MB     -26.7 MB     0         │
│  post-quantum                                                                                                      1        -11.9 MB     -42.3 MB     0         │
│  rdw                                                                                                               1        -10.8 MB     -17.6 MB     0         │
│  build                                                                                                             5        -8.4 MB      -49.6 MB     0         │
│  viewer                                                                                                            1        -4.3 MB      159.2 kB     0         │
│  modemmanager                                                                                                      1        -3.9 MB      -5.3 MB      0         │
│  freepg                                                                                                            1        -3.1 MB      -805.1 kB    2         │
│  reference                                                                                                         4        -2.9 MB      33.7 kB      2         │
│  shaderc                                                                                                           1        -2.2 MB      -4.4 MB      1         │
│  daemon                                                                                                            1        -2.0 MB      -3.2 MB      0         │
│  qmi                                                                                                               1        -1.7 MB      -3.6 MB      1         │
│  elogind                                                                                                           4        -1.7 MB      -3.5 MB      0         │
│  minimal                                                                                                           10       -1.5 MB      -9.1 MB      0         │
│  wifi                                                                                                              1        -1.4 MB      -3.7 MB      1         │
│  iwd                                                                                                               1        -1.4 MB      -3.7 MB      0         │
│  gawk                                                                                                              1        -1.3 MB      -1.2 MB      0         │
│  rsync-verify                                                                                                      1        -1.0 MB      -1.3 MB      0         │
│  ensurepip                                                                                                         1        -1010.0 kB   -8.9 MB      0         │
│  gpg                                                                                                               2        -880.0 kB    -1.2 MB      0         │
│  perl                                                                                                              1        -810.0 kB    -1.1 MB      0         │
│  ppd                                                                                                               1        -800.0 kB    -2.2 MB      0         │
│  dav1d                                                                                                             1        -630.0 kB    -1014.5 kB   0         │
│  libarchive                                                                                                        1        -550.0 kB    -6.4 MB      1         │
│  opengl                                                                                                            4        -540.0 kB    -1.3 MB      2         │
│  libinput                                                                                                          1        -510.0 kB    -1.6 MB      0         │
│  policykit                                                                                                         1        -500.0 kB    -1.4 MB      0         │
│  qmanifest                                                                                                         1        -500.0 kB    -1.4 MB      0         │
│  argon2                                                                                                            1        -490.0 kB    -1.4 MB      0         │
│  webp                                                                                                              1        -470.0 kB    -4.1 MB      0         │
│  mbim                                                                                                              2        -460.0 kB    -224.2 kB    0         │
│  ssl                                                                                                               9        -450.0 kB    -922.5 kB    8         │
│  gnu                                                                                                               3        -410.0 kB    -1.6 MB      0         │
│  ppp                                                                                                               1        -410.0 kB    -991.1 kB    0         │
│  lua                                                                                                               1        -380.0 kB    -1.1 MB      0         │
│  lua_single_target_luajit                                                                                          1        -380.0 kB    -1.1 MB      0         │
│  lua_single_target_lua5-1                                                                                          2        -380.0 kB    -1.1 MB      0         │
│  debuginfod                                                                                                        2        -340.0 kB    -2.1 MB      0         │
│  jpeg2k                                                                                                            1        -320.0 kB    -2.1 MB      0         │
└─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg 791.5 MB  Download 3318.1 MB  BDEPEND 243.2 MB  Pakete 555  Konflikte 0  [Space] Toggle  [Tab] Pane  [1/2] View  [S] Save  [E] Export  [V] Verify  [q] Quit
```

erklaere diesen screencap? heisst das wenn ich openmp ausschalte dann
braucht ess 112MB weniger in binaerdateien und 95MB weniger
runtergeladenen resourcen? und dass 555 packete betroffen sind.
warum?

warum gibt es ein packet was llvm reinzieht? sind das dieselben 112MB wie openmp?

