ich habe das program mittel
`gentoo-dep-tui/target/release/gentoo-dep-tui tui --binhost
/tmp/Packages` auf meinem gentoo gestartet. es oeffnet so:


```
 [1] Pakete & Alternativen │ [2] Global Big Levers

───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
┌Pakete (588)──────────────────────────────┐┌Details / USE-Flags──────────────────────────────────────┐┌Blame / Pros-Cons─────────────────────────┐
│> sys-kernel/gentoo-sources               ││sys-kernel/gentoo-sources  Binpkg: 0 B  Pkg-Quell-DL: 153││Warum ist das hier?                       │
│  app-admin/sudo                          ││                                                         ││Kette 1:                                  │
│  app-admin/sysklogd                      ││[ ] build -6.8 MB bin / -46.2 MB dl (+0/-11 pkg)         ││                                          │
│  app-arch/dpkg                           ││[ ] experimental 0 B bin / 0 B dl                        ││symlink: Force kernel ebuilds to          │
│  app-containers/docker                   ││[ ] symlink 0 B bin / 0 B dl                             ││automatically update the /usr/src/linux   │
│  app-containers/docker-buildx            ││[ ] vanilla 0 B bin / 0 B dl                             ││symlink                                   │
│  app-containers/docker-cli               ││                                                         ││                                          │
└──────────────────────────────────────────┘└─────────────────────────────────────────────────────────┘└──────────────────────────────────────────┘
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg 758.8 MB (504/554 Hits)  Quell-DL 3313.0 MB  Build-Only 243.2 MB bin / 1761.3 MB dl  Pakete 554  Konflikte 0
[Space] Toggle  [Tab] Pane  [1/2] View  [/] Suche  [?] Hilfe  [S] Save  [E] Export  [V] Verify  [q] Quit
```

benutze perf report fuer die verschiedenen modi wie index-check, graph, deltas und big-levers:

```
local $ time gentoo-dep-tui/target/release/gentoo-dep-tui index-check --binhost /tmp/Packages  --perf-report
Kategorien : 168
Pakete     : 13674
Cache-Dateien: 33625
Ladezeit   : 486 ms
Binhost-Treffer: 49.1 %
perf-report (Phasen: ms / Anzahl):
index_load           503.1 ms
effective_use          0.0 ms / 0 calls
package_use     match_calls=0
resolve                0.0 ms / 0 calls
build_extra            0.0 ms / 0 calls
cascade                0.0 ms / 0 calls / 0 rounds
delta                  0.0 ms / 0 runs / 0 noop_skips
big_levers             0.0 ms / 0 runs / 0 noop_skips
index           mask_checks=31786 dep_parses=239
delta_cache     hits=0 misses=0

real	0m0.584s
user	0m0.919s
sys	0m0.994s
```


```
local $ time gentoo-dep-tui/target/release/gentoo-dep-tui graph --binhost /tmp/Packages  --perf-report
Pakete     : 554
Binpkg     : 758 MB
Download   : 3312 MB
BDEPEND-extra: 243 MB
Konflikte  : 0
Uebersprungen: 1
SKIP sys-kernel/gentoo-sources:6.18.57 (von world): keine passende Version im Index
  acct-group/audio-0-r3
  acct-group/cdrom-0-r3
  acct-group/cuse-0
  acct-group/dhcp-0-r3
[...]
  x11-themes/hicolor-icon-theme-0.17
  x11-wm/dwm-6.8
perf-report (Phasen: ms / Anzahl):
index_load           505.8 ms
effective_use         28.1 ms / 3924 calls
package_use     match_calls=3924
resolve               52.7 ms / 3 calls
build_extra           21.6 ms / 3 calls
cascade               69.3 ms / 1 calls / 3 rounds
delta                  0.0 ms / 0 runs / 0 noop_skips
big_levers             0.0 ms / 0 runs / 0 noop_skips
index           mask_checks=31786 dep_parses=239
delta_cache     hits=0 misses=0

real	0m0.654s
user	0m0.988s
sys	0m1.076s
```

```
local $ time gentoo-dep-tui/target/release/gentoo-dep-tui deltas --binhost /tmp/Packages  --perf-report media-video/ffmpeg
  [ ] X  (+71680 B bin / 281460 B dl (+1/-0 Pakete: x11-libs/libXv))
  [ ] alsa  (0 B bin / 0 B dl)
  [ ] amf  (0 B bin / 4864728 B dl (+1/-0 Pakete: media-video/amdgpu-pro-amf))
  [ ] amr  (+163840 B bin / 939179 B dl (+1/-0 Pakete: media-libs/opencore-amr))
  [ ] amrenc  (+92160 B bin / 608936 B dl (+1/-0 Pakete: media-libs/vo-amrwbenc))
  [ ] appkit  (0 B bin / 0 B dl)
  [ ] bluray  (0 B bin / 500813 B dl (+2/-0 Pakete: dev-libs/libudfread, media-libs/libbluray))
  [ ] bs2b  (+51200 B bin / 606283 B dl (+1/-0 Pakete: media-libs/libbs2b))
  [ ] bzip2  (0 B bin / 0 B dl)
  [ ] cairo  (0 B bin / 0 B dl)
  [ ] cdio  (+563200 B bin / 4225124 B dl (+4/-0 Pakete: app-eselect/eselect-cdparanoia, app-eselect/eselect-lib-bin-symlink, dev-libs/libcdio, ...) KASKADE)
  [ ] chromaprint  (+1617920 B bin / 5721795 B dl (+2/-0 Pakete: media-libs/chromaprint, sci-libs/fftw))
  [ ] chromium  (0 B bin / 0 B dl KASKADE)
  [ ] codec2  (+645120 B bin / 7688549 B dl (+1/-0 Pakete: media-libs/codec2))
  [ ] cuda-clang  (0 B bin / 0 B dl KASKADE)
  [x] dav1d  (-645120 B bin / -1038852 B dl (+0/-1 Pakete: media-libs/dav1d))
  [ ] doc  (0 B bin / 0 B dl)
  [x] drm  (0 B bin / 0 B dl KASKADE)
  [ ] dvd  (+327680 B bin / 563996 B dl (+3/-0 Pakete: media-libs/libdvdcss, media-libs/libdvdnav, media-libs/libdvdread) KASKADE)
  [ ] fdk  (+727040 B bin / 2906697 B dl (+1/-0 Pakete: media-libs/fdk-aac))
  [ ] flite  (+12492800 B bin / 20233792 B dl (+1/-0 Pakete: app-accessibility/flite))
  [x] fontconfig  (0 B bin / 0 B dl)
  [ ] frei0r  (+18124800 B bin / 224458175 B dl (+5/-0 Pakete: dev-cpp/eigen, dev-libs/flatbuffers, media-libs/gavl, ...) KASKADE)
  [ ] fribidi  (0 B bin / 0 B dl KASKADE)
  [ ] gcrypt  (0 B bin / 0 B dl)
  [ ] gme  (+194560 B bin / 343689 B dl (+1/-0 Pakete: media-libs/game-music-emu))
  [ ] gmp  (0 B bin / 0 B dl KASKADE)
  [x] gnutls  (0 B bin / 0 B dl)
  [x] gpl  (0 B bin / 0 B dl KASKADE)
  [ ] gsm  (+71680 B bin / 66597 B dl (+1/-0 Pakete: media-sound/gsm))
  [ ] iec61883  (+235520 B bin / 888439 B dl (+3/-0 Pakete: media-libs/libiec61883, sys-libs/libavc1394, sys-libs/libraw1394))
  [ ] ieee1394  (+256000 B bin / 927607 B dl (+2/-0 Pakete: media-libs/libdc1394, sys-libs/libraw1394))
  [ ] jack  (+133120 B bin / 1091204 B dl (+3/-0 Pakete: media-sound/jack-example-tools, media-sound/jack2, virtual/jack))
  [ ] jpeg2k  (0 B bin / 0 B dl)
  [ ] jpegxl  (+3061760 B bin / 6036912 B dl (+3/-0 Pakete: app-arch/brotli, dev-cpp/highway, media-libs/libjxl))
  [ ] kvazaar  (+296960 B bin / 600315 B dl (+1/-0 Pakete: media-libs/kvazaar))
  [ ] ladspa  (0 B bin / 0 B dl)
  [x] lame  (0 B bin / 0 B dl)
  [ ] lcms  (0 B bin / 0 B dl)
  [ ] libaom  (+1566720 B bin / 5497367 B dl (+1/-0 Pakete: media-libs/libaom))
  [ ] libaribb24  (0 B bin / 57378 B dl (+1/-0 Pakete: media-libs/aribb24) KASKADE)
  [x] libass  (0 B bin / 0 B dl)
  [ ] libcaca  (+296960 B bin / 893319 B dl (+1/-0 Pakete: media-libs/libcaca))
  [ ] libilbc  (+102400 B bin / 1307591 B dl (+1/-0 Pakete: media-libs/libilbc))
  [ ] liblc3  (+133120 B bin / 1659909 B dl (+1/-0 Pakete: media-sound/liblc3))
  [ ] libplacebo  (0 B bin / 0 B dl KASKADE)
  [ ] librtmp  (0 B bin / 143043 B dl (+1/-0 Pakete: media-video/rtmpdump) KASKADE)
  [ ] libsoxr  (+133120 B bin / 94384 B dl (+1/-0 Pakete: media-libs/soxr))
  [ ] lv2  (+6481920 B bin / 14779507 B dl (+14/-0 Pakete: dev-libs/libxslt, dev-libs/serd, dev-libs/sord, ...))
  [ ] lzma  (0 B bin / 0 B dl)
  [ ] modplug  (+204800 B bin / 238609 B dl (+1/-0 Pakete: media-libs/libmodplug))
  [ ] nvenc  (0 B bin / 0 B dl KASKADE)
  [ ] ocr  (+7229440 B bin / 31394660 B dl (+3/-0 Pakete: app-text/tessdata_fast, app-text/tesseract, media-libs/leptonica))
  [ ] openal  (+747520 B bin / 1025568 B dl (+1/-0 Pakete: media-libs/openal))
  [ ] opencl  (+102400 B bin / 110431 B dl (+2/-0 Pakete: dev-libs/opencl-icd-loader, virtual/opencl))
  [ ] opencolorio  (+1976320 B bin / 15531519 B dl (+5/-0 Pakete: dev-cpp/pystring, dev-cpp/yaml-cpp, media-libs/opencolorio, ...)) CONFLICT
  [ ] opengl  (0 B bin / 0 B dl)
  [ ] openh264  (0 B bin / 60313204 B dl (+1/-0 Pakete: media-libs/openh264))
  [ ] openmpt  (0 B bin / 1724964 B dl (+1/-0 Pakete: media-libs/libopenmpt))
  [ ] openssl  (0 B bin / 0 B dl)
  [ ] opus  (0 B bin / 0 B dl KASKADE)
  [ ] pulseaudio  (0 B bin / 0 B dl)
  [ ] qrcode  (+92160 B bin / 461562 B dl (+1/-0 Pakete: media-gfx/qrencode))
  [ ] qsv  (+266240 B bin / 12974961 B dl (+1/-0 Pakete: media-libs/libvpl))
  [ ] quirc  (0 B bin / 42916 B dl (+1/-0 Pakete: media-libs/quirc))
  [ ] rabbitmq  (+92160 B bin / 126670 B dl (+1/-0 Pakete: net-libs/rabbitmq-c))
  [ ] rav1e  (0 B bin / 23032235 B dl (+1/-0 Pakete: media-video/rav1e))
  [ ] rist  (+993280 B bin / 8206377 B dl (+3/-0 Pakete: dev-libs/cJSON, net-libs/librist, net-libs/mbedtls))
  [ ] rubberband  (+2805760 B bin / 7700273 B dl (+3/-0 Pakete: media-libs/libsamplerate, media-libs/rubberband, sci-libs/fftw) KASKADE)
  [ ] samba  (+10608640 B bin / 211081490 B dl (+14/-0 Pakete: app-crypt/mit-krb5, dev-libs/libev, dev-libs/libverto, ...) KASKADE)
  [ ] sdl  (+1167360 B bin / 7627356 B dl (+1/-0 Pakete: media-libs/libsdl2))
  [ ] snappy  (+81920 B bin / 1108618 B dl (+1/-0 Pakete: app-arch/snappy))
  [ ] sndio  (+163840 B bin / 162725 B dl (+2/-0 Pakete: acct-user/sndiod, media-sound/sndio))
  [ ] soc  (0 B bin / 702636 B dl (+1/-0 Pakete: media-video/ffmpeg-soc-udev-rules) KASKADE)
  [ ] sofa  (0 B bin / 208427964 B dl (+1/-0 Pakete: media-libs/libmysofa))
  [ ] speex  (+512000 B bin / 1043278 B dl (+1/-0 Pakete: media-libs/speex))
  [ ] srt  (+573440 B bin / 1743040 B dl (+1/-0 Pakete: net-libs/srt))
  [ ] ssh  (0 B bin / 0 B dl)
  [ ] svg  (0 B bin / 0 B dl)
  [ ] svt-av1  (+2068480 B bin / 10225189 B dl (+1/-0 Pakete: media-libs/svt-av1))
  [ ] theora  (+204800 B bin / 1828469 B dl (+1/-0 Pakete: media-libs/libtheora))
  [x] truetype  (0 B bin / 0 B dl KASKADE)
  [ ] twolame  (+143360 B bin / 890908 B dl (+1/-0 Pakete: media-sound/twolame))
  [ ] v4l  (+1433600 B bin / 1384024 B dl (+1/-0 Pakete: media-libs/libv4l))
  [ ] vaapi  (+245760 B bin / 306278 B dl (+1/-0 Pakete: media-libs/libva))
  [ ] vdpau  (+102400 B bin / 143279 B dl (+1/-0 Pakete: x11-libs/libvdpau))
  [ ] verify-sig  (0 B bin / 520 B dl)
  [ ] vidstab  (0 B bin / 130387 B dl (+1/-0 Pakete: media-libs/vidstab) KASKADE)
  [ ] vmaf  (+481280 B bin / 38406134 B dl (+3/-0 Pakete: dev-lang/nasm, dev-util/xxd, media-libs/libvmaf))
  [ ] vorbis  (0 B bin / 0 B dl)
  [ ] vpx  (+1894400 B bin / 5635379 B dl (+1/-0 Pakete: media-libs/libvpx))
  [ ] vulkan  (0 B bin / 0 B dl KASKADE)
  [ ] webp  (0 B bin / 0 B dl)
  [ ] x264  (+573440 B bin / 841594 B dl (+1/-0 Pakete: media-libs/x264) KASKADE)
  [ ] x265  (0 B bin / 1725279 B dl (+1/-0 Pakete: media-libs/x265) KASKADE)
  [x] xml  (0 B bin / 0 B dl)
  [ ] xvid  (+296960 B bin / 698615 B dl (+1/-0 Pakete: media-libs/xvid) KASKADE)
  [ ] zeromq  (+1167360 B bin / 4538766 B dl (+2/-0 Pakete: dev-libs/libsodium, net-libs/zeromq))
  [ ] zimg  (+317440 B bin / 326578 B dl (+1/-0 Pakete: media-libs/zimg))
  [x] zlib  (0 B bin / 0 B dl)
  [ ] zvbi  (0 B bin / 942350 B dl (+1/-0 Pakete: media-libs/zvbi))
perf-report (Phasen: ms / Anzahl):
index_load           489.5 ms
effective_use       3885.9 ms / 295352 calls
package_use     match_calls=295352
resolve             3796.6 ms / 297 calls
build_extra            0.0 ms / 0 calls
cascade             6771.6 ms / 99 calls / 297 rounds
delta               6811.5 ms / 101 runs / 3 noop_skips
big_levers             0.0 ms / 0 runs / 0 noop_skips
index           mask_checks=31786 dep_parses=239
delta_cache     hits=0 misses=0

real	0m1.066s
user	0m6.795s
sys	0m1.667s
```

```
local $ time gentoo-dep-tui/target/release/gentoo-dep-tui big-levers --binhost /tmp/Packages  --perf-report
Flag                           Pakete    Bin-Delta     DL-Delta  Konflikte
openmp                              4   -118323200    -99709420          0
llvm                                1   -117463040   -334443316          0
gui                                 3    -25712640    -12883734          4
icu                                 1    -20940800    -27977255          0
post-quantum                        1    -12441600    -44319868          0
rdw                                 1    -11304960    -18419206          0
build                               5     -8775680    -52012376          0
viewer                              1     -4515840       163030          0
modemmanager                        1     -4136960     -5584336          0
freepg                              1     -3256320      -824386          2
reference                           4     -3010560        34547          2
shaderc                             1     -2304000     -4643210          1
daemon                              1     -2140160     -3381076          0
qmi                                 1     -1802240     -3736056          1
elogind                             4     -1781760     -3626432          0
minimal                            10     -1525760     -9499546          0
iwd                                 1     -1423360     -3841433          0
wifi                                1     -1423360     -3841433          1
gawk                                1     -1361920     -1223787          0
convert                             1     -1218560     -7337236          0
rsync-verify                        1     -1054720     -1383470          0
ensurepip                           1     -1034240     -9314004          0
gpg                                 2      -901120     -1286925          0
perl                                1      -829440     -1147258          0
ppd                                 1      -819200     -2278865          0
dav1d                               1      -645120     -1038852          0
libarchive                          1      -563200     -6735604          1
opengl                              4      -552960     -1403315          2
libinput                            1      -522240     -1711990          0
policykit                           1      -512000     -1482662          0
qmanifest                           1      -512000     -1422222          0
argon2                              1      -501760     -1505307          0
webp                                1      -481280     -4296070          0
mbim                                2      -471040      -229553          0
ssl                                 9      -460800      -944599          8
gnu                                 3      -419840     -1634077          0
ppp                                 1      -419840     -1014852          0
lua                                 1      -389120     -1103140          0
lua_single_target_lua5-1            2      -389120     -1103140          0
lua_single_target_luajit            1      -389120     -1103140          0
debuginfod                          2      -348160     -2239593          0
jpeg2k                              1      -327680     -2157028          0
container-init                      1      -317440       -32369          0
oniguruma                           1      -276480      -979159          0
static-libs                       104      -276480      -979159          3
udev                                8      -266240        -3666          5
quic                                2      -235520      -709136          1
javascript                          1      -215040     -2323029          1
tools                               4      -215040      -617740          1
graphite                            1      -204800     -6656693          0
idn                                 2      -194560     -2234804          0
http2                               1      -194560     -2018996          0
lz4                                 1      -184320      -387114          0
acl                                 2      -184320      -384828          0
passwdqc                            1      -184320      -210003          0
vulkan                              3      -174080     -1820868          0
yescrypt                            1      -163840      -671272          0
cairo                               2      -153600      -666308          0
seccomp                             3      -143360      -656231          0
readline                            9      -133120      -538611          0
gif                                 1      -133120      -470579          0
psl                                 1      -112640     -7624251          0
http3                               1      -112640      -410868          1
xattr                               2      -112640      -343608          1
iconv                               5      -112640      -222648          1
uchardet                            1      -112640      -222648          0
lzo                                 1      -102400      -600622          0
mailutils                           1       -71680       -69834          0
lvm                                 1       -61440       -75224          0
qrtr                                2       -61440       -26540          0
asyncns                             1       -51200      -341591          0
client                              3       -51200       -27709          0
caps                                2       -40960            0          1
minizip                             2       -30720            0          0
binutils-plugin                     1       -20480            0          0
system-bootstrap                    1            0    -12987073          0
introspection                      10            0     -1083172          2
vanilla                             5            0       -14220          0
eselect                             1            0        -3539          0
10bit                               1            0            0          0
64-bit-bfd                          1            0            0          0
8bit                                1            0            0          0
Xaw3d                               1            0            0          0
accessibility                       1            0            0          0
activefilter                        1            0            0          0
ada                                 2            0            0          0
[...]
zic-slim                            1            0            0          0
zlib                                5            0            0          0
zstd                                9            0            0          0
mktemp                              1            0         6754          0
man                                 2            0         8885          0
apng                                1            0        10680          0
examples                           29            0        27036          0
branding                            1            0        29322          0
quirc                               1            0        42916          0
cacert                              2            0        43963          0
rrsync                              1            0        44019          0
libaribb24                          1            0        57378          0
ukify                               1            0        76008          0
verify-provenance                  10            0        97632          0
lzip                                1            0       117718          0
vidstab                             1            0       130387          0
verify-sig                        108            0       134044          0
librtmp                             1            0       143043          0
tcpd                                2            0       143734          0
input_devices_wacom                 1            0       181360          0
[...]
oci8                                1            0     85513991          0
dist-kernel                         1            0    162422598          0
sofa                                2            0    208427964          0
games                               3        20480            0          0
resolvconf                          1        20480            0          0
standalone                          1        20480            0          0
nftables                            2        20480          770          1
busybox                             1        20480      2525473          1
pps                                 1        40960        12949          0
loudness                            1        51200        25890          0
samurai                             1        51200        32709          1
bs2b                                1        51200       606283          0
libunibreak                         1        51200       855733          0
aad                                 1        61440       356247          0
geoip                               1        61440       846612          0
maxminddb                           1        61440       846612          0
pbzip2                              1        71680        48015          1
libproxy                            1        71680        60529          0
gsm                                 1        71680        66597          0
bcg729                              1        71680       122475          0
sbc                                 1        71680       272116          0
xv                                  2        71680       281460          0
mp3                                 1        81920        48672          0
snappy                              2        81920      1108618          0
libcamera                           1        81920      1690242          0
[...]
flite                               1     12492800     20233792          0
sssd                                2     12523520    231891760          0
postgres                            2     13332480    178857816          0
gtk                                 7     16332800     36714458          0
frei0r                              1     18124800    224458175          0
z3                                  1     18606080      6689265          0
source-highlight                    1     20377600    206296700          0
valgrind                           14     20459520     17372682          0
eps                                 1     32235520     85985861          0
nvml                                1     33812480    469144839          0
video_cards_nvidia                  1     33812480    469144839          0
doc                                87     37570560    114872689          0
efl                                 2     41390080    193019651          0
opencl                              3     72222720   1034754306          0
eglfs                               2    111155200    262302930          0
qml                                 2    111155200    262302930          0
X                                  16    111319040    262255794          8
plasma                              1    129300480    150455441          2
kde                                 1    132536320    152788637          3
perf-report (Phasen: ms / Anzahl):
index_load           508.7 ms
effective_use      28398.5 ms / 2089143 calls
package_use     match_calls=2089143
resolve            27905.3 ms / 2102 calls
build_extra            0.0 ms / 0 calls
cascade            49425.9 ms / 700 calls / 2102 rounds
delta                  0.0 ms / 0 runs / 0 noop_skips
big_levers          3189.3 ms / 1 runs / 194 noop_skips
index           mask_checks=31786 dep_parses=239
delta_cache     hits=0 misses=0

real	0m3.825s
user	0m44.012s
sys	0m5.518s
```


look at the tui, in particular emacs with different gui settings:

```
┌Pakete (588)───────────────────────────────┐┌Details / USE-Flags────────────────────────────────────────┐┌Blame / Pros-Cons──────────────────────────┐
│  sys-kernel/gentoo-sources                ││app-editors/emacs  Binpkg: 35.4 MB  Pkg-Quell-DL: 55.3 MB  ││Warum ist das hier?                        │
│  app-admin/sudo                           ││                                                           ││(kein Pfad — Welt-Paket oder leer)         │
│  app-admin/sysklogd                       ││[x] X 0 B bin / 0 B dl                                     ││                                           │
│  app-arch/dpkg                            ││[x] Xaw3d 0 B bin / 0 B dl                                 ││Flag gui zieht:                            │
│  app-containers/docker                    ││[ ] acl +30.0 kB bin / 0 B dl (+1/-0 pkg)                  ││+ x11-libs/libXaw3d                        │
│  app-containers/docker-buildx             ││[ ] alsa 0 B bin / 0 B dl                                  ││                                           │
│  app-containers/docker-cli                ││[ ] athena 0 B bin / 0 B dl                                ││gui: Enable support for a graphical user   │
│  app-crypt/p11-kit                        ││[ ] cairo 0 B bin / 0 B dl                                 ││interface                                  │
│> app-editors/emacs                        ││[ ] dbus 0 B bin / 0 B dl                                  ││                                           │
│  app-misc/fastfetch                       ││[ ] dynamic-loading 0 B bin / 0 B dl                       ││                                           │
│  app-misc/fdupes                          ││[ ] games +20.0 kB bin / 0 B dl (+1/-0 pkg)                ││                                           │
│  app-misc/jq                              ││[ ] gfile 0 B bin / 0 B dl                                 ││                                           │
│  app-misc/mc                              ││[ ] gif 0 B bin / 0 B dl                                   ││                                           │
│  app-misc/tmate                           ││[x] gmp 0 B bin / 0 B dl                                   ││                                           │
│  app-misc/tmux                            ││[ ] gpm +200.0 kB bin / +811.9 kB dl (+1/-0 pkg)           ││                                           │
│  app-pda/ifuse                            ││[ ] gsettings 0 B bin / 0 B dl                             ││                                           │
│  app-portage/cpuid2cpuflags               ││[ ] gtk 0 B bin / 0 B dl                                   ││                                           │
│  app-portage/eix                          ││[ ] gui +220.0 kB bin / +470.7 kB dl (+1/-0 pkg)           ││                                           │
│  app-portage/gentoolkit                   ││[ ] gzip-el 0 B bin / 0 B dl                               ││                                           │
│  app-shells/bash-completion               ││[ ] harfbuzz 0 B bin / 0 B dl                              ││                                           │
│  app-shells/gentoo-zsh-completions        ││[ ] imagemagick 0 B bin / 0 B dl                           ││                                           │
│  app-shells/zsh                           ││[x] inotify 0 B bin / 0 B dl                               ││                                           │
│  app-text/mupdf                           ││[ ] jit 0 B bin / 0 B dl                                   ││                                           │
│  app-text/tree                            ││[ ] jpeg 0 B bin / 0 B dl                                  ││                                           │
│  dev-build/ninja                          ││[ ] lcms 0 B bin / 0 B dl                                  ││                                           │
│  dev-debug/gdb                            ││[x] libxml2 0 B bin / 0 B dl                               ││                                           │
│  dev-debug/ltrace                         ││[ ] livecd 0 B bin / 0 B dl                                ││                                           │
│  dev-debug/strace                         ││[ ] m17n-lib 0 B bin / 0 B dl                              ││                                           │
│  dev-libs/nss                             ││[ ] mailutils -70.0 kB bin / -68.2 kB dl (+0/-2 pkg)       ││                                           │
│  dev-lisp/sbcl                            ││[ ] motif 0 B bin / 0 B dl                                 ││                                           │
│  dev-python/btrfs                         ││[ ] png 0 B bin / 0 B dl                                   ││                                           │
│  dev-python/uv                            ││[ ] sound 0 B bin / 0 B dl                                 ││                                           │
│  dev-vcs/git                              ││[ ] source 0 B bin / 0 B dl                                ││                                           │
│  media-fonts/wqy-zenhei                   ││[ ] sqlite 0 B bin / 0 B dl                                ││                                           │
│  media-gfx/feh                            ││[ ] ssl 0 B bin / 0 B dl                                   ││                                           │
│  media-gfx/scrot                          ││[ ] svg 0 B bin / 0 B dl                                   ││                                           │
│  media-libs/fontconfig                    ││[ ] systemd +10.6 MB bin / +17.7 MB dl (+22/-0 pkg)        ││                                           │
│  media-sound/pulsemixer                   ││[x] threads 0 B bin / 0 B dl                               ││                                           │
│  media-video/mpv                          ││[ ] tiff 0 B bin / 0 B dl                                  ││                                           │
│  media-video/pipewire                     ││[ ] toolkit-scroll-bars 0 B bin / 0 B dl                   ││                                           │
│  media-video/wireplumber                  ││[ ] tree-sitter 0 B bin / +996.3 kB dl (+1/-0 pkg)         ││                                           │
│  net-analyzer/hping                       ││[ ] valgrind +19.5 MB bin / +16.6 MB dl (+1/-0 pkg)        ││                                           │
│  net-analyzer/iftop                       ││[ ] webp 0 B bin / 0 B dl                                  ││                                           │
│  net-analyzer/iptraf-ng                   ││[ ] wide-int 0 B bin / 0 B dl                              ││                                           │
│  net-analyzer/macchanger                  ││[ ] xattr 0 B bin / 0 B dl                                 ││                                           │
│  net-analyzer/netcat                      ││[ ] xft 0 B bin / 0 B dl                                   ││                                           │
│  net-analyzer/nethogs                     ││[x] xpm 0 B bin / 0 B dl                                   ││                                           │
│  net-analyzer/ngrep                       ││[ ] xwidgets 0 B bin / 0 B dl                              ││                                           │
│  net-analyzer/ssmping                     ││[ ] zlib 0 B bin / 0 B dl                                  ││                                           │
│  net-analyzer/wireshark                   ││                                                           ││                                           │
└───────────────────────────────────────────┘└───────────────────────────────────────────────────────────┘└───────────────────────────────────────────┘
```


```
┌Pakete (2) [Filter: editors/emacs]─────────────┐┌Details / USE-Flags────────────────────────────────────────────┐┌Blame / Pros-Cons──────────────────────────────┐
│> app-editors/emacs                            ││app-editors/emacs  Binpkg: 35.4 MB  Pkg-Quell-DL: 55.3 MB      ││Warum ist das hier?                            │
│  || virtual/editor||app-editors/nano,app-edito││                                                               ││(kein Pfad — Welt-Paket oder leer)             │
│                                               ││[x] X 0 B bin / 0 B dl                                         ││                                               │
│                                               ││[x] Xaw3d -220.0 kB bin / -470.7 kB dl (+0/-1 pkg)             ││Flag gui zieht:                                │
│                                               ││[ ] acl +30.0 kB bin / 0 B dl (+1/-0 pkg)                      ││- x11-libs/libXaw3d                            │
│                                               ││[ ] alsa 0 B bin / 0 B dl                                      ││                                               │
│                                               ││[ ] athena 0 B bin / 0 B dl                                    ││gui: Enable support for a graphical user       │
│                                               ││[ ] cairo 0 B bin / 0 B dl                                     ││interface                                      │
│                                               ││[ ] dbus 0 B bin / 0 B dl                                      ││                                               │
│                                               ││[ ] dynamic-loading 0 B bin / 0 B dl                           ││                                               │
│                                               ││[ ] games +20.0 kB bin / 0 B dl (+1/-0 pkg)                    ││                                               │
│                                               ││[ ] gfile 0 B bin / 0 B dl                                     ││                                               │
│                                               ││[ ] gif 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[x] gmp 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[ ] gpm +200.0 kB bin / +811.9 kB dl (+1/-0 pkg)               ││                                               │
│                                               ││[ ] gsettings 0 B bin / 0 B dl                                 ││                                               │
│                                               ││[ ] gtk -220.0 kB bin / -470.7 kB dl (+0/-1 pkg)               ││                                               │
│                                               ││[x] gui -220.0 kB bin / -470.7 kB dl (+0/-1 pkg)               ││                                               │
│                                               ││[ ] gzip-el 0 B bin / 0 B dl                                   ││                                               │
│                                               ││[ ] harfbuzz 0 B bin / 0 B dl                                  ││                                               │
│                                               ││[ ] imagemagick +8.3 MB bin / +9.7 MB dl (+1/-0 pkg)           ││                                               │
│                                               ││[x] inotify 0 B bin / 0 B dl                                   ││                                               │
│                                               ││[ ] jit 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[ ] jpeg 0 B bin / 0 B dl                                      ││                                               │
│                                               ││[ ] lcms 0 B bin / 0 B dl                                      ││                                               │
│                                               ││[x] libxml2 0 B bin / 0 B dl                                   ││                                               │
│                                               ││[ ] livecd 0 B bin / 0 B dl                                    ││                                               │
│                                               ││[ ] m17n-lib 0 B bin / 0 B dl                                  ││                                               │
│                                               ││[ ] mailutils -70.0 kB bin / -68.2 kB dl (+0/-2 pkg)           ││                                               │
│                                               ││[ ] motif -220.0 kB bin / +7.9 MB dl (+1/-1 pkg)               ││                                               │
│                                               ││[ ] png 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[ ] sound 0 B bin / 0 B dl                                     ││                                               │
│                                               ││[ ] source 0 B bin / 0 B dl                                    ││                                               │
│                                               ││[ ] sqlite 0 B bin / 0 B dl                                    ││                                               │
│                                               ││[ ] ssl 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[ ] svg 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[ ] systemd +10.6 MB bin / +17.7 MB dl (+22/-0 pkg)            ││                                               │
│                                               ││[x] threads 0 B bin / 0 B dl                                   ││                                               │
│                                               ││[ ] tiff 0 B bin / 0 B dl                                      ││                                               │
│                                               ││[ ] toolkit-scroll-bars 0 B bin / 0 B dl                       ││                                               │
│                                               ││[ ] tree-sitter 0 B bin / +996.3 kB dl (+1/-0 pkg)             ││                                               │
│                                               ││[ ] valgrind +19.5 MB bin / +16.6 MB dl (+1/-0 pkg)            ││                                               │
│                                               ││[ ] webp 0 B bin / 0 B dl                                      ││                                               │
│                                               ││[ ] wide-int 0 B bin / 0 B dl                                  ││                                               │
│                                               ││[ ] xattr 0 B bin / 0 B dl                                     ││                                               │
│                                               ││[ ] xft 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[x] xpm 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[ ] xwidgets 0 B bin / 0 B dl                                  ││                                               │
│                                               ││[ ] zlib 0 B bin / 0 B dl                                      ││                                               │
│                                               ││                                                               ││                                               │
└───────────────────────────────────────────────┘└───────────────────────────────────────────────────────────────┘└───────────────────────────────────────────────┘
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg 759.0 MB (505/555 Hits)  Quell-DL 3313.4 MB  Build-Only 243.2 MB bin / 1761.3 MB dl  Pakete 555  Konflikte 0
```

```
┌Pakete (2) [Filter: editors/emacs]─────────────┐┌Details / USE-Flags────────────────────────────────────────────┐┌Blame / Pros-Cons──────────────────────────────┐
│> app-editors/emacs                            ││app-editors/emacs  Binpkg: 35.4 MB  Pkg-Quell-DL: 55.3 MB      ││(kein Pfad — Welt-Paket oder leer)             │
│  || virtual/editor||app-editors/nano,app-edito││                                                               ││                                               │
│                                               ││[x] X +320.0 kB bin / +745.1 kB dl (+2/-0 pkg)                 ││Flag gtk zieht:                                │
│                                               ││[x] Xaw3d 0 B bin / 0 B dl                                     ││+ x11-libs/libXaw3d                            │
│                                               ││[ ] acl +30.0 kB bin / 0 B dl (+1/-0 pkg)                      ││                                               │
│                                               ││[ ] alsa 0 B bin / 0 B dl                                      ││gtk: Add support for x11-libs/gtk+ (The GIMP   │
│                                               ││[ ] athena 0 B bin / 0 B dl                                    ││Toolkit)                                       │
│                                               ││[ ] cairo 0 B bin / 0 B dl                                     ││                                               │
│                                               ││[ ] dbus 0 B bin / 0 B dl                                      ││Voll-Ersparnis gtk [app-editors/emacs]:        │
│                                               ││[ ] dynamic-loading 0 B bin / 0 B dl                           ││ohne Flag-Blocker: -163.8 MB bin / 282.2 MB dl │
│                                               ││[ ] games +20.0 kB bin / 0 B dl (+1/-0 pkg)                    ││(-106 pkg)                                     │
│                                               ││[ ] gfile 0 B bin / 0 B dl                                     ││Flag aus bei:                                  │
│                                               ││[ ] gif 0 B bin / 0 B dl                                       ││app-accessibility/at-spi2-core [introspection] │
│                                               ││[x] gmp 0 B bin / 0 B dl                                       ││app-accessibility/at-spi2-core                 │
│                                               ││[ ] gpm +200.0 kB bin / +811.9 kB dl (+1/-0 pkg)               ││[python_targets_python3_14]                    │
│                                               ││[ ] gsettings 0 B bin / 0 B dl                                 ││app-admin/perl-cleaner [!pkgcore]              │
│                                               ││[x] gtk +220.0 kB bin / +470.7 kB dl (+1/-0 pkg)               ││app-admin/sudo [sendmail]                      │
│                                               ││[x] gui 0 B bin / 0 B dl                                       ││app-arch/dpkg [bzip2]                          │
│                                               ││[ ] gzip-el 0 B bin / 0 B dl                                   ││app-arch/dpkg [lzma]                           │
│                                               ││[ ] harfbuzz 0 B bin / 0 B dl                                  ││app-arch/dpkg [zlib]                           │
│                                               ││[ ] imagemagick +8.3 MB bin / +9.7 MB dl (+1/-0 pkg)           ││app-arch/dpkg [zstd]                           │
│                                               ││[x] inotify 0 B bin / 0 B dl                                   ││(...)                                          │
│                                               ││[ ] jit 0 B bin / 0 B dl                                       ││inkl. world-Entfernung: -591.4 MB bin (-353    │
│                                               ││[ ] jpeg 0 B bin / 0 B dl                                      ││pkg)                                           │
│                                               ││[ ] lcms 0 B bin / 0 B dl                                      ││world entfernen:                               │
│                                               ││[x] libxml2 0 B bin / 0 B dl                                   ││app-admin/sudo                                 │
│                                               ││[ ] livecd 0 B bin / 0 B dl                                    ││app-arch/dpkg                                  │
│                                               ││[ ] m17n-lib 0 B bin / 0 B dl                                  ││app-containers/docker                          │
│                                               ││[ ] mailutils -70.0 kB bin / -68.2 kB dl (+0/-2 pkg)           ││app-crypt/p11-kit                              │
│                                               ││[ ] motif 0 B bin / 0 B dl                                     ││app-misc/fastfetch                             │
│                                               ││[ ] png 0 B bin / 0 B dl                                       ││app-misc/fdupes                                │
│                                               ││[ ] sound 0 B bin / 0 B dl                                     ││app-misc/mc                                    │
│                                               ││[ ] source 0 B bin / 0 B dl                                    ││app-misc/tmate                                 │
│                                               ││[ ] sqlite 0 B bin / 0 B dl                                    ││(...)                                          │
│                                               ││[ ] ssl 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[ ] svg 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[ ] systemd +10.6 MB bin / +17.7 MB dl (+22/-0 pkg)            ││                                               │
│                                               ││[x] threads 0 B bin / 0 B dl                                   ││                                               │
│                                               ││[ ] tiff 0 B bin / 0 B dl                                      ││                                               │
│                                               ││[ ] toolkit-scroll-bars 0 B bin / 0 B dl                       ││                                               │
│                                               ││[ ] tree-sitter 0 B bin / +996.3 kB dl (+1/-0 pkg)             ││                                               │
│                                               ││[ ] valgrind +19.5 MB bin / +16.6 MB dl (+1/-0 pkg)            ││                                               │
│                                               ││[ ] webp 0 B bin / 0 B dl                                      ││                                               │
│                                               ││[ ] wide-int 0 B bin / 0 B dl                                  ││                                               │
│                                               ││[ ] xattr 0 B bin / 0 B dl                                     ││                                               │
│                                               ││[ ] xft 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[x] xpm 0 B bin / 0 B dl                                       ││                                               │
│                                               ││[ ] xwidgets +56.5 MB bin / +95.2 MB dl (+16/-0 pkg)           ││                                               │
│                                               ││[ ] zlib 0 B bin / 0 B dl                                      ││                                               │
│                                               ││                                                               ││                                               │
└───────────────────────────────────────────────┘└───────────────────────────────────────────────────────────────┘└───────────────────────────────────────────────┘
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg 758.8 MB (504/554 Hits)  Quell-DL 3313.0 MB  Build-Only 243.2 MB bin / 1761.3 MB dl  Pakete 554  Konflikte 0
```

ffmpeg use flags

```
┌Pakete (1) [Filter: ffmp]──────────────────────────────────────────┐┌Details / USE-Flags────────────────────────────────────────────────────────────────────────┐┌Blame / Pros-Cons──────────────────────────────────────────────────┐
│>   media-video/ffmpeg                                             ││media-video/ffmpeg  Binpkg: 9.9 MB  Pkg-Quell-DL: 11.2 MB                                  ││Warum ist das hier?                                                │
│                                                                   ││                                                                                           ││Kette 1:                                                           │
│                                                                   ││[ ] X +70.0 kB bin / +274.9 kB dl (+1/-0 pkg)                                              ││media-video/mpv -> media-video/ffmpeg                              │
│                                                                   ││[ ] alsa 0 B bin / 0 B dl                                                                  ││Kette 2:                                                           │
│                                                                   ││[ ] amf 0 B bin / +4.6 MB dl (+1/-0 pkg)                                                   ││net-misc/freerdp -> media-video/ffmpeg [+ffmpeg]                   │
│                                                                   ││[ ] amr +160.0 kB bin / +917.2 kB dl (+1/-0 pkg)                                           ││Kette 3:                                                           │
│                                                                   ││[ ] amrenc +90.0 kB bin / +594.7 kB dl (+1/-0 pkg)                                         ││net-misc/tigervnc -> media-video/ffmpeg [+viewer]                  │
│                                                                   ││[ ] appkit 0 B bin / 0 B dl                                                                ││Kette 4:                                                           │
│                                                                   ││[ ] bluray 0 B bin / +489.1 kB dl (+2/-0 pkg)                                              ││www-client/firefox-bin -> media-video/ffmpeg                       │
│                                                                   ││[ ] bs2b +50.0 kB bin / +592.1 kB dl (+1/-0 pkg)                                           ││                                                                   │
│                                                                   ││[ ] bzip2 0 B bin / 0 B dl                                                                 ││gmp: Add support for dev-libs/gmp (GNU MP library)                 │
│                                                                   ││[ ] cairo 0 B bin / 0 B dl                                                                 ││                                                                   │
│                                                                   ││[ ] cdio +550.0 kB bin / +4.0 MB dl (+4/-0 pkg)                                            ││                                                                   │
│                                                                   ││[ ] chromaprint +1.5 MB bin / +5.5 MB dl (+2/-0 pkg)                                       ││                                                                   │
│                                                                   ││[ ] chromium 0 B bin / 0 B dl                                                              ││                                                                   │
│                                                                   ││[ ] codec2 +630.0 kB bin / +7.3 MB dl (+1/-0 pkg)                                          ││                                                                   │
│                                                                   ││[ ] cuda-clang 0 B bin / 0 B dl                                                            ││                                                                   │
│                                                                   ││[x] dav1d -630.0 kB bin / -1014.5 kB dl (+0/-1 pkg)                                        ││                                                                   │
│                                                                   ││[ ] doc 0 B bin / 0 B dl                                                                   ││                                                                   │
│                                                                   ││[x] drm 0 B bin / 0 B dl                                                                   ││                                                                   │
│                                                                   ││[ ] dvd +320.0 kB bin / +550.8 kB dl (+3/-0 pkg)                                           ││                                                                   │
│                                                                   ││[ ] fdk +710.0 kB bin / +2.8 MB dl (+1/-0 pkg)                                             ││                                                                   │
│                                                                   ││[ ] flite +11.9 MB bin / +19.3 MB dl (+1/-0 pkg)                                           ││                                                                   │
│                                                                   ││[x] fontconfig 0 B bin / 0 B dl                                                            ││                                                                   │
│                                                                   ││[ ] frei0r +17.3 MB bin / +214.1 MB dl (+5/-0 pkg)                                         ││                                                                   │
│                                                                   ││[ ] fribidi 0 B bin / 0 B dl                                                               ││                                                                   │
│                                                                   ││[ ] gcrypt 0 B bin / 0 B dl                                                                ││                                                                   │
│                                                                   ││[ ] gme +190.0 kB bin / +335.6 kB dl (+1/-0 pkg)                                           ││                                                                   │
│                                                                   ││[ ] gmp 0 B bin / 0 B dl                                                                   ││                                                                   │
│                                                                   ││[x] gnutls 0 B bin / 0 B dl                                                                ││                                                                   │
│                                                                   ││[x] gpl 0 B bin / 0 B dl                                                                   ││                                                                   │
│                                                                   ││[ ] gsm +70.0 kB bin / +65.0 kB dl (+1/-0 pkg)                                             ││                                                                   │
│                                                                   ││[ ] iec61883 +230.0 kB bin / +867.6 kB dl (+3/-0 pkg)                                      ││                                                                   │
│                                                                   ││[ ] ieee1394 +250.0 kB bin / +905.9 kB dl (+2/-0 pkg)                                      ││                                                                   │
│                                                                   ││[ ] jack +130.0 kB bin / +1.0 MB dl (+3/-0 pkg)                                            ││                                                                   │
│                                                                   ││[ ] jpeg2k 0 B bin / 0 B dl                                                                ││                                                                   │
│                                                                   ││[ ] jpegxl +2.9 MB bin / +5.8 MB dl (+3/-0 pkg)                                            ││                                                                   │
│                                                                   ││[ ] kvazaar +290.0 kB bin / +586.2 kB dl (+1/-0 pkg)                                       ││                                                                   │
│                                                                   ││[ ] ladspa 0 B bin / 0 B dl                                                                ││                                                                   │
│                                                                   ││[x] lame 0 B bin / 0 B dl                                                                  ││                                                                   │
│                                                                   ││[ ] lcms 0 B bin / 0 B dl                                                                  ││                                                                   │
│                                                                   ││[ ] libaom +1.5 MB bin / +5.2 MB dl (+1/-0 pkg)                                            ││                                                                   │
│                                                                   ││[ ] libaribb24 0 B bin / +56.0 kB dl (+1/-0 pkg)                                           ││                                                                   │
│                                                                   ││[x] libass 0 B bin / 0 B dl                                                                ││                                                                   │
│                                                                   ││[ ] libcaca +290.0 kB bin / +872.4 kB dl (+1/-0 pkg)                                       ││                                                                   │
│                                                                   ││[ ] libilbc +100.0 kB bin / +1.2 MB dl (+1/-0 pkg)                                         ││                                                                   │
│                                                                   ││[ ] liblc3 +130.0 kB bin / +1.6 MB dl (+1/-0 pkg)                                          ││                                                                   │
│                                                                   ││[ ] libplacebo 0 B bin / 0 B dl                                                            ││                                                                   │
│                                                                   ││[ ] librtmp 0 B bin / +139.7 kB dl (+1/-0 pkg)                                             ││                                                                   │
│                                                                   ││[ ] libsoxr +130.0 kB bin / +92.2 kB dl (+1/-0 pkg)                                        ││                                                                   │
│                                                                   ││[ ] lv2 +6.2 MB bin / +14.1 MB dl (+14/-0 pkg)                                             ││                                                                   │
│                                                                   ││[ ] lzma 0 B bin / 0 B dl                                                                  ││                                                                   │
│                                                                   ││[ ] modplug +200.0 kB bin / +233.0 kB dl (+1/-0 pkg)                                       ││                                                                   │
│                                                                   ││[ ] nvenc 0 B bin / 0 B dl                                                                 ││                                                                   │
│                                                                   ││[ ] ocr +6.9 MB bin / +29.9 MB dl (+3/-0 pkg)                                              ││                                                                   │
│                                                                   ││[ ] openal +730.0 kB bin / +1001.5 kB dl (+1/-0 pkg)                                       ││                                                                   │
│                                                                   ││[ ] opencl +100.0 kB bin / +107.8 kB dl (+2/-0 pkg)                                        ││                                                                   │
│                                                                   ││[ ] opencolorio +1.9 MB bin / +14.8 MB dl (+5/-0 pkg) CONFLICT                             ││                                                                   │
│                                                                   ││[ ] opengl 0 B bin / 0 B dl                                                                ││                                                                   │
│                                                                   ││[ ] openh264 0 B bin / +57.5 MB dl (+1/-0 pkg)                                             ││                                                                   │
│                                                                   ││[ ] openmpt 0 B bin / +1.6 MB dl (+1/-0 pkg)                                               ││                                                                   │
│                                                                   ││[ ] openssl 0 B bin / 0 B dl                                                               ││                                                                   │
│                                                                   ││[ ] opus 0 B bin / 0 B dl                                                                  ││                                                                   │
│                                                                   ││[ ] pulseaudio 0 B bin / 0 B dl                                                            ││                                                                   │
│                                                                   ││[ ] qrcode +90.0 kB bin / +450.7 kB dl (+1/-0 pkg)                                         ││                                                                   │
│                                                                   ││[ ] qsv +260.0 kB bin / +12.4 MB dl (+1/-0 pkg)                                            ││                                                                   │
│                                                                   ││[ ] quirc 0 B bin / +41.9 kB dl (+1/-0 pkg)                                                ││                                                                   │
│                                                                   ││[ ] rabbitmq +90.0 kB bin / +123.7 kB dl (+1/-0 pkg)                                       ││                                                                   │
│                                                                   ││[ ] rav1e 0 B bin / +22.0 MB dl (+1/-0 pkg)                                                ││                                                                   │
│                                                                   ││[ ] rist +970.0 kB bin / +7.8 MB dl (+3/-0 pkg)                                            ││                                                                   │
│                                                                   ││[ ] rubberband +2.7 MB bin / +7.3 MB dl (+3/-0 pkg)                                        ││                                                                   │
│                                                                   ││[ ] samba +10.1 MB bin / +201.3 MB dl (+14/-0 pkg)                                         ││                                                                   │
│                                                                   ││[ ] sdl +1.1 MB bin / +7.3 MB dl (+1/-0 pkg)                                               ││                                                                   │
│                                                                   ││[ ] snappy +80.0 kB bin / +1.1 MB dl (+1/-0 pkg)                                           ││                                                                   │
│                                                                   ││[ ] sndio +160.0 kB bin / +158.9 kB dl (+2/-0 pkg)                                         ││                                                                   │
│                                                                   ││[ ] soc 0 B bin / +686.2 kB dl (+1/-0 pkg)                                                 ││                                                                   │
│                                                                   ││[ ] sofa 0 B bin / +198.8 MB dl (+1/-0 pkg)                                                ││                                                                   │
│                                                                   ││[ ] speex +500.0 kB bin / +1018.8 kB dl (+1/-0 pkg)                                        ││                                                                   │
│                                                                   ││[ ] srt +560.0 kB bin / +1.7 MB dl (+1/-0 pkg)                                             ││                                                                   │
│                                                                   ││[ ] ssh 0 B bin / 0 B dl                                                                   ││                                                                   │
│                                                                   ││[ ] svg 0 B bin / 0 B dl                                                                   ││                                                                   │
│                                                                   ││[ ] svt-av1 +2.0 MB bin / +9.8 MB dl (+1/-0 pkg)                                           ││                                                                   │
│                                                                   ││[ ] theora +200.0 kB bin / +1.7 MB dl (+1/-0 pkg)                                          ││                                                                   │
│                                                                   ││[x] truetype 0 B bin / 0 B dl                                                              ││                                                                   │
│                                                                   ││[ ] twolame +140.0 kB bin / +870.0 kB dl (+1/-0 pkg)                                       ││                                                                   │
│                                                                   ││[ ] v4l +1.4 MB bin / +1.3 MB dl (+1/-0 pkg)                                               ││                                                                   │
│                                                                   ││[ ] vaapi +240.0 kB bin / +299.1 kB dl (+1/-0 pkg)                                         ││                                                                   │
│                                                                   ││[ ] vdpau +100.0 kB bin / +139.9 kB dl (+1/-0 pkg)                                         ││                                                                   │
│                                                                   ││[ ] verify-sig 0 B bin / +520 B dl                                                         ││                                                                   │
│                                                                   ││[ ] vidstab 0 B bin / +127.3 kB dl (+1/-0 pkg)                                             ││                                                                   │
│                                                                   ││[ ] vmaf +470.0 kB bin / +36.6 MB dl (+3/-0 pkg)                                           ││                                                                   │
│                                                                   ││[ ] vorbis 0 B bin / 0 B dl                                                                ││                                                                   │
│                                                                   ││[ ] vpx +1.8 MB bin / +5.4 MB dl (+1/-0 pkg)                                               ││                                                                   │
│                                                                   ││[ ] vulkan 0 B bin / 0 B dl                                                                ││                                                                   │
│                                                                   ││[ ] webp 0 B bin / 0 B dl                                                                  ││                                                                   │
│                                                                   ││[ ] x264 +560.0 kB bin / +821.9 kB dl (+1/-0 pkg)                                          ││                                                                   │
│                                                                   ││[ ] x265 0 B bin / +1.6 MB dl (+1/-0 pkg)                                                  ││                                                                   │
│                                                                   ││[x] xml 0 B bin / 0 B dl                                                                   ││                                                                   │
│                                                                   ││[ ] xvid +290.0 kB bin / +682.2 kB dl (+1/-0 pkg)                                          ││                                                                   │
│                                                                   ││[ ] zeromq +1.1 MB bin / +4.3 MB dl (+2/-0 pkg)                                            ││                                                                   │
│                                                                   ││[ ] zimg +310.0 kB bin / +318.9 kB dl (+1/-0 pkg)                                          ││                                                                   │
│                                                                   ││[x] zlib 0 B bin / 0 B dl                                                                  ││                                                                   │
│                                                                   ││[ ] zvbi 0 B bin / +920.3 kB dl (+1/-0 pkg)                                                ││                                                                   │
└───────────────────────────────────────────────────────────────────┘└───────────────────────────────────────────────────────────────────────────────────────────┘└───────────────────────────────────────────────────────────────────┘
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg 758.8 MB (504/554 Hits)  Quell-DL 3313.0 MB  Build-Only 243.2 MB bin / 1761.3 MB dl  Pakete 554  Konflikte 0
```


git ist auch interessant:

```
┌Pakete (588)───────────────────────────────────┐┌Details / USE-Flags───────────────────────────────────────────┐┌Blame / Pros-Cons──────────────────────────────┐
│  app-arch/dpkg                                ││dev-vcs/git  Binpkg: 9.8 MB  Pkg-Quell-DL: 8.7 MB             ││Warum ist das hier?                            │
│  app-containers/docker                        ││                                                              ││Kette 1:                                       │
│  app-containers/docker-buildx                 ││[ ] cgi +780.0 kB bin / +689.8 kB dl (+19/-0 pkg)             ││app-containers/docker -> dev-vcs/git           │
│  app-containers/docker-cli                    ││[x] curl 0 B bin / 0 B dl CONFLICT                            ││                                               │
│  app-crypt/p11-kit                            ││[ ] cvs +3.2 MB bin / +3.5 MB dl (+8/-0 pkg)                  ││Flag tk zieht:                                 │
│  app-editors/emacs                            ││[ ] debug 0 B bin / 0 B dl                                    ││+ dev-lang/tcl                                 │
│  app-misc/fastfetch                           ││[ ] doc 0 B bin / +1.7 MB dl                                  ││+ dev-lang/tk                                  │
│  app-misc/fdupes                              ││[x] gpg 0 B bin / 0 B dl                                      ││                                               │
│  app-misc/jq                                  ││[ ] highlight 0 B bin / 0 B dl                                ││tk: Include the 'gitk' and 'git gui' tools     │
│  app-misc/mc                                  ││[x] iconv 0 B bin / 0 B dl                                    ││                                               │
│  app-misc/tmate                               ││[ ] keyring +1.7 MB bin / +2.0 MB dl (+6/-0 pkg)              ││                                               │
│  app-misc/tmux                                ││[x] nls 0 B bin / 0 B dl                                      ││                                               │
│  app-pda/ifuse                                ││[x] pcre 0 B bin / 0 B dl                                     ││                                               │
│  app-portage/cpuid2cpuflags                   ││[ ] perforce 0 B bin / 0 B dl                                 ││                                               │
│  app-portage/eix                              ││[x] perl -810.0 kB bin / -1.1 MB dl (+0/-12 pkg)              ││                                               │
│  app-portage/gentoolkit                       ││[ ] python_single_target_python3_12 0 B bin / 0 B dl          ││                                               │
│  app-shells/bash-completion                   ││[ ] python_single_target_python3_13 0 B bin / 0 B dl          ││                                               │
│  app-shells/gentoo-zsh-completions            ││[x] python_single_target_python3_14 0 B bin / 0 B dl          ││                                               │
│  app-shells/zsh                               ││[x] rust 0 B bin / 0 B dl                                     ││                                               │
│  app-text/mupdf                               ││[x] safe-directory 0 B bin / 0 B dl                           ││                                               │
│  app-text/tree                                ││[ ] subversion +4.0 MB bin / +10.9 MB dl (+34/-0 pkg)         ││                                               │
│  dev-build/ninja                              ││[ ] test 0 B bin / 0 B dl                                     ││                                               │
│  dev-debug/gdb                                ││[ ] tk +4.9 MB bin / +11.1 MB dl (+2/-0 pkg)                  ││                                               │
│  dev-debug/ltrace                             ││[x] webdav 0 B bin / 0 B dl                                   ││                                               │
│  dev-debug/strace                             ││[ ] xinetd 0 B bin / 0 B dl                                   ││                                               │
│  dev-libs/nss                                 ││                                                              ││                                               │
│  dev-lisp/sbcl                                ││                                                              ││                                               │
│  dev-python/btrfs                             ││                                                              ││                                               │
│  dev-python/uv                                ││                                                              ││                                               │
│> dev-vcs/git                                  ││                                                              ││                                               │
└───────────────────────────────────────────────┘└──────────────────────────────────────────────────────────────┘└───────────────────────────────────────────────┘
──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg 758.8 MB (504/554 Hits)  Quell-DL 3313.0 MB  Build-Only 243.2 MB bin / 1761.3 MB dl  Pakete 554  Konflikte 0
```

```
┌Pakete (588)───────────────────────────────┐┌Details / USE-Flags────────────────────────────────────────┐┌Blame / Pros-Cons──────────────────────────┐
│  app-portage/gentoolkit                   ││dev-debug/ltrace  Binpkg: 0 B  Pkg-Quell-DL: 290.0 kB      ││Warum ist das hier?                        │
│  app-shells/bash-completion               ││                                                           ││Kette 1:                                   │
│  app-shells/gentoo-zsh-completions        ││[ ] debug 0 B bin / 0 B dl                                 ││                                           │
│  app-shells/zsh                           ││[ ] elfutils 0 B bin / 0 B dl                              ││Flag unwind zieht:                         │
│  app-text/mupdf                           ││[ ] test 0 B bin / 0 B dl                                  ││+ sys-libs/libunwind                       │
│  app-text/tree                            ││[ ] unwind +160.0 kB bin / +986.6 kB dl (+1/-0 pkg)        ││                                           │
│  dev-build/ninja                          ││                                                           ││unwind: Use sys-libs/libunwind for frame   │
│  dev-debug/gdb                            ││                                                           ││unwinding support                          │
│> dev-debug/ltrace                         ││                                                           ││                                           │
└───────────────────────────────────────────┘└───────────────────────────────────────────────────────────┘└───────────────────────────────────────────┘
───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg 758.8 MB (504/554 Hits)  Quell-DL 3313.0 MB  Build-Only 243.2 MB bin / 1761.3 MB dl  Pakete 554  Konflikte 0
```

```
┌Pakete (588)────────────────────────────────────┐┌Details / USE-Flags─────────────────────────────────────────────┐┌Blame / Pros-Cons──────────────────────────────┐
│  sys-kernel/gentoo-sources                     ││app-text/mupdf  Binpkg: 26.7 MB  Pkg-Quell-DL: 63.9 MB          ││Warum ist das hier?                            │
│  app-admin/sudo                                ││                                                                ││Kette 1:                                       │
│  app-admin/sysklogd                            ││[x] X 0 B bin / 0 B dl                                          ││                                               │
│  app-arch/dpkg                                 ││[ ] archive 0 B bin / 0 B dl                                    ││Flag javascript zieht:                         │
│  app-containers/docker                         ││[ ] barcode +590.0 kB bin / +984.9 kB dl (+1/-0 pkg)            ││- dev-lang/mujs                                │
│  app-containers/docker-buildx                  ││[ ] brotli +390.0 kB bin / +631.4 kB dl (+1/-0 pkg)             ││                                               │
│  app-containers/docker-cli                     ││[x] javascript -210.0 kB bin / -2.2 MB dl (+0/-1 pkg) CONFLICT  ││javascript: Enable javascript support          │
│  app-crypt/p11-kit                             ││[x] jpeg2k -320.0 kB bin / -2.1 MB dl (+0/-1 pkg)               ││                                               │
│  app-editors/emacs                             ││[x] opengl -260.0 kB bin / -526.1 kB dl (+0/-1 pkg)             ││Voll-Ersparnis javascript [app-text/mupdf]:    │
│  app-misc/fastfetch                            ││[ ] ssl 0 B bin / 0 B dl                                        ││ohne Flag-Blocker: -3.4 MB bin / -8.1 MB dl (-7│
│  app-misc/fdupes                               ││                                                                ││pkg)                                           │
│  app-misc/jq                                   ││                                                                ││Flag aus bei:                                  │
│  app-misc/mc                                   ││                                                                ││app-misc/fdupes [ncurses]                      │
│  app-misc/tmate                                ││                                                                ││app-misc/mc [slang]                            │
│  app-misc/tmux                                 ││                                                                ││app-shells/bash [readline]                     │
│  app-pda/ifuse                                 ││                                                                ││dev-cpp/tbb [!kernel_Darwin]                   │
│  app-portage/cpuid2cpuflags                    ││                                                                ││dev-db/sqlite [readline]                       │
│  app-portage/eix                               ││                                                                ││dev-lang/perl [!minimal]                       │
│  app-portage/gentoolkit                        ││                                                                ││dev-lang/python [!libedit]                     │
│  app-shells/bash-completion                    ││                                                                ││dev-lang/python [ncurses]                      │
│  app-shells/gentoo-zsh-completions             ││                                                                ││(...)                                          │
│  app-shells/zsh                                ││                                                                ││inkl. world-Entfernung: -64.7 MB bin (-53 pkg) │
│> app-text/mupdf                                ││                                                                ││world entfernen:                               │
│  app-text/tree                                 ││                                                                ││app-admin/sudo                                 │
│  dev-build/ninja                               ││                                                                ││app-arch/dpkg                                  │
│  dev-debug/gdb                                 ││                                                                ││app-editors/emacs                              │
│  dev-debug/ltrace                              ││                                                                ││app-misc/tmux                                  │
│  dev-debug/strace                              ││                                                                ││app-shells/bash-completion                     │
│  dev-libs/nss                                  ││                                                                ││app-shells/zsh                                 │
│  dev-lisp/sbcl                                 ││                                                                ││dev-debug/gdb                                  │
│  dev-python/btrfs                              ││                                                                ││media-video/pipewire                           │
│  dev-python/uv                                 ││                                                                ││(...)                                          │
└────────────────────────────────────────────────┘└────────────────────────────────────────────────────────────────┘└───────────────────────────────────────────────┘
─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Binpkg 758.8 MB (504/554 Hits)  Quell-DL 3313.0 MB  Build-Only 243.2 MB bin / 1761.3 MB dl  Pakete 554  Konflikte 0
```
