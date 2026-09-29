#!/bin/bash
# emulator_e2e.sh — HIL-Test: echte App im Android-Emulator gegen echten lbw-server.
#
#   Xvfb :99  „entfernter“ Rechner: xterm + lbw-server --listen 0.0.0.0:7878
#   Emulator  (KVM, headless) erreicht den Host als 10.0.2.2
#   sshd      :2222 (scripts/test_sshd.sh) für den Tunnel-Durchlauf
#
# Nachweise (Exit 0 nur wenn alle gelten):
#   1. Direktverbindung: "link up", OCR-Text des xterm kommt in der App an
#   2. Tap (Modus Direkt) auf Szene (200,150) → Zeiger auf :99 steht dort
#   3. adb-Tastatureingabe und Bildschirmtastatur (commitText) → xterm → OCR → App
#   4. Trackpad: Tap klickt am Zeiger (Mitte), Wischen bewegt ihn relativ nach rechts
#   5. SSH-Tunnel (JSch auf Android): Fingerabdruck = ssh-keygen, Text kommt an,
#      Pin in den Prefs, Passwort nicht
#   6. HOME → onStop trennt, erneuter Start → onStart verbindet neu
#   7. Tastatur startet verborgen; ein Zurück schließt die Sitzung → Formular
#   Screenshots und Logs in $OUT.
#
# Aufruf: scripts/emulator_e2e.sh [out-dir]
#   Läuft bereits ein Emulator (in `adb devices`), wird er benutzt und nicht beendet.
#   APK: vorher scripts/build_android.sh (oder APK=pfad).
set -u
HERE="$(cd "$(dirname "$0")/.." && pwd)"
SRC6="$(cd "$HERE/.." && pwd)"
OUT="${1:-/tmp/lbw-emu}"
export ANDROID_HOME="${ANDROID_HOME:-/opt/android-sdk}"
export ANDROID_SDK_ROOT="$ANDROID_HOME"
ADB="$ANDROID_HOME/platform-tools/adb"
EMU="$ANDROID_HOME/emulator/emulator"
AVD="${AVD:-lbw36}"
IMAGE="${IMAGE:-system-images;android-36;default;x86_64}"
APK="${APK:-$HERE/android-app/app/build/outputs/apk/debug/app-debug.apk}"
PKG=de.lbw.client
PORT=7878
mkdir -p "$OUT"
rm -f "$OUT"/*.png "$OUT"/*.log

[ -f "$APK" ] || { echo "APK fehlt: $APK (scripts/build_android.sh)"; exit 1; }
(cd "$SRC6" && cargo build --release -q -p lbw-server && ./scripts/fetch_models.sh >/dev/null) || exit 1

PIDS=()
OWN_EMU=0
cleanup() {
  [ "$OWN_EMU" = 1 ] && "$ADB" emu kill >/dev/null 2>&1
  (cd "$HERE" && scripts/test_sshd.sh stop >/dev/null 2>&1)
  kill "${PIDS[@]}" 2>/dev/null; wait 2>/dev/null
}
trap cleanup EXIT

FAIL=0
check() { if eval "$2"; then echo "OK   $1"; else echo "FAIL $1"; FAIL=1; fi; }
wait_log() { # timeout-s muster → logcat (Tag lbw) seit dem letzten Löschen
  local end=$(( $(date +%s) + $1 ))
  while [ "$(date +%s)" -lt "$end" ]; do
    "$ADB" logcat -d -s lbw:I | grep -q -- "$2" && return 0; sleep 0.5
  done
  return 1
}
launch() { "$ADB" logcat -c; "$ADB" shell am start -S -n "$PKG/.MainActivity" --ez autoconnect true --ez hud true "$@" >/dev/null; }
shot() { "$ADB" exec-out screencap -p >"$OUT/$1.png"; }
pointer() { DISPLAY=:99 xdotool getmouselocation | sed -E 's/x:([0-9]+) y:([0-9]+).*/\1 \2/'; }

# ---- Emulator ---------------------------------------------------------------
# Ein gelisteter Emulator (auch noch bootend/offline) wird benutzt, nicht beendet.
if ! "$ADB" devices | grep -q "^emulator-"; then
  if ! "$ANDROID_HOME"/cmdline-tools/*/bin/avdmanager list avd -c 2>/dev/null | grep -qx "$AVD"; then
    echo no | "$ANDROID_HOME"/cmdline-tools/*/bin/avdmanager create avd -n "$AVD" -k "$IMAGE" -d pixel_6 --force >/dev/null || exit 1
  fi
  "$EMU" -avd "$AVD" -no-window -gpu swiftshader_indirect -no-audio -no-snapshot -no-boot-anim \
    -memory 3072 -cores 4 >"$OUT/emulator.log" 2>&1 & PIDS+=($!)
  OWN_EMU=1
fi
timeout 300 "$ADB" wait-for-device || { echo "kein Emulator"; exit 1; }
end=$(( $(date +%s) + 300 ))
until [ "$("$ADB" shell getprop sys.boot_completed 2>/dev/null | tr -d '\r')" = 1 ]; do
  [ "$(date +%s)" -lt "$end" ] || { echo "Boot-Timeout"; exit 1; }; sleep 2
done
"$ADB" shell settings put system screen_off_timeout 1800000
"$ADB" shell input keyevent WAKEUP
# Die virtuelle QWERTY-Tastatur des Emulators gilt als Hardware-Tastatur und
# unterdrückt sonst die Bildschirmtastatur (mIsInputViewShown=false).
"$ADB" shell settings put secure show_ime_with_hard_keyboard 1
"$ADB" install -r "$APK" >/dev/null || exit 1

# ---- „Entfernter“ Rechner -----------------------------------------------------
Xvfb :99 -screen 0 1280x1024x24 >"$OUT/xvfb.log" 2>&1 & PIDS+=($!)
sleep 2
DISPLAY=:99 xterm -geometry 70x30+0+0 -fa Monospace -fs 13 -bg white -fg black \
  -e bash --norc -c 'echo HELLO ANDROID 123; exec bash --norc -i' >"$OUT/xterm.log" 2>&1 & PIDS+=($!)
sleep 3
(cd "$SRC6" && exec target/release/lbw-server --display :99 --listen 0.0.0.0:$PORT -v) >"$OUT/server.log" 2>&1 & PIDS+=($!)
sleep 2

# ---- 1–4 Direktverbindung -----------------------------------------------------
launch --es addr "10.0.2.2:$PORT" --es mode direct
check "1a link up (direkt)" 'wait_log 30 "link up"'
check "1b OCR-Text in der App" 'wait_log 30 "HELLO ANDROID 123"'
shot 01_direct

# Szene → Bildschirm: Viewport passt 640² mittig in die View ein ("view WxH@X,Y")
VIEW=$("$ADB" logcat -d -s lbw:I | grep -o "view [0-9]*x[0-9]*@[0-9]*,[0-9]*" | tail -1)
read -r VW VH VX VY <<<"$(echo "$VIEW" | sed -E 's/view ([0-9]+)x([0-9]+)@([0-9]+),([0-9]+)/\1 \2 \3 \4/')"
to_screen() { awk -v w="$VW" -v h="$VH" -v x0="$VX" -v y0="$VY" -v sx="$1" -v sy="$2" 'BEGIN {
  s = (w < h ? w : h) / 640; ox = (w - 640 * s) / 2; oy = (h - 640 * s) / 2
  printf "%d %d", x0 + ox + (sx + 0.5) * s, y0 + oy + (sy + 0.5) * s }'; }
echo "     $VIEW"
read -r TX TY <<<"$(to_screen 200 150)"
"$ADB" shell input tap "$TX" "$TY"
sleep 2
read -r PX PY <<<"$(pointer)"
check "2 Tap → Zeiger auf :99 bei 200,150 (ist $PX,$PY)" '[ "$PX" = 200 ] && [ "$PY" = 150 ]'

"$ADB" shell input text "echo%styped%son%sandroid"
"$ADB" shell input keyevent ENTER
check "3 Tippen → xterm → OCR → App" 'wait_log 30 "| typed on android"'
shot 02_typed

# Bildschirmtastatur (LatinIME, Pixel-6-Layout) → InputConnection.commitText
BTN=$("$ADB" shell uiautomator dump /sdcard/ui.xml >/dev/null && "$ADB" shell cat /sdcard/ui.xml |
  grep -o 'text="⌨"[^>]*bounds="\[[0-9]*,[0-9]*\]' | grep -o '[0-9]*,[0-9]*\]$' | tr -d ']' | tr , ' ')
read -r KX KY <<<"$BTN"
"$ADB" shell input tap $((KX + 20)) $((KY + 20))
sleep 2
check "3b ⌨ öffnet die Bildschirmtastatur" '"$ADB" shell dumpsys input_method | grep -q "mIsInputViewShown=true"'
read -r SW SH <<<"$("$ADB" shell wm size | grep -o '[0-9]*x[0-9]*' | tail -1 | tr x ' ')"
key() { # Tastenmitte als Anteil der Bildschirmgröße (Tausendstel)
  case "$1" in
    e) set -- 248 703 ;; c) set -- 398 840 ;; h) set -- 597 771 ;; o) set -- 850 703 ;;
    ' ') set -- 498 907 ;; enter) set -- 924 907 ;;
  esac
  "$ADB" shell input tap $((SW * $1 / 1000)) $((SH * $2 / 1000)); sleep 0.3
}
for k in e c h o ' ' o h c e enter; do key "$k"; done
check "3c Bildschirmtastatur → xterm → OCR → App" 'wait_log 30 "| ohce"'
shot 02b_ime

launch --es addr "10.0.2.2:$PORT" --es mode trackpad
wait_log 30 "link up" >/dev/null
# Der Trackpad-Zeiger beginnt in der Szenenmitte; ein Tap klickt dort.
read -r AX AY <<<"$(to_screen 500 500)"
"$ADB" shell input tap "$AX" "$AY"
sleep 2
read -r P0X P0Y <<<"$(pointer)"
check "4a Trackpad-Tap klickt am Zeiger (Mitte 320,320; ist $P0X,$P0Y)" '[ "$P0X" = 320 ] && [ "$P0Y" = 320 ]'
read -r AX AY <<<"$(to_screen 100 400)"
read -r BX BY <<<"$(to_screen 300 400)"
"$ADB" shell input swipe "$AX" "$AY" "$BX" "$BY" 400
sleep 2
read -r P1X P1Y <<<"$(pointer)"
check "4b Trackpad-Wischen bewegt Zeiger nach rechts ($P0X,$P0Y → $P1X,$P1Y)" \
  '[ "$P1X" -gt $((P0X + 50)) ] && [ "$((P1Y - P0Y))" -lt 20 ] && [ "$((P0Y - P1Y))" -lt 20 ]'

# ---- 5 SSH-Tunnel -------------------------------------------------------------
eval "$(cd "$HERE" && LBW_SSHD_PASSWORD=pw scripts/test_sshd.sh start)" || exit 1
"$ADB" shell run-as "$PKG" rm -f shared_prefs/lbw.xml
launch --es ssh_host 10.0.2.2 --ei ssh_port "$LBW_SSHD_PORT" --es ssh_user "$LBW_SSHD_PWUSER" \
  --es ssh_password pw --ei remote_port "$PORT"
check "5a SSH-Tunnel steht" 'wait_log 40 "ssh tunnel up"'
FP=$("$ADB" logcat -d -s lbw:I | grep -o "fp=SHA256:[A-Za-z0-9+/]*" | tail -1 | cut -c4-)
KEYS=$(for k in /tmp/lbw-sshd/host_*.pub; do ssh-keygen -lf "$k"; done)
echo "     Fingerabdruck $FP $(echo "$KEYS" | grep -F -- "$FP" | grep -o "([A-Z0-9]*)$")"
check "5b Fingerabdruck = ssh-keygen" '[ -n "$FP" ] && echo "$KEYS" | grep -qF -- "$FP"'
check "5c OCR-Text über den Tunnel" 'wait_log 30 "typed on android"'
PREFS=$("$ADB" shell run-as "$PKG" cat shared_prefs/lbw.xml)
check "5d Host-Key-Pin gespeichert" 'echo "$PREFS" | grep -qF "$FP"'
check "5e Passwort nicht gespeichert" '! echo "$PREFS" | grep -q "ssh_password"'
shot 03_ssh

# ---- 6 Lebenszyklus -----------------------------------------------------------
"$ADB" logcat -c
"$ADB" shell input keyevent HOME
check "6a onStop trennt" 'wait_log 10 "stop: closing connection"'
"$ADB" shell am start -n "$PKG/.MainActivity" >/dev/null 2>&1
check "6b onStart verbindet neu" 'wait_log 20 "start: reconnecting"'
check "6c Tunnel und Verbindung wieder da (gleicher Pin)" 'wait_log 30 "link up" && wait_log 5 "fp=$FP"'
shot 04_resumed

# ---- 7 Zurück ------------------------------------------------------------------
IME=$("$ADB" shell dumpsys input_method | grep -o "mInputShown=[a-z]*" | head -1)
check "7a Tastatur beim Start verborgen ($IME)" '[ "$IME" = mInputShown=false ]'
"$ADB" shell input keyevent BACK
check "7b ein Zurück schließt die Sitzung" 'wait_log 5 "back: session closed"'
check "7c Formular sichtbar" '"$ADB" shell uiautomator dump /sdcard/ui.xml >/dev/null && "$ADB" shell cat /sdcard/ui.xml | grep -q "Verbinden\|VERBINDEN"'
shot 05_form

"$ADB" logcat -d >"$OUT/logcat.log"
echo "     Logs/Screenshots: $OUT"
exit $FAIL
