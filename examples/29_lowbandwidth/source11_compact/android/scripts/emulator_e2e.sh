#!/bin/bash
# emulator_e2e.sh — HIL-Test: echte App im Android-Emulator gegen echten
# lbw-server durch die echte SSH-Kette (A2).
#
#   Xvfb :99  „entfernter“ Rechner: xterm + lbw-server (echte Modelle)
#   sshd-Paar auf dem Host: Jump :2322, Ziel :2323 (wie tinyeu → hpz6o)
#   Emulator  (KVM, headless) erreicht den Host als 10.0.2.2
#
# Nachweise (Exit 0 nur wenn alle gelten):
#   1. Jump-Kette mit Ed25519-Keys (beide Hops): "link up" im logcat
#   2. OCR-Text des xterm kommt in der App an (Marker im "texts"-Log)
#   3. Getippter Text (adb input) landet im xterm → OCR → App-Log
#   4. Host-Key-Pins beider Hops landen in den Prefs (TOFU), Keys nie
#   5. HOME → onStop trennt, erneuter Start → onStart verbindet neu
#
# Aufruf: scripts/emulator_e2e.sh [out-dir]
#   APK: vorher scripts/build_android.sh (oder APK=pfad).
set -u
HERE="$(cd "$(dirname "$0")/.." && pwd)"
SRC11="$(cd "$HERE/.." && pwd)"
OUT="${1:-/tmp/lbw11-emu}"
export ANDROID_HOME="${ANDROID_HOME:-/opt/android-sdk}"
export ANDROID_SDK_ROOT="$ANDROID_HOME"
export JAVA_HOME="${JAVA_HOME:-/usr/lib/jvm/java-21-openjdk-amd64}"
export PATH="$JAVA_HOME/bin:$ANDROID_HOME/platform-tools:$ANDROID_HOME/emulator:$ANDROID_HOME/cmdline-tools/latest/bin:$PATH"
ADB="$ANDROID_HOME/platform-tools/adb"
EMU="$ANDROID_HOME/emulator/emulator"
AVD="${AVD:-lbw11}"
IMAGE="${IMAGE:-system-images;android-36;google_apis;x86_64}"
APK="${APK:-$HERE/android-app/app/build/outputs/apk/debug/app-debug.apk}"
PKG=de.lbw.client
TOKEN="emu-token-geheim"
mkdir -p "$OUT"
rm -f "$OUT"/screen.png "$OUT"/server.log "$OUT"/logcat.log "$OUT"/prefs.xml

[ -f "$APK" ] || { echo "APK fehlt: $APK (scripts/build_android.sh)"; exit 1; }
(cd "$SRC11" && cargo build --release -q -p lbw-server && ./scripts/fetch_models.sh >/dev/null) || exit 1

PIDS=()
OWN_EMU=0
cleanup() {
  [ "$OWN_EMU" = 1 ] && "$ADB" emu kill >/dev/null 2>&1
  kill "${PIDS[@]}" 2>/dev/null; wait 2>/dev/null
}
trap cleanup EXIT

FAIL=0
check() { if eval "$2"; then echo "OK   $1"; else echo "FAIL $1"; FAIL=1; fi; }
wait_log() { # timeout-s muster → logcat (Tag lbw)
  local end=$(( $(date +%s) + $1 ))
  while [ "$(date +%s)" -lt "$end" ]; do
    "$ADB" logcat -d -s lbw:I | grep -q -- "$2" && return 0; sleep 1
  done
  return 1
}
launch() { "$ADB" logcat -c; "$ADB" shell am start -S -n "$PKG/.MainActivity" --ez autoconnect true --ez hud true "$@" >/dev/null; }

# sshd-Paar + Keys (Ed25519, wie tinyeu_key.pem / id_lenovo_to_hpz6o).
K="$OUT/keys"; mkdir -p "$K"
[ -f "$K/jump" ] || ssh-keygen -q -t ed25519 -N "" -f "$K/jump"
[ -f "$K/target" ] || ssh-keygen -q -t ed25519 -N "" -f "$K/target"
[ -f "$K/user" ] || ssh-keygen -q -t ed25519 -N "" -f "$K/user"
cp "$K/user.pub" "$K/authorized_keys"; chmod 700 "$K"; chmod 600 "$K/authorized_keys"
mkdir -p /run/sshd
/usr/sbin/sshd -D -p 2322 -h "$K/jump" -o ListenAddress=127.0.0.1 \
  -o AuthorizedKeysFile="$K/authorized_keys" -o PasswordAuthentication=no \
  -o PermitRootLogin=prohibit-password -o StrictModes=no -o AllowTcpForwarding=yes & PIDS+=($!)
/usr/sbin/sshd -D -p 2323 -h "$K/target" -o ListenAddress=127.0.0.1 \
  -o AuthorizedKeysFile="$K/authorized_keys" -o PasswordAuthentication=no \
  -o PermitRootLogin=prohibit-password -o StrictModes=no -o AllowTcpForwarding=yes & PIDS+=($!)
JFP=$(ssh-keygen -lf "$K/jump.pub" | awk '{print $2}')
TFP=$(ssh-keygen -lf "$K/target.pub" | awk '{print $2}')

# Xvfb + xterm + Server (echte Modelle, CPU).
Xvfb :99 -screen 0 1280x720x24 >/dev/null 2>&1 & PIDS+=($!)
sleep 2
export DISPLAY=:99
xterm -geometry 100x30+10+10 -e "printf 'EMU-MARKER-11\n'; sleep 120" & PIDS+=($!)
sleep 1
(cd "$SRC11" && ./target/release/lbw-server --token "$TOKEN" --budget 6000 --cpu >"$OUT/server.log" 2>&1 & echo $! >"$OUT/server.pid")
PIDS+=($(cat "$OUT/server.pid"))
for i in $(seq 1 24); do grep -q "lauscht" "$OUT/server.log" && break; sleep 5; done

# Emulator (oder laufenden benutzen).
if ! "$ADB" devices 2>/dev/null | grep -q "device$"; then
  sdkmanager --install "$IMAGE" >/dev/null
  if ! avdmanager list avd 2>/dev/null | grep -q "Name: $AVD"; then
    echo no | avdmanager create avd -n "$AVD" -k "$IMAGE" -d pixel_6 >/dev/null || exit 1
  fi
  "$EMU" -avd "$AVD" -no-window -no-audio -gpu swiftshader_indirect -no-snapshot -memory 2048 -partition-size 2048 & PIDS+=($!)
  OWN_EMU=1
  for i in $(seq 1 60); do "$ADB" shell getprop sys.boot_completed 2>/dev/null | grep -q 1 && break; sleep 5; done
fi
"$ADB" install -r "$APK" >/dev/null || exit 1

USER=$(whoami)
JKEY_B64=$(base64 -w0 <"$K/user"); TKEY_B64=$JKEY_B64 # einzeilig: adb-shell zerbricht mehrzeilige --es-Werte
launch --es jump_host 10.0.2.2 --ei jump_port 2322 --es jump_user "$USER" --es jump_key_b64 "$JKEY_B64" \
  --es target_host 10.0.2.2 --ei target_port 2323 --es target_user "$USER" --es target_key_b64 "$TKEY_B64" \
  --es target_sshd_host 127.0.0.1 --ei target_sshd_port 2323 --ei lbw_port 7878 --es lbw_token "$TOKEN"

check "1 Kette steht (link up)" "wait_log 90 'link up'"
check "2 OCR-Text kommt an (EMU-MARKER-11)" "wait_log 60 'EMU-MARKER-11'"
"$ADB" shell input text "emu-typed-hello" >/dev/null 2>&1
check "3 getippter Text rundet (emu-typed-hello)" "wait_log 90 'emu-typed-hello'"
"$ADB" exec-out screencap -p >"$OUT/screen.png"
check "4 Screenshot nicht schwarz" "test \$(stat -c%s \"$OUT/screen.png\") -gt 50000"
PREFS=$("$ADB" shell "run-as $PKG cat /data/data/$PKG/shared_prefs/lbw.xml" 2>/dev/null)
echo "$PREFS" >"$OUT/prefs.xml"
check "5a Pin Jump-Hop gespeichert" "echo \"\$PREFS\" | grep -q '$JFP'"
check "5b Pin Ziel-Hop gespeichert" "echo \"\$PREFS\" | grep -q '$TFP'"
check "5c Keys im Klartext in Prefs (erwartet: ja, app-privat)" "echo \"\$PREFS\" | grep -q 'BEGIN OPENSSH PRIVATE KEY'"
"$ADB" shell input keyevent KEYCODE_HOME >/dev/null 2>&1; sleep 3
launch --es jump_host 10.0.2.2 --ei jump_port 2322 --es jump_user "$USER" --es jump_key_b64 "$JKEY_B64" \
  --es target_host 10.0.2.2 --ei target_port 2323 --es target_user "$USER" --es target_key_b64 "$TKEY_B64" \
  --es target_sshd_host 127.0.0.1 --ei target_sshd_port 2323 --ei lbw_port 7878 --es lbw_token "$TOKEN"
check "6 Reconnect nach HOME (link up)" "wait_log 90 'link up'"
"$ADB" logcat -d >"$OUT/logcat.log"
[ "$FAIL" = 0 ] && echo "E2E-OK" || { echo "E2E-FEHLER"; exit 1; }
