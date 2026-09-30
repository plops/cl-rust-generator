#!/bin/bash
# build_android.sh — complete Android build, same steps as the GitHub Action.
#
#   1. Unifont → assets            (fetch_font.sh)
#   2. liblbw_core.so per ABI       (cargo ndk → app/src/main/jniLibs)
#   3. host liblbw_core.so          (cargo build -p lbw-core, for JVM JNI tests)
#   4. JVM unit tests + APK         (gradlew testDebugUnitTest lintDebug assembleDebug;
#                                    release: GRADLE_TASKS="testDebugUnitTest assembleRelease")
#   5. APK check: both .so and the font are packaged (every built APK)
#
# Call (from anywhere): android_client/scripts/build_android.sh
# Env: ANDROID_HOME (default /opt/android-sdk), LBW_NDK_HOME (default: pinned NDK),
#      ABIS="arm64-v8a x86_64", PROFILE=release|dev, GRADLE_TASKS.
set -eu
cd "$(dirname "$0")/.."
export ANDROID_HOME="${ANDROID_HOME:-/opt/android-sdk}"
NDK_VERSION=30.0.16248370
# Pinned NDK; runners export ANDROID_NDK_HOME for their own default NDK.
export ANDROID_NDK_HOME="${LBW_NDK_HOME:-$ANDROID_HOME/ndk/$NDK_VERSION}"
[ -d "$ANDROID_NDK_HOME" ] || { echo "NDK $NDK_VERSION fehlt: sdkmanager \"ndk;$NDK_VERSION\"" >&2; exit 1; }
ABIS="${ABIS:-arm64-v8a x86_64}"
PROFILE="${PROFILE:-release}"
GRADLE_TASKS="${GRADLE_TASKS:-testDebugUnitTest lintDebug assembleDebug}"
APP=android-app

./scripts/fetch_font.sh

targets=()
for a in $ABIS; do targets+=(-t "$a"); done
prof=(--release)
[ "$PROFILE" = release ] || prof=(--profile "$PROFILE")
(cd .. && cargo ndk "${targets[@]}" -P 26 -o "android_client/$APP/app/src/main/jniLibs" \
    build "${prof[@]}" -p lbw-core)
(cd .. && cargo build -q -p lbw-core)

(cd "$APP" && ./gradlew --console=plain $GRADLE_TASKS)

for APK in "$APP"/app/build/outputs/apk/*/app-*.apk; do
    [ -f "$APK" ] || continue
    list=$(unzip -l "$APK")
    for a in $ABIS; do
        grep -q "lib/$a/liblbw_core.so" <<<"$list" || { echo "APK: lib/$a/liblbw_core.so fehlt" >&2; exit 1; }
    done
    grep -q "assets/fonts/unifont.otf" <<<"$list" || echo "APK: ohne Unifont (Fallback MONOSPACE)" >&2
    echo "APK ok: $APK ($(stat -c %s "$APK") B)"
done
