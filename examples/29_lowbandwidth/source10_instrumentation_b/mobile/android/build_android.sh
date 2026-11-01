#!/bin/bash
# build_android.sh — Rust-Kern (arm64-v8a + x86_64) + APK + JVM-Tests.
# Braucht ANDROID_HOME (SDK 36 + NDK 27.0.12077973) und JDK 21.
# Aufruf (aus source10_instrumentation_b/): mobile/android/build_android.sh [release]
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
export ANDROID_HOME="${ANDROID_HOME:-/opt/android-sdk}"
export JAVA_HOME="${JAVA_HOME:-/usr/lib/jvm/java-21-openjdk-amd64}"
export PATH="$JAVA_HOME/bin:$PATH"
NDK="${ANDROID_NDK_HOME:-$ANDROID_HOME/ndk/27.0.12077973}"
TC="$NDK/toolchains/llvm/prebuilt/linux-x86_64/bin"
[ -d "$TC" ] || { echo "NDK fehlt: $TC" >&2; exit 2; }
# Direkt per NDK-Clang (cargo-ndk 4.1.2 kennt das NDK-27-Layout nicht —
# Befund aus source11/android/README.md).
export CC_aarch64_linux_android=$TC/aarch64-linux-android26-clang \
  CXX_aarch64_linux_android=$TC/aarch64-linux-android26-clang++ \
  AR_aarch64_linux_android=$TC/llvm-ar \
  CC_x86_64_linux_android=$TC/x86_64-linux-android26-clang \
  CXX_x86_64_linux_android=$TC/x86_64-linux-android26-clang++ \
  AR_x86_64_linux_android=$TC/llvm-ar \
  CARGO_TARGET_AARCH64_LINUX_ANDROID_LINKER=$TC/aarch64-linux-android26-clang \
  CARGO_TARGET_X86_64_LINUX_ANDROID_LINKER=$TC/x86_64-linux-android26-clang

cd "$ROOT"
cargo build --release -p lbw-mobile-core --target aarch64-linux-android --target x86_64-linux-android
OUT="$HERE/app/src/main/jniLibs"
mkdir -p "$OUT/arm64-v8a" "$OUT/x86_64"
for t in aarch64-linux-android:arm64-v8a x86_64-linux-android:x86_64; do
  "$TC/llvm-strip" -o "$OUT/${t#*:}/liblbw_mobile_core.so" "target/${t%%:*}/release/liblbw_mobile_core.so"
done
ls -la "$OUT"/*/liblbw_mobile_core.so

# Unifont (GNU, CJK-Abdeckung) als Asset, falls installiert (apt: fonts-unifont);
# sonst fällt die App auf Monospace zurück.
FONT=/usr/share/fonts/opentype/unifont/unifont.otf
if [ -f "$FONT" ] && [ ! -f "$HERE/app/src/main/assets/fonts/unifont.otf" ]; then
  mkdir -p "$HERE/app/src/main/assets/fonts" && cp "$FONT" "$HERE/app/src/main/assets/fonts/"
fi

# JVM-Tests laden die Host-Variante des Kerns (target/debug).
cargo build -p lbw-mobile-core
cd "$HERE"
TASK=assembleDebug
[ "${1:-}" = release ] && TASK=assembleRelease
./gradlew --no-daemon -q ":app:$TASK" ":app:testDebugUnitTest"
find app/build/outputs/apk -name '*.apk' -exec ls -la {} \;
