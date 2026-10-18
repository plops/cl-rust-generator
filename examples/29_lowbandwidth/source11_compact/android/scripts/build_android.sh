#!/bin/bash
# build_android.sh — .so (beide ABIs) + Debug-APK für source11_compact.
# Braucht ANDROID_HOME (SDK + NDK 27) und JAVA_HOME (JDK 21).
# Aufruf (aus source11_compact/android): ./scripts/build_android.sh
set -euo pipefail
cd "$(dirname "$0")/.."
: "${ANDROID_HOME:?ANDROID_HOME fehlt}"
: "${JAVA_HOME:?JAVA_HOME fehlt}"
RUST_CORE="$PWD/rust-core"
APP="$PWD/android-app"
OUT="$APP/app/src/main/jniLibs"

TC="$ANDROID_HOME/ndk/27.0.12077973/toolchains/llvm/prebuilt/linux-x86_64/bin"
export CC_aarch64_linux_android=$TC/aarch64-linux-android26-clang \
  CXX_aarch64_linux_android=$TC/aarch64-linux-android26-clang++ \
  AR_aarch64_linux_android=$TC/llvm-ar \
  CC_x86_64_linux_android=$TC/x86_64-linux-android26-clang \
  CXX_x86_64_linux_android=$TC/x86_64-linux-android26-clang++ \
  AR_x86_64_linux_android=$TC/llvm-ar \
  CARGO_TARGET_AARCH64_LINUX_ANDROID_LINKER=$TC/aarch64-linux-android26-clang \
  CARGO_TARGET_X86_64_LINUX_ANDROID_LINKER=$TC/x86_64-linux-android26-clang

cargo build --release --manifest-path "$RUST_CORE/Cargo.toml" \
  --target aarch64-linux-android --target x86_64-linux-android
"$TC/llvm-strip" \
  ../target/aarch64-linux-android/release/liblbw_android_core.so \
  ../target/x86_64-linux-android/release/liblbw_android_core.so
mkdir -p "$OUT/arm64-v8a" "$OUT/x86_64"
cp ../target/aarch64-linux-android/release/liblbw_android_core.so "$OUT/arm64-v8a/"
cp ../target/x86_64-linux-android/release/liblbw_android_core.so "$OUT/x86_64/"
ls -la "$OUT/arm64-v8a" "$OUT/x86_64"

export PATH="$JAVA_HOME/bin:$PATH"
if command -v gradle >/dev/null; then
  (cd "$APP" && gradle :app:assembleDebug)
else
  echo "HINWEIS: kein gradle im PATH — nur .so gebaut, APK übersprungen"
fi
