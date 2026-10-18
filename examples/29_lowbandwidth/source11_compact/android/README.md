# Android rust-core (A0)

`lbw-android-core` (`cdylib`) beweist: `lbw-tunnel` (russh + Tokio) + JNI
linkt für `aarch64-linux-android` und `x86_64-linux-android` (minSdk 26),
JNI-Symbol `Java_de_lbw_client_CoreBridge_version` exportiert.

Bauen (NDK r27d, ohne cargo-ndk — 4.1.2 kennt das NDK-Layout ohne
`platforms/` nicht, `unknown package: 26`):

```sh
TC=$ANDROID_NDK_HOME/toolchains/llvm/prebuilt/linux-x86_64/bin
export CC_aarch64_linux_android=$TC/aarch64-linux-android26-clang \
  CXX_aarch64_linux_android=$TC/aarch64-linux-android26-clang++ \
  AR_aarch64_linux_android=$TC/llvm-ar \
  CC_x86_64_linux_android=$TC/x86_64-linux-android26-clang \
  CXX_x86_64_linux_android=$TC/x86_64-linux-android26-clang++ \
  AR_x86_64_linux_android=$TC/llvm-ar \
  CARGO_TARGET_AARCH64_LINUX_ANDROID_LINKER=$TC/aarch64-linux-android26-clang \
  CARGO_TARGET_X86_64_LINUX_ANDROID_LINKER=$TC/x86_64-linux-android26-clang
cargo build --target aarch64-linux-android --target x86_64-linux-android
```

Debug-`.so` ≈ 53 MB (unoptimiert, mit Debug-Info — keine Aussage über die
Release-Größe; Messung in A1/A2). Echte APK + Emulator-E2E folgen in A1/A2
(brauchen JDK + SDK, hier nicht installiert).
