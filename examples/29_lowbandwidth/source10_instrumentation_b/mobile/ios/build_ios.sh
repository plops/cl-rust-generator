#!/bin/bash
# build_ios.sh — Rust-Kern als XCFramework (Gerät + Simulator) und App.
# Nur auf macOS mit Xcode (+ brew install xcodegen). Ergebnis:
#   build/LbwCore.xcframework, build/LbwClient-unsigned.ipa (Gerät),
#   Simulator-Build als Kompilier-Nachweis.
# Signieren/Sideload: s. plan/20261031_01_review_b/handbuch_mobile.md.
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
export IPHONEOS_DEPLOYMENT_TARGET=16.0
cd "$ROOT"
rustup target add aarch64-apple-ios aarch64-apple-ios-sim
cargo build --release -p lbw-mobile-core --target aarch64-apple-ios --target aarch64-apple-ios-sim
cd "$HERE"
rm -rf build && mkdir -p build/headers
cp ../core/include/lbw_core.h build/headers/
xcodebuild -create-xcframework \
  -library "$ROOT/target/aarch64-apple-ios/release/liblbw_mobile_core.a" -headers build/headers \
  -library "$ROOT/target/aarch64-apple-ios-sim/release/liblbw_mobile_core.a" -headers build/headers \
  -output build/LbwCore.xcframework
xcodegen generate
# Simulator: kompiliert + linkt alles (kein Signieren nötig).
xcodebuild -project LbwClient.xcodeproj -scheme LbwClient -configuration Release \
  -sdk iphonesimulator -destination 'generic/platform=iOS Simulator' \
  -derivedDataPath build/dd-sim CODE_SIGNING_ALLOWED=NO build
# Gerät: unsigniertes .app → .ipa (zum Signieren mit eigenem Profil/AltStore).
xcodebuild -project LbwClient.xcodeproj -scheme LbwClient -configuration Release \
  -sdk iphoneos -destination 'generic/platform=iOS' \
  -derivedDataPath build/dd-dev CODE_SIGNING_ALLOWED=NO CODE_SIGNING_REQUIRED=NO build
APP="$(find build/dd-dev -name 'LbwClient.app' -type d | head -n 1)"
mkdir -p build/Payload && cp -R "$APP" build/Payload/
(cd build && zip -qr LbwClient-unsigned.ipa Payload)
ls -la build/LbwClient-unsigned.ipa
