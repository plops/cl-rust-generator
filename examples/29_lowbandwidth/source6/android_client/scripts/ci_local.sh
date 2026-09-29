#!/bin/bash
# ci_local.sh — runs the steps of .github/workflows/android-lbw.yml locally
# (without the actions: checkout, caches, artifact upload).
#
# Needs: rustup targets aarch64/x86_64-linux-android, cargo-ndk, JDK 21,
# Android SDK with the pinned NDK, nasm, fonts-unifont, root for sshd.
set -euo pipefail
cd "$(dirname "$0")/../.."   # source6

echo "== fmt + clippy"
cargo fmt --all --check
cargo clippy -q -p lbw-common -p lbw-core --all-targets -- -D warnings
cargo clippy -q -p lbw-client --no-default-features --all-targets -- -D warnings

echo "== Rust host tests"
cargo test -q -p lbw-common -p lbw-core
cargo test -q -p lbw-client --no-default-features

echo "== sshd"
LBW_SSHD_PASSWORD="ci-$RANDOM-$RANDOM"
export LBW_SSHD_PASSWORD
eval "$(android_client/scripts/test_sshd.sh start)"
trap 'android_client/scripts/test_sshd.sh stop' EXIT

echo "== Android build"
android_client/scripts/build_android.sh
