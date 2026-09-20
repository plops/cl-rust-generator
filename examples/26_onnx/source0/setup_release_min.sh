#!/bin/sh
# setup_release_min.sh — minimaler Release-Build ohne Tests.
#
# Warum so viele Dependencies? Der Prompt schreibt pykeio/ort,
# parasyte/pixels und nashaofu/xcap vor; deren Linux-Stack ist
# fix verdrahtet und lässt sich per Feature nicht weiter stutzen:
# - xcap zieht UNBEDINGT pipewire + Wayland-Helfer (libwayshot-xcap,
#   zbus, xcb). pipewire-sys braucht System-pipewire UND libclang
#   (bindgen). Wayland-Anteil von xcap ist nicht abwählbar.
# - pixels zieht UNBEDINGT wgpu (Vulkan/GL-Backend, braucht EGL-Header).
# - Tests ziehen NICHTS extra (keine dev-dependencies); `cargo build`
#   baut Test-Targets ohnehin nie.
# Gestutzt wurde, was geht (s. Cargo.toml): image nur mit `png`
# (spart u. a. rav1e), winit nur mit `x11` (spart den Wayland-Stack
# auf unserer Seite). Rest: 367 statt 456 Crates.
#
# Gebrauch: ./setup_release_min.sh   (braucht sudo für apt)
set -eu

SYSTEM_PKGS="libegl-dev libx11-dev libxkbcommon-dev libgbm-dev libdrm-dev \
  libpipewire-0.3-dev libclang-dev pkg-config build-essential ca-certificates"

if [ "$(id -u)" -eq 0 ]; then
  SUDO=""
elif command -v sudo >/dev/null 2>&1; then
  SUDO="sudo"
else
  echo "neither root nor sudo; install manually: $SYSTEM_PKGS" >&2
  SUDO="false"
fi
if command -v apt-get >/dev/null 2>&1; then
  echo "--> installing minimal system deps (no xvfb/feh: no tests built)"
  # shellcheck disable=SC2086
  $SUDO apt-get update -qq
  # shellcheck disable=SC2086
  $SUDO env DEBIAN_FRONTEND=noninteractive apt-get install -y -qq $SYSTEM_PKGS
else
  echo "no apt-get; ensure installed: $SYSTEM_PKGS" >&2
fi

echo "--> release build (bin only, no test targets)"
cargo build --release --bin x11_yolo_viewer

echo "--> smoke"
./target/release/x11_yolo_viewer --help >/dev/null
echo "OK: target/release/x11_yolo_viewer"
