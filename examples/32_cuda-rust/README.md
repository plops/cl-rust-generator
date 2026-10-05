# CUDA-Rust examples: GUI from Docker on the host display

Tested with `sar_tdbp` on an NVIDIA RTX A4000 host running Xorg (`:0`).
App options are in [sar_tdbp/README.md](./sar_tdbp/README.md);
required image content is listed in
[walkthrough §4](./plan/20261004_01_sar_tdbp/walkthrough.md).

Source for the cuda-oxide setup:
https://github.com/NVIDIA/cuda-rust/blob/main/cuda-oxide/cuda-oxide-book/getting-started/hello-gpu.md

## 1. Host: allow container windows and launch

```bash
xhost +local:docker

docker run -it --rm \
  --gpus all \
  --ipc=host \
  -e DISPLAY="$DISPLAY" \
  -v /tmp/.X11-unix:/tmp/.X11-unix:ro \
  -v "$PWD":/workspace \
  <image>
```

What each flag is for:

- `--gpus all`: CUDA + `/dev/nvidia*` inside the container.
- `--ipc=host`: shares `/dev/shm` so MIT-SHM can work between
  container and host X server.
- `-e DISPLAY` + X11 socket mount: the display transport.
  Unix socket, not TCP — host Xorg normally has TCP disabled
  (`-nolisten tcp`), so `DISPLAY=<host-ip>:0` is refused.
- Add `-e NVIDIA_DRIVER_CAPABILITIES=all` on relaunch if you want
  direct NVIDIA GLX instead of the Mesa fallback (see table below).

## 2. Container: one-time setup and display check

```bash
apt-get update
apt-get install -y libclang-dev libxi6 libxkbcommon0 libxkbcommon-x11-0 libgl1
# optional: mesa-utils (for glxinfo), xvfb (only for `cargo oxide test`, not for host display)

cargo +nightly-2026-08-28 install --git https://github.com/NVlabs/cuda-oxide.git cargo-oxide
cargo oxide doctor   # fix anything it reports before building

xterm &              # plain X11 check: a window must pop up on the host
glxinfo -B           # GLX check (needs mesa-utils)
```

## 3. Container: build and run the GUI

The toolchain in this image lives in `/root/.cargo`, so build as root,
then run the GUI **as the X-session user** (uid 1000 here):

```bash
cd /workspace/src/cl-rust-generator/examples/32_cuda-rust/sar_tdbp
cargo oxide build

su -s /bin/bash ubuntu -c 'DISPLAY=:0 ./target/release/sar_tdbp --phantom rust'
# Left/Right: aperture, R: full aperture, click: profiles, Esc: quit
```

Headless alternatives (any user, no display needed):

```bash
./target/release/sar_tdbp --headless /tmp/bild.png --phantom rust
./target/release/sar_tdbp --bench
```

## 4. If it fails

| Symptom | Cause | Fix |
|---|---|---|
| `miniquad ... linux_x11.rs ... unwrap() on None` | no `libGL` in container | step 2 `apt-get install` line |
| `BadShmSeg ... MIT-SHM ... X_ShmPutImage` | app UID ≠ X server UID | run as the X-session user (step 3 `su ...`), not root |
| `failed to create dri3 screen`, `failed to load driver: nvidia-drm` | container has compute-only NVIDIA caps | non-fatal: Mesa software GL takes over; add `NVIDIA_DRIVER_CAPABILITIES=all` on relaunch for direct GLX |
| `cannot open display` | DISPLAY / socket / xhost missing | recheck all three step-1 items on the host |
