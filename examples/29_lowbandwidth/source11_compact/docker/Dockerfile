# Harness-Image für D1 (Ubuntu + Toolchain + Xvfb + sshd + Wireshark).
# Baut NICHT die Modelle ein (kommen per fetch_models.sh, 52 MB).
FROM ubuntu:24.04
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update -q && apt-get install -y -q \
    build-essential cmake pkg-config nasm \
    curl ca-certificates \
    xvfb xterm x11-utils xdotool \
    openssh-server openssh-client \
    tshark libwireshark-dev \
    iproute2 \
    libxcb1-dev libxcb-shm0-dev libxcb-randr0-dev \
 && rm -rf /var/lib/apt/lists/*
COPY rust-toolchain.toml /opt/lbw/
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
    | sh -s -- -y --profile minimal --default-toolchain stable \
 && /root/.cargo/bin/rustup target add aarch64-linux-android x86_64-linux-android
ENV PATH="/root/.cargo/bin:${PATH}"
WORKDIR /src/source11_compact
CMD ["./scripts/smoke_e2e.sh"]
