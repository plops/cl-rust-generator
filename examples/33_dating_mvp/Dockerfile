# Anti-Tinder MVP — Multi-Stage-Build (Builder + schlanker Runner).
FROM rust:1.88-slim-bookworm AS builder
WORKDIR /app
RUN apt-get update && apt-get install -y --no-install-recommends pkg-config libssl-dev && rm -rf /var/lib/apt/lists/*
COPY Cargo.toml ./
COPY src ./src
COPY templates ./templates
COPY migrations ./migrations
COPY static ./static
# Ohne --locked: Cargo.lock ist per Repo-Konvention (*.lock) nicht versioniert.
RUN cargo build --release

FROM debian:bookworm-slim AS runner
RUN apt-get update && apt-get install -y --no-install-recommends ca-certificates && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY --from=builder /app/target/release/dating-mvp ./dating-mvp
COPY --from=builder /app/templates ./templates
COPY --from=builder /app/migrations ./migrations
COPY --from=builder /app/static ./static
ENV PORT=3000
EXPOSE 3000
CMD ["./dating-mvp"]
