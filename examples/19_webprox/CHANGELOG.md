# Changelog

All notable changes to `examples/19_webprox` are documented in this file.

## [0.1.4] - 2026-02-28

### Added
- `cloud-proxy-server`: new `--chrome-binary <PATH>` option to explicitly select the Chrome/Chromium executable.

### Changed
- `cloud-proxy-server`: browser launch path is now configurable to avoid distro-specific wrapper issues.

## [0.1.3] - 2026-02-28

### Added
- `--version` support for `cloud-proxy-server` and `minimal-tui-client`.
- CI: release workflow installs `protobuf-compiler` on Linux runners.

### Changed
- Release workflow validates `webprox-vX.Y.Z` tag against crate versions.
- Release packaging publishes only `cloud-proxy-server` and `minimal-tui-client`.

## [0.1.2] - 2026-02-28

### Changed
- CI: temporary Linux-only release matrix for faster iteration.
- Repo: root `.gitignore` now ignores `*.tar` and `*.tar.zst`.

## [0.1.1] - 2026-02-28

### Added
- Monorepo release workflow for `examples/19_webprox` at `.github/workflows/release-19-webprox.yml`.
- Release process documentation in `examples/19_webprox/doc/release_process.md`.
