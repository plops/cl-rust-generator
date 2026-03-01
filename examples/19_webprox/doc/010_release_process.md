# 19_webprox Release Process

This project is released from the monorepo root via:

- `.github/workflows/release-19-webprox.yml`

The workflow builds and publishes release artifacts for:

- Linux (`x86_64-unknown-linux-gnu`)
- macOS Intel (`x86_64-apple-darwin`)
- macOS Apple Silicon (`aarch64-apple-darwin`)
- Windows (`x86_64-pc-windows-msvc`)

Published artifacts include only these binaries:

- `cloud-proxy-server`
- `minimal-tui-client`

`chrome-test` is not published as a release artifact.

## Trigger Rules

Releases are triggered by pushing a tag matching:

- `webprox-v*` (example: `webprox-v0.1.0`)

For tag-triggered releases, the workflow enforces version consistency:

- tag `webprox-vX.Y.Z` must match `version = "X.Y.Z"` in:
- `examples/19_webprox/cloud-proxy-server/Cargo.toml`
- `examples/19_webprox/minimal-tui-client/Cargo.toml`

If either version does not match the tag, the workflow fails before building.

The workflow is path-gated for the `19_webprox` subproject and only proceeds with build/release when relevant files changed in the tagged commit:

- `examples/19_webprox/**`
- `.github/workflows/release-19-webprox.yml`

A manual run is also possible with `workflow_dispatch`.

## Step-by-Step

1. Prepare your changes in `examples/19_webprox`.
   - Update `examples/19_webprox/CHANGELOG.md` with release notes for the new version.
2. Commit and push to your branch/main.
3. Create and push a release tag:

```bash
git tag webprox-v0.1.0
git push origin webprox-v0.1.0
```

4. Verify in GitHub:
- Open **Actions** and find `Release 19_webprox`.
- Confirm all matrix builds pass.

5. Download artifacts:
- Open **Releases** and download archives for your target OS.

## Local Release Build (Current Host)

To produce local release binaries:

```bash
cargo build --manifest-path examples/19_webprox/Cargo.toml --workspace --release
```

Expected binary outputs are under:

- `examples/19_webprox/target/release/`

Primary binaries:

- `cloud-proxy-server`
- `minimal-tui-client`

## Notes

- The workflow file is stored at monorepo root, not inside `examples/19_webprox`.
- Tag naming is specific to this subproject: `webprox-v*`.
