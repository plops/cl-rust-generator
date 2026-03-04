# Webprox AVIF Release Procedure

This document outlines the steps required to create a new release of `20_webprox_avif`.

## Prerequisites
- Ensure all tests pass.
- Ensure your working branch is up to date with the main branch and you are ready to make a release.
- Ensure you have cleanly committed your changes or stashed uncommitted files.

## 1. Run the Bump Version Script

We use an automated script to handle bumping versions across both the server and client crates, and to safely tag the git release. The CI/CD GitHub action is heavily dependent on the tag name (`webprox-avif-v<version>`) and validation checks.

From anywhere in the project, execute:
```bash
./scripts/bump_version_and_tag.sh
```

You will be prompted to enter the new version number in the format `x.y.z` (e.g., `0.2.0`).

The script will automatically:
1. Validating that your working directory is clean.
2. Update the `version` field in `cloud-render-srv/Cargo.toml`.
3. Update the `version` field in `macroquad-client/Cargo.toml`.
4. Stage and commit these two files.
5. Create a git tag corresponding to the new version `webprox-avif-v<version>`.

## 2. Push to GitHub

Push your current branch changes, then push the tag. The tag is what triggers the GitHub Actions workflow to build and create the release.

```bash
git push origin HEAD
git push origin <TAG_NAME>
```

*(The `bump_version_and_tag.sh` script will output the exact commands for you to copy and paste.)*

## 3. GitHub Actions

Once the tag is pushed, the `Release 20_webprox_avif` GitHub Action will execute:
- It detects changes.
- Validates the structural integrity of the `Cargo.toml` versions compared to the tag.
- Compiles the x86 Linux Server and Client releases.
- Attaches the produced binaries as assets to a new GitHub Release corresponding to the pushed tag.

## 4. Considerations and Notes
- Currently, macOS and Windows release builds are temporarily **disabled** in the CI/CD pipeline (`.github/workflows/release-20-webprox-avif.yml`). To enable them later, simply uncomment the relevant blocks in the `matrix` strategy section of the YAML workflow.
- You can print the version of the CLI binaries locally using `cargo run --bin <crate> -- --version`.
