#!/usr/bin/env bash
set -euo pipefail

# Navigate to the 20_webprox_avif directory
cd "$(dirname "$0")/.."

# Check if git tree is dirty
if [[ -n $(git status -s) ]]; then
  echo "Error: Working directory is not clean. Commit or stash changes before releasing."
  exit 1
fi

echo "Current server version: $(grep '^version =' cloud-render-srv/Cargo.toml | cut -d '"' -f 2)"
echo "Current client version: $(grep '^version =' macroquad-client/Cargo.toml | cut -d '"' -f 2)"
echo ""

read -p "Enter new version (e.g. 0.2.0): " NEW_VERSION

if [[ ! "$NEW_VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "Error: Invalid version format. Must be x.y.z"
  exit 1
fi

# Use perl or sed to safely replace the version in Cargo.toml
if [[ "$OSTYPE" == "darwin"* ]]; then
  sed -i '' "s/^version = \".*\"/version = \"$NEW_VERSION\"/" cloud-render-srv/Cargo.toml
  sed -i '' "s/^version = \".*\"/version = \"$NEW_VERSION\"/" macroquad-client/Cargo.toml
else
  sed -i "s/^version = \".*\"/version = \"$NEW_VERSION\"/" cloud-render-srv/Cargo.toml
  sed -i "s/^version = \".*\"/version = \"$NEW_VERSION\"/" macroquad-client/Cargo.toml
fi

echo "Updated Cargo.toml files."

git add cloud-render-srv/Cargo.toml macroquad-client/Cargo.toml

git commit -m "chore(release): bump 20_webprox_avif to $NEW_VERSION"
echo "Committed version bump."

TAG_NAME="webprox-avif-v$NEW_VERSION"
git tag "$TAG_NAME"
echo "Successfully bumped version and created tag $TAG_NAME"
echo ""
echo "To push the release to GitHub and trigger the CI/CD pipeline, run:"
echo "git push origin HEAD"
echo "git push origin $TAG_NAME"
