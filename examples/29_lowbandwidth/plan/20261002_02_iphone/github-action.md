**Yes, absolutely.** GitHub Actions is actually one of the standard ways to build and distribute iOS apps because GitHub provides hosted **macOS runners** (`macos-14`, `macos-15`, running on Apple Silicon) with Xcode, `rustup`, and Apple build tools pre-installed.

Depending on your distribution target, you can automate everything from compiling the Rust core to producing an `.ipa` release file or pushing directly to **TestFlight**.

---

### The Two Common Distribution Paths

| Goal | Requirements | CI Output |
| :--- | :--- | :--- |
| **A. Sideloading / Ad-Hoc / GitHub Releases** | Free Apple ID (or none at all for unsigned) | An unsigned or ad-hoc signed `.ipa` uploaded to your GitHub Releases (users install via AltStore, SideStore, or TrollStore). |
| **B. TestFlight / App Store** | Paid Apple Developer Account ($99/year) | An `.ipa` automatically uploaded to App Store Connect / TestFlight. |

---

### The CI Pipeline Flow

A typical GitHub Actions workflow consists of three stages:

1. **Build the Rust Core as an `.xcframework`:**
   * Target `aarch64-apple-ios` (real devices) and optionally `aarch64-apple-ios-sim` (simulator).
   * Bundle into `LbwCore.xcframework` using `xcodebuild -create-xcframework`.
2. **Setup Certificates & Provisioning (for signed builds):**
   * Import your Apple Distribution Certificate (`.p12`) and Provisioning Profile (`.mobileprovision`) into a temporary macOS keychain on the runner.
3. **Build & Archive Xcode Project:**
   * Run `xcodebuild -archivePath ...` and `xcodebuild -exportArchive`.
   * Export the `.ipa` and upload it to GitHub Release assets (or TestFlight via `xcrun altool` / Fastlane).

---

### Complete GitHub Actions Workflow Example

Here is a working blueprint (`.github/workflows/build-ios.yml`) demonstrating how to build the Rust core and package the iOS app:

```yaml
name: Build & Release iOS Client

on:
  push:
    tags:
      - 'v*'
  workflow_dispatch:

jobs:
  build-ios:
    runs-on: macos-14 # Apple Silicon (M1/M2) runner

    steps:
      - name: Checkout repository
        uses: actions/checkout@v4

      - name: Install Rust toolchain
        uses: dtolnay/rust-toolchain@stable
        with:
          targets: aarch64-apple-ios, aarch64-apple-ios-sim

      - name: Cache Cargo Dependencies
        uses: actions/cache@v4
        with:
          path: |
            ~/.cargo/bin/
            ~/.cargo/registry/index/
            ~/.cargo/registry/cache/
            ~/.cargo/git/db/
            target/
          key: ${{ runner.os }}-cargo-${{ hashFiles('**/Cargo.lock') }}

      # ----------------------------------------------------
      # 1. Compile Rust Core & generate XCFramework
      # ----------------------------------------------------
      - name: Build Rust Static Library
        run: |
          cargo build --release --target aarch64-apple-ios -p lbw-ios-core
          
          # Bundle into XCFramework with C headers
          rm -rf build/LbwCore.xcframework
          xcodebuild -create-xcframework \
            -library target/aarch64-apple-ios/release/liblbw_core.a \
            -headers ios_client/include \
            -output ios_client/Frameworks/LbwCore.xcframework

      # ----------------------------------------------------
      # 2. Setup Code Signing (If using paid Developer Account)
      # ----------------------------------------------------
      - name: Install Apple Certificate and Provisioning Profile
        if: env.BUILD_SIGNED == 'true'
        env:
          BUILD_CERTIFICATE_BASE64: ${{ secrets.APPLE_CERTIFICATE_P12_BASE64 }}
          P12_PASSWORD: ${{ secrets.APPLE_CERTIFICATE_PASSWORD }}
          PROVISION_PROFILE_BASE64: ${{ secrets.APPLE_PROVISION_PROFILE_BASE64 }}
          KEYCHAIN_PASSWORD: ${{ secrets.KEYCHAIN_PASSWORD }}
        run: |
          # Create temporary keychain
          KEYCHAIN_PATH=$RUNNER_TEMP/build.keychain
          security create-keychain -p "$KEYCHAIN_PASSWORD" $KEYCHAIN_PATH
          security set-keychain-settings -lut 21600 $KEYCHAIN_PATH
          security unlock-keychain -p "$KEYCHAIN_PASSWORD" $KEYCHAIN_PATH

          # Import .p12 certificate
          CERT_PATH=$RUNNER_TEMP/cert.p12
          echo "$BUILD_CERTIFICATE_BASE64" | base64 --decode > $CERT_PATH
          security import $CERT_PATH -k $KEYCHAIN_PATH -P "$P12_PASSWORD" -T /usr/bin/codesign
          security set-key-partition-list -S apple-tool:,apple: -s -k "$KEYCHAIN_PASSWORD" $KEYCHAIN_PATH

          # Place provisioning profile
          mkdir -p ~/Library/MobileDevice/Provisioning\ Profiles
          echo "$PROVISION_PROFILE_BASE64" | base64 --decode > ~/Library/MobileDevice/Provisioning\ Profiles/profile.mobileprovision

      # ----------------------------------------------------
      # 3. Build & Archive Xcode project
      # ----------------------------------------------------
      - name: Build Archive
        run: |
          xcodebuild -project ios_client/lbw-client.xcodeproj \
            -scheme lbw-client \
            -configuration Release \
            -destination 'generic/platform=iOS' \
            -archivePath build/lbw-client.xcarchive \
            archive \
            CODE_SIGNING_ALLOWED=NO # Set to YES if signing credentials above are active

      # ----------------------------------------------------
      # 4. Package to .IPA
      # ----------------------------------------------------
      - name: Package Unsigned .ipa (for Sideloading)
        run: |
          mkdir -p build/Payload
          cp -r build/lbw-client.xcarchive/Products/Applications/lbw-client.app build/Payload/
          cd build && zip -r lbw-client-unsigned.ipa Payload/

      - name: Upload IPA to Release
        uses: softprops/action-gh-release@v2
        with:
          files: build/lbw-client-unsigned.ipa
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

---

### Important Things to Keep in Mind

1. **Free vs. Private Repositories:**
   * GitHub Actions provides **free macOS runner minutes for public repositories**.
   * For private repositories, macOS runner minutes cost **10× the standard Linux rate** (e.g., 10 build minutes consume 100 billing minutes). Keep compilation times lean by caching `~/.cargo` and `target/`.
2. **Use Fastlane for TestFlight (Optional but recommended):**
   * If you target TestFlight, using **[Fastlane](https://fastlane.tools/)** (`fastlane match` for certificates and `fastlane pilot` for upload) with an App Store Connect API Key simplifies the certificate management in CI considerably.
3. **No Mac Required Locally:**
   * You do not need a physical Mac at home to build the app; the entire pipeline, including compiling the Swift/UIKit code and `rav1d`, runs entirely on GitHub's hosted macOS machines.
