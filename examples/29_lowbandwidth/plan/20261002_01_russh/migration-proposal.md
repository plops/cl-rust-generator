
would you suggest to convert the linux and android client to also use russh? discuss the pros and cons. one thing i dont like about the java ssh approach is that tit doesnt support ed25519

**Yes, migrating both Linux and Android (along with iOS) to `russh` is highly recommended.**

The issue with **JSch** is well known: the original `com.jcraft.jsch` library has been unmaintained since ~2018. It lacks support for **Ed25519**, ChaCha20-Poly1305, and modern `rsa-sha2-256/512` key exchanges [cite: 3]. Because modern OpenSSH (8.8+) disables legacy `ssh-rsa` by default [cite: 4], connecting to modern servers with standard JSch requires configuring fallback algorithms on the server or juggling patched third-party forks (like `mwiede/jsch`) [cite: 1, 4].

Standardizing on `russh` (or a similar pure-Rust SSH client) completely resolves this and provides architectural benefits across all targets.

---

### The Architecture Shift

Currently, the setup is fragmented:
* **Linux:** Has **no** SSH code at all. Users must manually run `ssh -L 7878:localhost:7878 ...` in a separate terminal.
* **Android:** Kotlin runs JSch $\to$ opens a local TCP listening socket on `127.0.0.1:localPort` $\to$ Rust connects to `127.0.0.1:localPort` via loopback.
* **iOS:** Would require yet another third-party SSH library in Swift (like `Citadel`).

With `russh` inside the shared Rust engine:

```
┌──────────────────────────────────────────────────────────────┐
│        Frontends (Linux CLI, Android Kotlin, iOS Swift)      │
│            Pure UI: pixels, touches, keys, settings          │
└──────────────────────────────┬───────────────────────────────┘
                               │
┌──────────────────────────────▼───────────────────────────────┐
│                    Shared Rust Client Core                   │
│                                                              │
│   ┌──────────────────────┐        ┌───────────────────────┐  │
│   │ LBW Protocol & Scene │ <----> │  russh Direct-TCP/IP  │  │
│   │   (Text/AV1 tiles)   │ (pipe) │  (Ed25519, TOFU, AES) │  │
│   └──────────────────────┘        └───────────┬───────────┘  │
└───────────────────────────────────────────────┼──────────────┘
                                                │ Encrypted SSH
                                                ▼
                                         Remote Server
```

---

### Pros

#### 1. Out-of-the-Box Modern Cryptography (Ed25519)
`russh` relies on modern Rust cryptography libraries (`ed25519-dalek`, `ring`, `ssh-key`) [cite: 2]. It supports Ed25519 keys, ECDSA, RSA-SHA2, and ChaCha20-Poly1305 out of the box without requiring external native dependencies like OpenSSL.

#### 2. Direct Channel Streaming (No Localhost Loopback)
With JSch, you were forced to do this:
$$\text{Rust Core} \xrightarrow{\text{local TCP}} \text{127.0.0.1:port} \xrightarrow{\text{Java JSch}} \text{SSH Socket} \to \text{Server}$$
With `russh`, you can call `channel_open_direct_tcpip()` [cite: 2]. This gives you an asynchronous I/O stream inside Rust that connects directly to `remoteHost:remotePort`.
* **No local socket binding:** You avoid random port collisions, firewall/SELinux permission quirks on Android/iOS, and socket cleanup issues (`TIME_WAIT`).
* **Lower latency & overhead:** No context-switching between Rust and Java/OS loopback sockets for every byte received.

#### 3. Single Implementation for TOFU & Key Pinning
In your Android code, you had to write custom fingerprint formatting and a custom `HostKeyRepository` in Kotlin. With `russh`, you implement:
* Host key verification / TOFU (Trust On First Use)
* Key parsing (OpenSSH/PEM, encrypted private keys)
* Reconnection & keep-alive logic
* Password / Public-key authentication

**once in Rust**, and it instantly works identically on Linux, Android, and iOS.

#### 4. Vastly Cleaner Mobile Apps
`android-app` and `ios-app` can drop all SSH-related dependencies (no JSch, no BouncyCastle, no SwiftNIO/Citadel). Their UI simply passes a connection struct to Rust:
```rust
pub struct SshConfig {
    pub host: String,
    pub port: u16,
    pub user: String,
    pub auth: SshAuth, // Password or PrivateKey
    pub pinned_fingerprint: Option<String>,
}
```

#### 5. Better Desktop UX
On Linux, users will no longer need to run an external `ssh -L` command before launching the client. They can simply run:
```bash
lbw-client --ssh user@my-server.com
```
while still retaining the option to connect to an existing local port (`--connect 127.0.0.1:7878`).

---

### Cons and Challenges

#### 1. Async Runtime (`tokio`) Dependency
Currently, `lbw-client` is written entirely using **blocking/synchronous standard library threads** (`std::thread`, `std::net::TcpStream`, `std::sync::mpsc`).
* `russh` is strictly asynchronous and requires `tokio` [cite: 2].
* **Impact:** Your network loop in `03_net.rs` will need to run within a Tokio runtime (e.g., `tokio::runtime::Builder::new_current_thread()`). While managing a small Tokio runtime in a background thread is standard practice in Rust, it does introduce async concepts (`async`/`await`) into what was previously a purely synchronous network module.

#### 2. Binary Size Overhead
Pulling in `russh`, `tokio` (io, net, rt), and cryptographic primitives (`ring` / `ed25519-dalek`) will increase binary sizes:
* On Linux: Adds $\approx 2 - 3\text{ MB}$ to the release binary.
* On Android/iOS: Adds $\approx 2\text{ MB}$ to `liblbw_core.so` and `LbwCore.xcframework`.
*(Given modern app sizes and the fact that `rav1d` is already included, this is typically negligible, but worth noting).*

#### 3. OpenSSH Edge Cases on Desktop Linux
Power users on Linux often rely on advanced `~/.ssh/config` setups:
* Hardware tokens (YubiKeys / FIDO2 `sk-ssh-ed25519@openssh.com`)
* `ssh-agent` or GPG agent forwarding
* Jump hosts (`ProxyJump`) / Bastions
* Kerberos / GSSAPI

A built-in `russh` client will not automatically support hardware security keys or complex `ProxyJump` setups unless explicitly implemented.
* **Solution:** Keep the `--connect host:port` flag in the Linux client. That way, power users can still use system `ssh -L` if they need complex agent/YubiKey routing, while everyone else gets the convenience of the built-in tunnel.

---

### Suggested Verdict & Roadmap

Migrating to `russh` is the right architectural move. The elimination of JSch and the ability to stream the LBW protocol directly over an SSH channel without an intermediate local loopback socket far outweighs the cost of adding a Tokio runtime.

**Recommended Migration Plan:**
1. **Abstract the Transport:** In `lbw_client::net`, make the connection take an abstract stream (`AsyncRead + AsyncWrite` or a standard trait) instead of a raw `TcpStream`.
2. **Implement `russh` in a new module (`ssh.rs`):** Handle SSH handshake, authentication (Password / Ed25519 / RSA), and host-key verification.
3. **Open Channel:** Use `channel_open_direct_tcpip(remote_host, remote_port)` [cite: 2] to acquire the stream and hand it directly to the LBW session loop.
4. **Clean up Android:** Delete `02_SshTunnel.kt` and the JSch dependency completely from the Gradle build. Pass the SSH credentials through JNI directly into `Engine::new_ssh(...)`.
5. **Use it for iOS:** Use the exact same interface for the iPhone client.
