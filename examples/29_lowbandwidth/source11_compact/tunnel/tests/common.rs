//! Gemeinsame Test-Hilfen: Wegwerf-sshd + Echo-Server (127.0.0.1).

use std::process::{Child, Command};

use tokio::io::{AsyncReadExt, AsyncWriteExt};

pub struct Sshd {
    child: Child,
}

impl Drop for Sshd {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

pub fn sh(cmd: &str) {
    // stdin zu, damit kein Tool je auf einen Prompt blockieren kann.
    let ok = Command::new("sh")
        .arg("-c")
        .arg(cmd)
        .stdin(std::process::Stdio::null())
        .status()
        .unwrap()
        .success();
    assert!(ok, "{cmd}");
}

pub fn sshd(port: u16, dir: &std::path::Path) -> Sshd {
    sh(&format!(
        "[ -f {d}/host ] || ssh-keygen -q -t ed25519 -N '' -f {d}/host",
        d = dir.display()
    ));
    sh(&format!(
        "[ -f {d}/user ] || ssh-keygen -q -t ed25519 -N '' -f {d}/user",
        d = dir.display()
    ));
    std::fs::copy(dir.join("user.pub"), dir.join("authorized_keys")).unwrap();
    let mut cmd = Command::new("/usr/sbin/sshd");
    cmd.args([
        "-D",
        "-p",
        &port.to_string(),
        "-h",
        &dir.join("host").to_string_lossy(),
        "-o",
        "ListenAddress=127.0.0.1",
        "-o",
        &format!(
            "AuthorizedKeysFile={}",
            dir.join("authorized_keys").display()
        ),
        "-o",
        "PasswordAuthentication=no",
        "-o",
        "PermitRootLogin=prohibit-password",
        "-o",
        "StrictModes=no",
        "-o",
        "AllowTcpForwarding=yes",
    ]);
    let child = cmd.spawn().expect("sshd startet");
    std::thread::sleep(std::time::Duration::from_millis(400));
    Sshd { child }
}

pub fn host_fp(dir: &std::path::Path) -> String {
    let out = Command::new("ssh-keygen")
        .args(["-lf", &dir.join("host.pub").to_string_lossy()])
        .output()
        .unwrap();
    let line = String::from_utf8(out.stdout).unwrap();
    line.split_whitespace().nth(1).unwrap().to_string()
}

pub fn user_key(dir: &std::path::Path) -> String {
    std::fs::read_to_string(dir.join("user")).unwrap()
}

pub fn user() -> String {
    std::env::var("USER").unwrap_or_else(|_| "root".into())
}

pub fn pinned_for(hop: &str, dir: &std::path::Path, pin: Option<String>) -> lbw_tunnel::PinnedHop {
    lbw_tunnel::PinnedHop {
        hop: lbw_tunnel::parse_hop(hop).unwrap(),
        identity_openssh: user_key(dir),
        passphrase: None,
        pin,
    }
}

pub fn tmpdir(name: &str) -> std::path::PathBuf {
    let d = std::env::temp_dir().join(format!("lbw11-{name}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&d);
    std::fs::create_dir_all(&d).unwrap();
    d
}

pub async fn echo_server() -> u16 {
    let l = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let port = l.local_addr().unwrap().port();
    tokio::spawn(async move {
        loop {
            let (mut s, _) = l.accept().await.unwrap();
            tokio::spawn(async move {
                let mut b = [0u8; 4096];
                loop {
                    let n = s.read(&mut b).await.unwrap_or(0);
                    if n == 0 {
                        return;
                    }
                    s.write_all(&b[..n]).await.unwrap();
                }
            });
        }
    });
    port
}
