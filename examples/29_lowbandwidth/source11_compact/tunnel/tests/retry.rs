//! Reconnect-Test (T0): Kette steht auch bei spät startendem sshd und
//! nach sshd-Neustart — mit kurzen Retry-Abständen (kein Minuten-Warten).
//!
//! Braucht `/usr/sbin/sshd` + `ssh-keygen`.

mod common;

use std::time::Duration;

use common::{echo_server, host_fp, pinned_for, sshd, tmpdir, user};

use lbw_tunnel::{TunnelPolicy, connect_chain_retry, echo_check};

fn quick_policy() -> TunnelPolicy {
    TunnelPolicy {
        connect_timeout: Duration::from_secs(5),
        keepalive: Duration::from_secs(15),
        keepalive_max: 3,
        retry_delays: vec![Duration::from_millis(300), Duration::from_millis(300)],
    }
}

#[tokio::test]
async fn retry_connects_when_sshd_starts_late() {
    let jd = tmpdir("late");
    // Keys deterministisch vorab (kein paralleles Keygen → kein Prompt-Race).
    common::sh(&format!(
        "[ -f {d}/host ] || ssh-keygen -q -t ed25519 -N '' -f {d}/host",
        d = jd.display()
    ));
    common::sh(&format!(
        "[ -f {d}/user ] || ssh-keygen -q -t ed25519 -N '' -f {d}/user",
        d = jd.display()
    ));
    std::fs::copy(jd.join("user.pub"), jd.join("authorized_keys")).unwrap();

    let echo = echo_server().await;
    let hops = vec![pinned_for(
        &format!("{}@127.0.0.1:2231", user()),
        &jd,
        Some(host_fp(&jd)),
    )];
    // sshd fehlt noch: Retry scheitert sauber (kein Hängen).
    let r = connect_chain_retry(&quick_policy(), &hops, ("127.0.0.1", 2231), echo).await;
    assert!(r.is_err(), "ohne sshd kein Connect");
    // sshd startet spät — derselbe Aufruf verbindet (Reconnect-Pfad).
    let _up = sshd(2231, &jd);
    let mut chain = connect_chain_retry(&quick_policy(), &hops, ("127.0.0.1", 2231), echo)
        .await
        .expect("Retry verbindet nach sshd-Start");
    let back = echo_check(&mut chain.stream, b"spaet").await.expect("Echo");
    assert_eq!(back, b"spaet");
}

#[tokio::test]
async fn retry_reconnects_after_sshd_restart() {
    let jd = tmpdir("restart");
    let echo = echo_server().await;
    let hops = vec![pinned_for(
        &format!("{}@127.0.0.1:2232", user()),
        &jd,
        Some({
            common::sh(&format!(
                "[ -f {d}/host ] || ssh-keygen -q -t ed25519 -N '' -f {d}/host",
                d = jd.display()
            ));
            common::sh(&format!(
                "[ -f {d}/user ] || ssh-keygen -q -t ed25519 -N '' -f {d}/user",
                d = jd.display()
            ));
            std::fs::copy(jd.join("user.pub"), jd.join("authorized_keys")).unwrap();
            host_fp(&jd)
        }),
    )];

    let policy = quick_policy();
    // Ohne sshd scheitern alle Versuche (Fehler, kein Hängen).
    let r = connect_chain_retry(&policy, &hops, ("127.0.0.1", 2232), echo).await;
    assert!(r.is_err(), "ohne sshd kein Connect");
    // Nach sshd-Start verbindet derselbe Aufruf (Reconnect-Pfad).
    let _up = sshd(2232, &jd);
    let mut chain = connect_chain_retry(&policy, &hops, ("127.0.0.1", 2232), echo)
        .await
        .expect("Reconnect nach sshd-Start");
    let back = echo_check(&mut chain.stream, b"neu").await.expect("Echo");
    assert_eq!(back, b"neu");
}
