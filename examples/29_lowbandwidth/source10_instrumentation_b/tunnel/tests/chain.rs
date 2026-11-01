//! Kettentest (D0): russh → Jump-sshd → genestet → Ziel-sshd → Echo.
//!
//! Braucht `/usr/sbin/sshd` + `ssh-keygen` (CI: `openssh-server`).
//! Topologie wie tinyeu → hpz6o: Der Jump erreicht den Ziel-sshd über
//! dessen lokale Adresse (hier derselbe Host, Ports 2222/2223).

mod common;

use common::{echo_server, host_fp, pinned_for, sshd, tmpdir, user};

use lbw_tunnel::{connect_chain, echo_check};

// (Hilfen liegen in common.rs; pinned_for baut PinnedHop mit User-Key.)
#[tokio::test]
async fn chain_jump_target_echo() {
    let jd = tmpdir("jump");
    let td = tmpdir("target");
    let _jump = sshd(2222, &jd);
    let _target = sshd(2223, &td);
    let echo = echo_server().await;

    let hops = vec![
        pinned_for(
            &format!("{}@127.0.0.1:2222", user()),
            &jd,
            Some(host_fp(&jd)),
        ),
        pinned_for(
            &format!("{}@127.0.0.1:2223", user()),
            &td,
            Some(host_fp(&td)),
        ),
    ];
    let mut chain = connect_chain(&hops, ("127.0.0.1", 2223), echo)
        .await
        .expect("Kette steht");
    let back = echo_check(&mut chain.stream, b"ping-2332")
        .await
        .expect("Echo");
    assert_eq!(back, b"ping-2332");
}

#[tokio::test]
async fn chain_rejects_wrong_hostkey_before_auth() {
    let jd = tmpdir("jump2");
    let _jump = sshd(2224, &jd);
    let hops = vec![pinned_for(
        &format!("{}@127.0.0.1:2224", user()),
        &jd,
        Some("SHA256:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA".into()),
    )];
    let r = connect_chain(&hops, ("127.0.0.1", 2224), 7878).await;
    assert!(r.is_err(), "falscher Hostkey muss abbrechen");
}

#[tokio::test]
async fn chain_rejects_wrong_key() {
    let jd = tmpdir("jump3");
    let td = tmpdir("target3");
    let _jump = sshd(2225, &jd);
    let _target = sshd(2226, &td);
    // Jump-Key am Ziel: Auth dort muss scheitern.
    let hops = vec![
        pinned_for(
            &format!("{}@127.0.0.1:2225", user()),
            &jd,
            Some(host_fp(&jd)),
        ),
        lbw_tunnel::PinnedHop {
            hop: lbw_tunnel::parse_hop(&format!("{}@127.0.0.1:2226", user())).unwrap(),
            identity_openssh: common::user_key(&jd),
            passphrase: None,
            pin: Some(host_fp(&td)),
        },
    ];
    let r = connect_chain(&hops, ("127.0.0.1", 2226), 7878).await;
    assert!(r.is_err(), "falscher Key muss scheitern");
}
