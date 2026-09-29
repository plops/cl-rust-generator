//! `14_session` — TCP-Verbindungen: Handshake, Resume, Heartbeat.
//!
//! Immer genau ein aktiver Client; eine neue Verbindung löst die alte ab
//! (typisch nach Tunnel-Abriss). Pro Verbindung ein Reader- (dieser Thread)
//! und ein Writer-Thread, die über `Shared` und den `Outbox` kommunizieren.
//! Kein TCP-Keepalive: hinter `ssh -L` endet der Socket auf localhost,
//! Ausfälle der WAN-Strecke sieht nur ein App-Heartbeat (Ping alle 2 s).

use std::net::{Shutdown, TcpListener, TcpStream};
use std::sync::mpsc::Sender;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use lbw_common::frame::{FrameReader, Read1, write_frame};
use lbw_common::{ClientMsg, Input, PROTO_VERSION, ServerMsg};

use crate::scheduler::Outbox;

/// Heartbeat- und Statistik-Intervall.
pub const PING_EVERY: Duration = Duration::from_secs(2);

/// Verbindungszustand, den Pipeline und Netz teilen.
#[derive(Default)]
struct State {
    /// Generation der aktiven Verbindung (0 = keine).
    active: u64,
    gen_counter: u64,
    stream: Option<TcpStream>,
    refresh: bool,
}

/// Gemeinsamer Server-Zustand.
pub struct Shared {
    pub outbox: Outbox,
    pub server_id: u64,
    pub size: (u16, u16),
    /// Verbindung gilt als tot nach so langer Stille (> 60 s Anforderung).
    pub dead_after: Duration,
    input: Mutex<Sender<Input>>,
    state: Mutex<State>,
    t0: Instant,
}

impl Shared {
    #[must_use]
    pub fn new(
        outbox: Outbox,
        size: (u16, u16),
        input: Sender<Input>,
        dead_after: Duration,
    ) -> Arc<Self> {
        let server_id = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_or(1, |d| d.as_nanos() as u64 | 1);
        Arc::new(Self {
            outbox,
            server_id,
            size,
            dead_after,
            input: Mutex::new(input),
            state: Mutex::default(),
            t0: Instant::now(),
        })
    }

    /// Ist gerade ein Client verbunden?
    #[must_use]
    pub fn connected(&self) -> bool {
        self.state.lock().unwrap().active != 0
    }

    /// Voll-Refresh angefordert? (setzt die Anforderung zurück)
    pub fn take_refresh(&self) -> bool {
        std::mem::take(&mut self.state.lock().unwrap().refresh)
    }

    fn now_ms(&self) -> u32 {
        self.t0.elapsed().as_millis() as u32
    }

    fn is_active(&self, g: u64) -> bool {
        self.state.lock().unwrap().active == g
    }

    fn drop_conn(&self, g: u64) {
        let mut s = self.state.lock().unwrap();
        if s.active == g {
            s.active = 0;
            if let Some(st) = s.stream.take() {
                let _ = st.shutdown(Shutdown::Both);
            }
        }
    }
}

/// Startet den Accept-Thread.
pub fn serve(listener: TcpListener, sh: Arc<Shared>) -> std::thread::JoinHandle<()> {
    std::thread::spawn(move || {
        for s in listener.incoming() {
            match s {
                Ok(s) => {
                    let sh = sh.clone();
                    std::thread::spawn(move || {
                        let peer = s.peer_addr().map(|a| a.to_string()).unwrap_or_default();
                        if let Err(e) = connection(s, &sh) {
                            eprintln!("[server] Verbindung {peer} beendet: {e}");
                        }
                    });
                }
                Err(e) => eprintln!("[server] accept: {e}"),
            }
        }
    })
}

fn io(e: std::io::Error) -> String {
    e.to_string()
}

/// Handshake, dann Reader-Schleife; Writer läuft parallel.
fn connection(mut s: TcpStream, sh: &Arc<Shared>) -> Result<(), String> {
    s.set_nodelay(true).map_err(io)?;
    s.set_read_timeout(Some(Duration::from_secs(10)))
        .map_err(io)?;
    let mut fr = FrameReader::new();
    let Read1::Frame(b) = fr.read(&mut s).map_err(io)? else {
        return Err("kein Hello".into());
    };
    let ClientMsg::Hello {
        version,
        server_id,
        seq,
    } = ClientMsg::decode(&b).map_err(|e| e.to_string())?
    else {
        return Err("erste Nachricht ist kein Hello".into());
    };
    if version != PROTO_VERSION {
        return Err(format!("Protokoll {version} ≠ {PROTO_VERSION}"));
    }

    // Aktivieren: alte Verbindung schließen, Scheduler zurücksetzen.
    let g = {
        let mut st = sh.state.lock().unwrap();
        if let Some(old) = st.stream.take() {
            let _ = old.shutdown(Shutdown::Both);
        }
        st.gen_counter += 1;
        st.active = st.gen_counter;
        st.stream = Some(s.try_clone().map_err(io)?);
        let conn = st.gen_counter;
        let resumed = sh.outbox.with(|q| {
            let ok = server_id == sh.server_id && seq != 0 && seq == q.sent_seq;
            q.reset(ok, conn);
            q.push_control(ServerMsg::Hello {
                server_id: sh.server_id,
                w: sh.size.0,
                h: sh.size.1,
                resumed: ok,
            });
            ok
        });
        st.refresh = !resumed;
        eprintln!(
            "[server] Client verbunden (gen {}, resume {resumed})",
            st.active
        );
        st.active
    };

    let ws = s.try_clone().map_err(io)?;
    let shw = sh.clone();
    std::thread::spawn(move || writer(ws, &shw, g));

    s.set_read_timeout(Some(Duration::from_millis(500)))
        .map_err(io)?;
    let mut last_rx = Instant::now();
    let r = loop {
        if !sh.is_active(g) {
            break Ok(());
        }
        match fr.read(&mut s) {
            Ok(Read1::Frame(b)) => {
                last_rx = Instant::now();
                match ClientMsg::decode(&b) {
                    Ok(m) => handle(sh, m),
                    Err(e) => break Err(e.to_string()),
                }
            }
            Ok(Read1::Idle) if last_rx.elapsed() > sh.dead_after => {
                break Err(format!("{} s ohne Daten", sh.dead_after.as_secs()));
            }
            Ok(Read1::Idle) => {}
            Err(e) => break Err(io(e)),
        }
    };
    sh.drop_conn(g);
    r
}

fn handle(sh: &Shared, m: ClientMsg) {
    match m {
        ClientMsg::Input(i) => {
            let _ = sh.input.lock().unwrap().send(i);
        }
        ClientMsg::Ack { rx_bytes, .. } => sh.outbox.with(|q| q.ack(rx_bytes)),
        ClientMsg::Ping { t } => sh.outbox.with(|q| q.push_control(ServerMsg::Pong { t })),
        ClientMsg::Pong { t } => {
            let rtt = Duration::from_millis(u64::from(sh.now_ms().wrapping_sub(t)));
            sh.outbox.with(|q| q.rtt(rtt));
        }
        ClientMsg::Hello { .. } => {}
    }
}

/// Sendet, was der Scheduler freigibt; Ping + Stats alle [`PING_EVERY`].
fn writer(mut s: TcpStream, sh: &Shared, g: u64) {
    let mut last_ping = Instant::now() - PING_EVERY;
    while sh.is_active(g) {
        if last_ping.elapsed() >= PING_EVERY {
            last_ping = Instant::now();
            let t = sh.now_ms();
            sh.outbox.with(|q| {
                q.push_control(ServerMsg::Ping { t });
                let now = Instant::now();
                q.push_control(ServerMsg::Stats {
                    rate: q.tx_rate(now),
                    backlog: q.image_backlog() as u32,
                    tiles: q.tiles_sent,
                });
            });
        }
        if let Some(b) = sh.outbox.next_blocking(Duration::from_millis(250), g)
            && let Err(e) = write_frame(&mut s, &b)
        {
            eprintln!("[server] senden: {e}");
            break;
        }
    }
    sh.drop_conn(g);
}
