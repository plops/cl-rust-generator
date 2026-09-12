//! Transport tasks (F0/F2): USB-CDC + UART serving the shared router.
//!
//! Both tasks share `CONTROL` (exclusive mode state) and the `FREQ_REQ` /
//! `FREQ_RESP` channels owned by the mode-E task. Binary replies are encoded
//! into a `MAX_FRAME + 32` scratch buffer (COBS overhead).

use defmt::warn;
use embassy_stm32::mode;
use embassy_stm32::peripherals;
use embassy_stm32::usart::Uart;
use embassy_stm32::usb::{Driver, Instance};
use embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex;
use embassy_sync::mutex::Mutex;
use embassy_time::Timer;
use embassy_usb::class::cdc_acm::CdcAcmClass;
use g474_common::err;
use g474_common::frame::encode_resp;
use g474_common::router::{InEvent, Router};
use g474_common::text::reply_text;
use g474_common::{DeviceResp, HostCmd, MAX_FRAME};

use crate::awg_06::{AwgReq, AWG_ACK, AWG_REQ};
use crate::control_04::{Action, Control};
use crate::mode_freq_05::{FreqResult, FREQ_REQ, FREQ_RESP};
use crate::scope_07::{ScopeReq, SCOPE_PER_BLOCK, SCOPE_REQ, SCOPE_RESP, SCOPE_SNAP};
use crate::usb_02::{write_all, Disconnected};
use crate::vna_08::{VnaReq, VNA_PER_BLOCK, VNA_REQ, VNA_RESP, VNA_SNAP};
use g474_common::blocks_05::crc16_update;

/// Exclusive mode state shared by USB and UART tasks.
pub static CONTROL: Mutex<CriticalSectionRawMutex, Control> = Mutex::new(Control::new());

/// Transport-independent reply to one router event. Binary replies are
/// encoded into `tx`; the length is valid only for `Out::Bin`.
enum Out {
    Text(heapless::String<96>),
    Bin(usize),
    Scope { off: usize, len: usize },
    Vna { off: usize, len: usize },
    Skip,
}

fn discriminant(cmd: &HostCmd) -> u8 {
    match cmd {
        HostCmd::Ping => 0,
        HostCmd::GetVer => 1,
        HostCmd::Echo(_) => 2,
        HostCmd::ModeStop => 3,
        HostCmd::SelfTest => 4,
        HostCmd::FreqStart(_) => 5,
        HostCmd::FreqRead => 6,
        HostCmd::ScopeStart(_) => 7,
        HostCmd::ScopeRead { .. } => 8,
        HostCmd::AwgLoad { .. } => 9,
        HostCmd::AwgStart(_) => 10,
        HostCmd::CapStart(_) => 11,
        HostCmd::CapRead => 12,
        HostCmd::VnaStart(_) => 13,
        HostCmd::VnaRead { .. } => 14,
        HostCmd::BlockAck { .. } => 15,
    }
}

/// Execute a control action: pure replies answer immediately, measurements
/// round-trip through the mode task (second request → `MODE_BUSY`).
async fn run_action(action: Action) -> DeviceResp {
    match action {
        // Scope/VNA downloads stream multiple frames; dispatch routes them to
        // Out::Scope/Out::Vna before run_action, so these arms are unreachable.
        Action::ScopeRead { .. } => unreachable!("scope uses Out::Scope"),
        Action::VnaRead { .. } => unreachable!("vna uses Out::Vna"),
        Action::Reply(r) => r,
        Action::MeasureFreq(cfg) => {
            if FREQ_REQ.try_send(cfg).is_err() {
                return DeviceResp::Err {
                    code: err::MODE_BUSY,
                };
            }
            let r: FreqResult = FREQ_RESP.receive().await;
            let hz = g474_common::modes_04::FreqConfig::hz_from_counts(r.counts, r.gate_ms);
            CONTROL.lock().await.store_freq(r.counts, r.gate_ms);
            DeviceResp::Freq {
                hz,
                counts: r.counts,
                gate_ms: r.gate_ms,
            }
        }
        Action::AwgStart(cfg) => {
            if AWG_REQ
                .try_send(AwgReq::Start {
                    freq_hz: cfg.freq_hz,
                })
                .is_err()
            {
                defmt::info!("awg ch full");
                return DeviceResp::Err {
                    code: err::MODE_BUSY,
                };
            }
            defmt::info!("awg req sent");
            AWG_ACK.receive().await;
            defmt::info!("awg acked");
            DeviceResp::ModeOk {
                mode: g474_common::modes_04::id::C_AWG,
            }
        }
        Action::VnaStart(cfg) => {
            if VNA_REQ.try_send(VnaReq { cfg }).is_err() {
                return DeviceResp::Err {
                    code: err::MODE_BUSY,
                };
            }
            let r = VNA_RESP.receive().await;
            defmt::info!("vna swept {} pts", r.points);
            DeviceResp::ModeOk {
                mode: g474_common::modes_04::id::B_VNA,
            }
        }
    }
}

async fn dispatch(ev: InEvent, tx: &mut [u8; MAX_FRAME + 32], uid_hex: &str) -> Out {
    match ev {
        InEvent::Text(line) => {
            if let Some(s) = CONTROL.lock().await.handle_text(line.as_str()) {
                return Out::Text(s);
            }
            Out::Text(reply_text(line.as_str(), uid_hex))
        }
        InEvent::Cmd(cmd) => {
            defmt::info!("rx {}", discriminant(&cmd));
            let is_stop = matches!(cmd, HostCmd::ModeStop);
            let action = CONTROL.lock().await.handle_cmd(cmd);
            if is_stop {
                // Quiesce the tone generator too (harmless when already idle).
                let _ = AWG_REQ.try_send(AwgReq::Stop);
            }
            match action {
                Action::ScopeRead { off, len } => Out::Scope {
                    off: off as usize,
                    len: len as usize,
                },
                Action::VnaRead { off, len } => Out::Vna {
                    off: off as usize,
                    len: len as usize,
                },
                _ => {
                    let resp = run_action(action).await;
                    match encode_resp(&resp, tx) {
                        Some(n) => Out::Bin(n),
                        None => Out::Skip,
                    }
                }
            }
        }
        InEvent::TextTooLong => Out::Text(heapless::String::from("ERR TOOLONG")),
        InEvent::BinError => match encode_resp(
            &DeviceResp::Err {
                code: err::BAD_FRAME,
            },
            tx,
        ) {
            Some(n) => Out::Bin(n),
            None => Out::Skip,
        },
    }
}

#[embassy_executor::task]
pub async fn usb_run_task(
    mut usb: embassy_usb::UsbDevice<'static, Driver<'static, peripherals::USB>>,
) {
    usb.run().await;
}

#[embassy_executor::task]
pub async fn usb_serve_task(mut class: CdcAcmClass<'static, Driver<'static, peripherals::USB>>) {
    loop {
        class.wait_connection().await;
        defmt::info!("USB connected");
        if serve(&mut class).await.is_err() {
            defmt::info!("USB disconnected");
        }
    }
}

/// Encode one response into `tx` and USB-write it.
async fn send_bin<'d, T: Instance + 'd>(
    class: &mut CdcAcmClass<'d, Driver<'d, T>>,
    tx: &mut [u8; MAX_FRAME + 32],
    resp: &DeviceResp,
) -> Result<(), Disconnected> {
    match encode_resp(resp, tx) {
        Some(n) => write_all(class, &tx[..n]).await,
        None => Ok(()),
    }
}

/// Acquire `off..off+len` samples and stream them as `Block`s + `BlockEnd`.
async fn serve_scope_usb<'d, T: Instance + 'd>(
    class: &mut CdcAcmClass<'d, Driver<'d, T>>,
    tx: &mut [u8; MAX_FRAME + 32],
    off: usize,
    len: usize,
) -> Result<(), Disconnected> {
    if SCOPE_REQ.try_send(ScopeReq { n: off + len }).is_err() {
        return send_bin(
            class,
            tx,
            &DeviceResp::Err {
                code: err::MODE_BUSY,
            },
        )
        .await;
    }
    let got = SCOPE_RESP.receive().await;
    if got.n < off + len {
        return send_bin(class, tx, &DeviceResp::Err { code: err::NO_DATA }).await;
    }
    // CRC covers exactly the downloaded range (streaming, no temp buffer).
    let total = len.div_ceil(SCOPE_PER_BLOCK);
    let mut crc: u16 = 0xFFFF;
    for seq in 0..total {
        let s = off + seq * SCOPE_PER_BLOCK;
        let e = (s + SCOPE_PER_BLOCK).min(off + len);
        let data = {
            let snap = SCOPE_SNAP.lock().await;
            let mut data = heapless::Vec::<u8, 96>::new();
            snap.bytes_into(s, e, &mut data);
            data
        };
        crc = crc16_update(crc, &data);
        send_bin(
            class,
            tx,
            &DeviceResp::Block {
                seq: seq as u16,
                total: total as u16,
                data,
            },
        )
        .await?;
    }
    send_bin(
        class,
        tx,
        &DeviceResp::BlockEnd {
            total: total as u16,
            crc,
        },
    )
    .await
}

/// Download sweep points `[off..off+len)` as `Block`s + `BlockEnd` (USB).
/// Unlike scope, VNA downloads the last completed sweep (no re-acquire).
async fn serve_vna_usb<'d, T: Instance + 'd>(
    class: &mut CdcAcmClass<'d, Driver<'d, T>>,
    tx: &mut [u8; MAX_FRAME + 32],
    off: usize,
    len: usize,
) -> Result<(), Disconnected> {
    let have = VNA_SNAP.lock().await.n;
    if have == 0 {
        return send_bin(class, tx, &DeviceResp::Err { code: err::NO_DATA }).await;
    }
    if off.saturating_add(len) > have {
        return send_bin(class, tx, &DeviceResp::Err { code: err::BAD_ARG }).await;
    }
    let total = len.div_ceil(VNA_PER_BLOCK);
    let mut crc: u16 = 0xFFFF;
    for seq in 0..total {
        let s = off + seq * VNA_PER_BLOCK;
        let e = (s + VNA_PER_BLOCK).min(off + len);
        let data = {
            let snap = VNA_SNAP.lock().await;
            let mut data = heapless::Vec::<u8, 96>::new();
            snap.bytes_into(s, e, &mut data);
            data
        };
        crc = crc16_update(crc, &data);
        send_bin(
            class,
            tx,
            &DeviceResp::Block {
                seq: seq as u16,
                total: total as u16,
                data,
            },
        )
        .await?;
    }
    send_bin(
        class,
        tx,
        &DeviceResp::BlockEnd {
            total: total as u16,
            crc,
        },
    )
    .await
}

/// UART twin of [`serve_vna_usb`].
async fn serve_vna_uart(
    uart: &mut Uart<'static, mode::Async>,
    tx: &mut [u8; MAX_FRAME + 32],
    off: usize,
    len: usize,
) {
    async fn send(uart: &mut Uart<'static, mode::Async>, tx: &[u8]) {
        usart_write_all(uart, tx).await;
    }
    let have = VNA_SNAP.lock().await.n;
    if have == 0 {
        if let Some(n) = encode_resp(&DeviceResp::Err { code: err::NO_DATA }, tx) {
            send(uart, &tx[..n]).await;
        }
        return;
    }
    if off.saturating_add(len) > have {
        if let Some(n) = encode_resp(&DeviceResp::Err { code: err::BAD_ARG }, tx) {
            send(uart, &tx[..n]).await;
        }
        return;
    }
    let total = len.div_ceil(VNA_PER_BLOCK);
    let mut crc: u16 = 0xFFFF;
    for seq in 0..total {
        let s = off + seq * VNA_PER_BLOCK;
        let e = (s + VNA_PER_BLOCK).min(off + len);
        let data = {
            let snap = VNA_SNAP.lock().await;
            let mut data = heapless::Vec::<u8, 96>::new();
            snap.bytes_into(s, e, &mut data);
            data
        };
        crc = crc16_update(crc, &data);
        if let Some(n) = encode_resp(
            &DeviceResp::Block {
                seq: seq as u16,
                total: total as u16,
                data,
            },
            tx,
        ) {
            send(uart, &tx[..n]).await;
        }
    }
    if let Some(n) = encode_resp(
        &DeviceResp::BlockEnd {
            total: total as u16,
            crc,
        },
        tx,
    ) {
        send(uart, &tx[..n]).await;
    }
}

async fn serve<'d, T: Instance + 'd>(
    class: &mut CdcAcmClass<'d, Driver<'d, T>>,
) -> Result<core::convert::Infallible, Disconnected> {
    let mut pkt = [0u8; 64];
    let mut router = Router::new();
    let mut tx = [0u8; MAX_FRAME + 32];
    let uid = embassy_stm32::uid::uid_hex();
    loop {
        let n = class.read_packet(&mut pkt).await?;
        for &b in &pkt[..n] {
            if let Some(ev) = router.feed(b) {
                match dispatch(ev, &mut tx, uid).await {
                    Out::Text(s) => {
                        write_all(class, s.as_bytes()).await?;
                        write_all(class, b"\n").await?;
                    }
                    Out::Bin(m) => {
                        write_all(class, &tx[..m]).await?;
                    }
                    Out::Scope { off, len } => {
                        serve_scope_usb(class, &mut tx, off, len).await?;
                    }
                    Out::Vna { off, len } => {
                        serve_vna_usb(class, &mut tx, off, len).await?;
                    }
                    Out::Skip => {}
                }
            }
        }
    }
}

#[embassy_executor::task]
pub async fn uart_serve_task(uart: Uart<'static, mode::Async>) {
    serve_uart(uart).await;
}

async fn serve_uart(mut uart: Uart<'static, mode::Async>) {
    let mut pkt = [0u8; 64];
    let mut router = Router::new();
    let mut tx = [0u8; MAX_FRAME + 32];
    let uid = embassy_stm32::uid::uid_hex();
    defmt::info!("UART serving on USART1");
    loop {
        // Idle-line detection returns whatever arrived as one chunk.
        match uart.read_until_idle(&mut pkt).await {
            Ok(n) => {
                for &b in &pkt[..n] {
                    if let Some(ev) = router.feed(b) {
                        match dispatch(ev, &mut tx, uid).await {
                            Out::Text(s) => {
                                let _ = usart_write_all(&mut uart, s.as_bytes()).await;
                                let _ = usart_write_all(&mut uart, b"\n").await;
                            }
                            Out::Bin(m) => {
                                let _ = usart_write_all(&mut uart, &tx[..m]).await;
                            }
                            Out::Scope { off, len } => {
                                serve_scope_uart(&mut uart, &mut tx, off, len).await;
                            }
                            Out::Vna { off, len } => {
                                serve_vna_uart(&mut uart, &mut tx, off, len).await;
                            }
                            Out::Skip => {}
                        }
                    }
                }
            }
            Err(_) => {
                warn!("UART read error");
                Timer::after_millis(10).await;
            }
        }
    }
}

async fn usart_write_all(uart: &mut Uart<'static, mode::Async>, data: &[u8]) {
    use embedded_io_async::Write;
    let _ = uart.write_all(data).await;
}

/// UART twin of [`serve_scope_usb`]: UART writes are infallible-by-policy
/// (errors only logged), so this returns `()`.
async fn serve_scope_uart(
    uart: &mut Uart<'static, mode::Async>,
    tx: &mut [u8; MAX_FRAME + 32],
    off: usize,
    len: usize,
) {
    async fn send(uart: &mut Uart<'static, mode::Async>, tx: &[u8]) {
        usart_write_all(uart, tx).await;
    }
    if SCOPE_REQ.try_send(ScopeReq { n: off + len }).is_err() {
        if let Some(n) = encode_resp(
            &DeviceResp::Err {
                code: err::MODE_BUSY,
            },
            tx,
        ) {
            send(uart, &tx[..n]).await;
        }
        return;
    }
    let got = SCOPE_RESP.receive().await;
    if got.n < off + len {
        if let Some(n) = encode_resp(&DeviceResp::Err { code: err::NO_DATA }, tx) {
            send(uart, &tx[..n]).await;
        }
        return;
    }
    let total = len.div_ceil(SCOPE_PER_BLOCK);
    let mut crc: u16 = 0xFFFF;
    for seq in 0..total {
        let s = off + seq * SCOPE_PER_BLOCK;
        let e = (s + SCOPE_PER_BLOCK).min(off + len);
        let data = {
            let snap = SCOPE_SNAP.lock().await;
            let mut data = heapless::Vec::<u8, 96>::new();
            snap.bytes_into(s, e, &mut data);
            data
        };
        crc = crc16_update(crc, &data);
        if let Some(n) = encode_resp(
            &DeviceResp::Block {
                seq: seq as u16,
                total: total as u16,
                data,
            },
            tx,
        ) {
            send(uart, &tx[..n]).await;
        }
    }
    if let Some(n) = encode_resp(
        &DeviceResp::BlockEnd {
            total: total as u16,
            crc,
        },
        tx,
    ) {
        send(uart, &tx[..n]).await;
    }
}
