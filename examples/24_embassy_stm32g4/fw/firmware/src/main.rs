//! USB-CDC dual-mode firmware for the WeAct STM32G474CEU6.
//!
//! First-byte dispatch: `\n`/`\r`-terminated ASCII lines go to the text
//! handler, `0x00`-terminated postcard+COBS frames go to the binary handler.

#![no_std]
#![no_main]

use defmt::{info, panic, warn};
use defmt_rtt as _;
use embassy_futures::join::join3;
use embassy_stm32::usart::{Config as UsartConfig, Uart};
use embassy_stm32::usb::{self, Driver, Instance};
use embassy_stm32::{bind_interrupts, dma, mode, peripherals, usart, Config};
use embassy_time::Timer;
use embassy_usb::class::cdc_acm::{CdcAcmClass, State};
use embassy_usb::driver::EndpointError;
use embassy_usb::Builder;
use embedded_io_async::Write;
use g474_common::err;
use g474_common::frame::encode_resp;
use g474_common::router::{InEvent, Router};
use g474_common::text::reply_text;
use g474_common::{DeviceResp, HostCmd, MAX_FRAME, PROTO_VER};
use panic_probe as _;

bind_interrupts!(struct Irqs {
    USB_LP => usb::InterruptHandler<peripherals::USB>;
    USART1 => usart::InterruptHandler<peripherals::USART1>;
    DMA1_CHANNEL1 => dma::InterruptHandler<peripherals::DMA1_CH1>;
    DMA1_CHANNEL2 => dma::InterruptHandler<peripherals::DMA1_CH2>;
});

#[embassy_executor::main]
async fn main(_spawner: embassy_executor::Spawner) {
    let mut config = Config::default();
    {
        use embassy_stm32::rcc::*;
        // HSI48 trimmed by USB SOF via CRS; feeds the 48 MHz USB domain.
        // No HSE dependency: the board crystal population is unverified.
        config.rcc.hsi48 = Some(Hsi48Config {
            sync_from_usb: true,
        });
        config.rcc.mux.clk48sel = mux::Clk48sel::HSI48;
    }
    let p = embassy_stm32::init(config);
    info!("usb_proto starting");

    let driver = Driver::new(p.USB, Irqs, p.PA12, p.PA11);

    let mut config = embassy_usb::Config::new(0xc0de, 0xcafe);
    config.manufacturer = Some("cl-rust-generator");
    config.product = Some("G474 dual-mode proto");
    config.serial_number = Some("000001");

    static CONFIG_DESC: static_cell::StaticCell<[u8; 256]> = static_cell::StaticCell::new();
    static BOS_DESC: static_cell::StaticCell<[u8; 256]> = static_cell::StaticCell::new();
    static CONTROL_BUF: static_cell::StaticCell<[u8; 64]> = static_cell::StaticCell::new();
    static CDC_STATE: static_cell::StaticCell<State> = static_cell::StaticCell::new();
    let config_desc = CONFIG_DESC.init([0; 256]);
    let bos_desc = BOS_DESC.init([0; 256]);
    let control_buf = CONTROL_BUF.init([0; 64]);
    let cdc_state = CDC_STATE.init(State::new());

    let mut builder = Builder::new(
        driver,
        config,
        config_desc,
        bos_desc,
        &mut [], // no MSOS descriptors
        control_buf,
    );
    let mut class = CdcAcmClass::new(&mut builder, cdc_state, 64);
    let mut usb = builder.build();

    // Second transport: USART1 on PA9 (TX) / PA10 (RX), 115200 8N1.
    // Same protocol as USB (shared Router), reachable via STLink VCP.
    let mut uc = UsartConfig::default();
    uc.baudrate = 115_200;
    let mut uart = Uart::new(p.USART1, p.PA10, p.PA9, p.DMA1_CH1, p.DMA1_CH2, Irqs, uc).unwrap();
    info!("UART ready on PA9/PA10 @115200");

    let usb_fut = usb.run();
    let proto_fut = async {
        loop {
            class.wait_connection().await;
            info!("USB connected");
            let _ = serve(&mut class).await;
            info!("USB disconnected");
        }
    };
    let uart_fut = serve_uart(&mut uart);
    join3(usb_fut, proto_fut, uart_fut).await;
}

struct Disconnected;

impl From<EndpointError> for Disconnected {
    fn from(val: EndpointError) -> Self {
        match val {
            EndpointError::BufferOverflow => panic!("USB buffer overflow"),
            EndpointError::Disabled => Disconnected,
        }
    }
}

async fn write_all<'d, T: Instance + 'd>(
    class: &mut CdcAcmClass<'d, Driver<'d, T>>,
    data: &[u8],
) -> Result<(), Disconnected> {
    for chunk in data.chunks(64) {
        class.write_packet(chunk).await?;
    }
    Ok(())
}

fn answer_cmd(cmd: HostCmd) -> DeviceResp {
    match cmd {
        HostCmd::Ping => DeviceResp::Pong,
        HostCmd::GetVer => DeviceResp::Ver {
            proto: PROTO_VER,
            fw: heapless::String::from(env!("CARGO_PKG_VERSION")),
        },
        HostCmd::Echo(v) => DeviceResp::Echo(v),
    }
}

/// Transport-independent reply to one router event. Binary replies are
/// encoded into `tx`; the length is valid only for `Out::Bin`.
enum Out {
    Text(heapless::String<96>),
    Bin(usize),
    Skip,
}

fn dispatch(ev: InEvent, tx: &mut [u8; MAX_FRAME + 32], uid_hex: &str) -> Out {
    match ev {
        InEvent::Text(line) => Out::Text(reply_text(line.as_str(), uid_hex)),
        InEvent::Cmd(cmd) => match encode_resp(&answer_cmd(cmd), tx) {
            Some(n) => Out::Bin(n),
            None => Out::Skip,
        },
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

async fn serve<'d, T: Instance + 'd>(
    class: &mut CdcAcmClass<'d, Driver<'d, T>>,
) -> Result<(), Disconnected> {
    let mut pkt = [0u8; 64];
    let mut router = Router::new();
    let mut tx = [0u8; MAX_FRAME + 32];
    let uid = embassy_stm32::uid::uid_hex();
    loop {
        let n = class.read_packet(&mut pkt).await?;
        for &b in &pkt[..n] {
            match router.feed(b) {
                None => {}
                Some(ev) => match dispatch(ev, &mut tx, uid) {
                    Out::Text(s) => {
                        write_all(class, s.as_bytes()).await?;
                        write_all(class, b"\n").await?;
                    }
                    Out::Bin(m) => {
                        write_all(class, &tx[..m]).await?;
                    }
                    Out::Skip => {}
                },
            }
        }
    }
}

async fn serve_uart(uart: &mut Uart<'_, mode::Async>) {
    let mut pkt = [0u8; 64];
    let mut router = Router::new();
    let mut tx = [0u8; MAX_FRAME + 32];
    let uid = embassy_stm32::uid::uid_hex();
    info!("UART serving on USART1");
    loop {
        // Idle-line detection returns whatever arrived as one chunk.
        match uart.read_until_idle(&mut pkt).await {
            Ok(n) => {
                for &b in &pkt[..n] {
                    if let Some(ev) = router.feed(b) {
                        match dispatch(ev, &mut tx, uid) {
                            Out::Text(s) => {
                                let _ = uart.write_all(s.as_bytes()).await;
                                let _ = uart.write_all(b"\n").await;
                            }
                            Out::Bin(m) => {
                                let _ = uart.write_all(&tx[..m]).await;
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
