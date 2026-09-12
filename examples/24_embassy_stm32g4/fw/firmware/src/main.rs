//! USB-CDC dual-mode firmware for the WeAct STM32G474CEU6.
//!
//! First-byte dispatch: `\n`/`\r`-terminated ASCII lines go to the text
//! handler, `0x00`-terminated postcard+COBS frames go to the binary handler.

#![no_std]
#![no_main]

use defmt::{info, panic};
use defmt_rtt as _;
use embassy_futures::join::join;
use embassy_stm32::usb::{self, Driver, Instance};
use embassy_stm32::{bind_interrupts, peripherals, Config};
use embassy_usb::class::cdc_acm::{CdcAcmClass, State};
use embassy_usb::driver::EndpointError;
use embassy_usb::Builder;
use g474_common::err;
use g474_common::frame::encode_resp;
use g474_common::router::{InEvent, Router};
use g474_common::text::handle_line;
use g474_common::{DeviceResp, HostCmd, MAX_FRAME, PROTO_VER};
use panic_probe as _;

bind_interrupts!(struct Irqs {
    USB_LP => usb::InterruptHandler<peripherals::USB>;
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
    let usb_fut = usb.run();
    let proto_fut = async {
        loop {
            class.wait_connection().await;
            info!("USB connected");
            let _ = serve(&mut class).await;
            info!("USB disconnected");
        }
    };
    join(usb_fut, proto_fut).await;
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

async fn serve<'d, T: Instance + 'd>(
    class: &mut CdcAcmClass<'d, Driver<'d, T>>,
) -> Result<(), Disconnected> {
    let mut pkt = [0u8; 64];
    let mut router = Router::new();
    let mut tx = [0u8; MAX_FRAME + 32];
    loop {
        let n = class.read_packet(&mut pkt).await?;
        for &b in &pkt[..n] {
            match router.feed(b) {
                None => {}
                Some(InEvent::Text(line)) => {
                    // GET UID is answered with the real chip UID here, not in common.
                    let trimmed = line.as_str().trim();
                    let reply: heapless::String<96> = if trimmed.eq_ignore_ascii_case("GET UID")
                        || trimmed.eq_ignore_ascii_case("G UID")
                    {
                        let mut s: heapless::String<96> = heapless::String::new();
                        let _ = core::fmt::write(
                            &mut s,
                            format_args!("OK UID {}", embassy_stm32::uid::uid_hex()),
                        );
                        s
                    } else {
                        handle_line(line.as_str())
                    };
                    write_all(class, reply.as_bytes()).await?;
                    write_all(class, b"\n").await?;
                }
                Some(InEvent::Cmd(cmd)) => {
                    let resp = answer_cmd(cmd);
                    if let Some(len) = encode_resp(&resp, &mut tx) {
                        write_all(class, &tx[..len]).await?;
                    }
                }
                Some(InEvent::TextTooLong) => {
                    write_all(class, b"ERR TOOLONG\n").await?;
                }
                Some(InEvent::BinError) => {
                    let resp = DeviceResp::Err {
                        code: err::BAD_FRAME,
                    };
                    if let Some(len) = encode_resp(&resp, &mut tx) {
                        write_all(class, &tx[..len]).await?;
                    }
                }
            }
        }
    }
}
