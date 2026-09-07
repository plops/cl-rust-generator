#![no_std]
#![no_main]
use defmt_rtt as _;
use embassy_rp::adc::{Adc, Channel as AdcChannel, Config as AdcConfig};
use embassy_rp::gpio::{Flex, Level, Output, Pull};
use embassy_rp::pio::{Config as PioConfig, Pio};
use embassy_rp::pwm::{Config as PwmConfig, Pwm};
use embassy_rp::uart::{Config as UartConfig, Uart};
use embassy_rp::{bind_interrupts, pac};
use embassy_time::{Duration, Instant, Ticker, Timer};
use embedded_io_async::{Read, Write};
use panic_probe as _;
use pico_link_proto as proto;
use pio::pio_asm;
embassy_rp::bind_interrupts!(struct Irqs {
    ADC_IRQ_FIFO => embassy_rp::adc::InterruptHandler;
    PIO0_IRQ_0 => embassy_rp::pio::InterruptHandler<embassy_rp::peripherals::PIO0>;
    UART0_IRQ => embassy_rp::uart::BufferedInterruptHandler<embassy_rp::peripherals::UART0>;
});
static PWM_CH: embassy_sync::channel::Channel<embassy_sync::blocking_mutex::raw::ThreadModeRawMutex, pico_link_proto::PwmCmd, 8> = embassy_sync::channel::Channel::new();
static HSTX_CH: embassy_sync::channel::Channel<embassy_sync::blocking_mutex::raw::ThreadModeRawMutex, pico_link_proto::HstxCmd, 4> = embassy_sync::channel::Channel::new();
static ADC_CH: embassy_sync::channel::Channel<embassy_sync::blocking_mutex::raw::ThreadModeRawMutex, pico_link_proto::AdcCmd, 4> = embassy_sync::channel::Channel::new();
static CAP_CH: embassy_sync::channel::Channel<embassy_sync::blocking_mutex::raw::ThreadModeRawMutex, u8, 4> = embassy_sync::channel::Channel::new();
static READ_CH: embassy_sync::channel::Channel<embassy_sync::blocking_mutex::raw::ThreadModeRawMutex, (u8, u16), 4> = embassy_sync::channel::Channel::new();
static BLOCK_READY: embassy_sync::channel::Channel<embassy_sync::blocking_mutex::raw::ThreadModeRawMutex, (u8, u16), 2> = embassy_sync::channel::Channel::new();
static BLOCK_BUF: embassy_sync::mutex::Mutex<embassy_sync::blocking_mutex::raw::ThreadModeRawMutex, [u16; 256]> = embassy_sync::mutex::Mutex::new([0; 256]);
static BLOCK_META: embassy_sync::mutex::Mutex<embassy_sync::blocking_mutex::raw::ThreadModeRawMutex, (u8, u16)> = embassy_sync::mutex::Mutex::new((0, 0));
static TEMP_VAL: embassy_sync::mutex::Mutex<embassy_sync::blocking_mutex::raw::ThreadModeRawMutex, u16> = embassy_sync::mutex::Mutex::new(0);
static CAP_VAL: embassy_sync::mutex::Mutex<embassy_sync::blocking_mutex::raw::ThreadModeRawMutex, u32> = embassy_sync::mutex::Mutex::new(0);
static CAP_PIO_VAL: embassy_sync::mutex::Mutex<embassy_sync::blocking_mutex::raw::ThreadModeRawMutex, u32> = embassy_sync::mutex::Mutex::new(0);
static HSTX_BUF_CELL: static_cell::StaticCell<[u32; 256]> = static_cell::StaticCell::new();
static UART_TX_BUF: static_cell::StaticCell<[u8; 256]> = static_cell::StaticCell::new();
static UART_RX_BUF: static_cell::StaticCell<[u8; 256]> = static_cell::StaticCell::new();
const SIN_Q16: [u16; 64] = [32768,35979,39160,42279,45307,48214,50972,53555,55938,58097,60013,61666,63041,64124,64905,65377,65535,65377,64905,64124,63041,61666,60013,58097,55938,53555,50972,48214,45307,42279,39160,35979,32768,29556,26375,23256,20228,17321,14563,11980,9597,7438,5522,3869,2494,1411,630,158,0,158,630,1411,2494,3869,5522,7438,9597,11980,14563,17321,20228,23256,26375,29556];
const CAP_PROG: pio::Program<12> = pio_asm!(
    "set pindirs, 1",
    "set pins, 1 [31]",
    "set pindirs, 0",
    "mov x, ~null",
    "again:",
    "jmp x--, dec",
    "mov isr, null",
    "push",
    "jmp again",
    "dec:",
    "jmp pin, again",
    "mov isr, x",
    "push",
    "jmp again",
);
fn temp_raw_to_c10(raw: u16) -> i16 {
    {
        let mv: i32 = (raw as i32 * 3300) / 4096;
        (270 - ((mv - 706) * 1000) / 1721) as i16
}
}
fn pwm_carrier() -> PwmConfig {
    {
        let c = PwmConfig::default();
                c.top=6250;
        c
}
}
fn sine_step(freq_hz: u32) -> u32 {
    ((freq_hz as u64 * 4294967296) / 500) as u32
}
fn sine_duty(acc: u32, amp_tenth: u16, phase_off: u32) -> u16 {
    {
        let idx: usize = ((acc >> 26) + phase_off & 63) as usize;
        ((((SIN_Q16[idx] as u32 * 6251) / 65536) * amp_tenth as u32) / 1000) as u16
}
}
#[embassy_executor::task]
async fn adc_task(mut adc: embassy_rp::adc::Adc<'static, embassy_rp::adc::Async>, pin0: embassy_rp::peripherals::PIN_26, pin1: embassy_rp::peripherals::PIN_27, pin2: embassy_rp::peripherals::PIN_28, pin_ts: embassy_rp::peripherals::ADC_TEMP_SENSOR) {
    {
        let mut c0 = embassy_rp::adc::Channel::new_pin(pin0, Pull::None);
        let mut c1 = embassy_rp::adc::Channel::new_pin(pin1, Pull::None);
        let mut c2 = embassy_rp::adc::Channel::new_pin(pin2, Pull::None);
        let mut cts = embassy_rp::adc::Channel::new_temp_sensor(pin_ts);
        let rate_hz: u32 = 1000;
        let mut temp_every: u32 = 100;
        let mut tick = Ticker::every(Duration::from_millis(1));
        let mut n: u32 = 0;
        let mut idx0: usize = 0;
        let mut idx1: usize = 0;
        let mut idx2: usize = 0;
        let mut valid0: u16 = 0;
        let mut valid1: u16 = 0;
        let mut valid2: u16 = 0;
        let mut cap0: [u16; 256] = [0; 256];
        let mut cap1: [u16; 256] = [0; 256];
        let mut cap2: [u16; 256] = [0; 256];
        loop {
            while let Ok(cmd) = ADC_CH.try_receive() {
                                rate_hz=cmd.rate_hz.clamp(1, 20000);
                temp_every=if rate_hz / 10 < 1 {
                    1
} else {
                    rate_hz / 10
};
                tick=Ticker::every(Duration::from_micros((1000000 / rate_hz) as u64));
                Timer::after(Duration::from_micros((cmd.phase_deg as u64 * 1000000) / (360 * rate_hz as u64))).await
}
            tick.next().await;
            if let Ok(v) = adc.read(&mut c0).await {
                                                cap0[idx0]=v;
                                idx0=(idx0 + 1) % 256;
                if valid0 < 256 {
                    valid0 += 1
}
}
            if let Ok(v) = adc.read(&mut c1).await {
                                                cap1[idx1]=v;
                                idx1=(idx1 + 1) % 256;
                if valid1 < 256 {
                    valid1 += 1
}
}
            if let Ok(v) = adc.read(&mut c2).await {
                                                cap2[idx2]=v;
                                idx2=(idx2 + 1) % 256;
                if valid2 < 256 {
                    valid2 += 1
}
}
            n += 1;
            if temp_every <= n {
                                n=0;
                if let Ok(t) = adc.read(&mut cts).await {
                                        {
                        let mut g = TEMP_VAL.lock().await;
                                                *g=t;
}
}
}
            while let Ok(req) = READ_CH.try_receive() {
                {
                    let ch: u8 = req.0;
                    let want: u16 = req.1;
                    let mut m: u16 = 0;
                    if ch == 0 {
                                                m=if want < valid0 {
                            want
} else {
                            valid0
};
} else {
                        if ch == 1 {
                                                        m=if want < valid1 {
                                want
} else {
                                valid1
};
} else {
                                                        m=if want < valid2 {
                                want
} else {
                                valid2
};
}
}
                    {
                        let mut g = BLOCK_BUF.lock().await;
                        let mut meta = BLOCK_META.lock().await;
                                                meta.0=ch;
                                                meta.1=m;
                        {
                            let mu: usize = m as usize;
                            for j in 0..mu {
                                if ch == 0 {
                                                                        g[j]=cap0[((idx0 + 256) - (mu - j)) % 256];
} else {
                                    if ch == 1 {
                                                                                g[j]=cap1[((idx1 + 256) - (mu - j)) % 256];
} else {
                                                                                g[j]=cap2[((idx2 + 256) - (mu - j)) % 256];
}
}
}
}
                        BLOCK_READY.send((ch, m)).await
}
}
}
}
}
}
#[embassy_executor::task]
async fn pwm_task(p0: embassy_rp::pwm::Pwm<'static>, p1: embassy_rp::pwm::Pwm<'static>, p2: embassy_rp::pwm::Pwm<'static>, p3: embassy_rp::pwm::Pwm<'static>) {
    {
        let mut pwms = [p0, p1, p2, p3];
        let mut freq: [u32; 4] = [5; 4];
        let mut amp: [u16; 4] = [800; 4];
        let mut ph: [u16; 4] = [0; 4];
        let mut acc: [u32; 4] = [0; 4];
        let mut tick = Ticker::every(Duration::from_millis(2));
        loop {
            while let Ok(cmd) = PWM_CH.try_receive() {
                {
                    let ch: usize = cmd.ch as usize;
                    if ch < 4 {
                                                freq[ch]=cmd.freq_hz.clamp(1, 1000);
                                                amp[ch]=cmd.amp_tenth_pct.clamp(0, 1000);
                                                ph[ch]=cmd.phase_deg % 360;
}
}
}
            tick.next().await;
            for ch in 0..4 {
                                acc[ch]=acc[ch] + sine_step(freq[ch]);
                {
                    let duty = sine_duty(acc[ch], amp[ch], (ph[ch] as u32 * 64) / 360);
                    let mut cfg = pwm_carrier();
                                        cfg.compare_a=duty;
                    pwms[ch].set_config(&cfg)
}
}
}
}
}
fn flex_measure(f: &mut embassy_rp::gpio::Flex<'static>) -> u32 {
    f.set_as_output();
    f.set_high();
    for _ in 0..6000 {
        core::hint::spin_loop()
}
    f.set_as_input();
    {
        let t0 = Instant::now();
        let mut tries: u32 = 0;
        while f.is_high() && tries < 200000 {
            tries += 1
}
        t0.elapsed().as_micros() as u32
}
}
#[embassy_executor::task]
async fn cap_task(mut rx: embassy_rp::pio::StateMachineRx<'static, embassy_rp::peripherals::PIO0, 0>, p20: embassy_rp::peripherals::PIN_20, p21: embassy_rp::peripherals::PIN_21, p23: embassy_rp::peripherals::PIN_23) {
    {
        let mut flexes: [embassy_rp::gpio::Flex<'static>; 3] = [Flex::new(p20), Flex::new(p21), Flex::new(p23)];
        let mut sel: u8 = 3;
        loop {
            while let Ok(s) = CAP_CH.try_receive() {
                                sel=if s < 4 {
                    s
} else {
                    3
};
}
            if sel < 3 {
                                {
                    let v: u32 = flex_measure(&mut flexes[sel as usize]);
                    {
                        let mut g = CAP_VAL.lock().await;
                                                *g=v;
}
}
} else {
                                {
                    let mut g = CAP_PIO_VAL.lock().await;
                    {
                        let mut h = CAP_VAL.lock().await;
                                                *h=*g;
}
}
}
            if let Some(v) = rx.try_pull() {
                                {
                    let mut g = CAP_PIO_VAL.lock().await;
                                        *g=v;
}
}
            Timer::after(Duration::from_millis(20)).await
}
}
}
fn hstx_init() {
    pac::RESETS.reset().modify(|w| {
        w.set_hstx(false)
});
    pac::RESETS.reset().modify(|w| {
        w.set_dma(false)
});
    while !pac::RESETS.reset_done().read().hstx() {
        core::hint::spin_loop()
}
    while !pac::RESETS.reset_done().read().dma() {
        core::hint::spin_loop()
}
    pac::CLOCKS.clk_hstx_ctrl().modify(|w| {
        w.set_auxsrc(pac::clocks::vals::ClkHstxCtrlAuxsrc::CLK_SYS)
});
    pac::CLOCKS.clk_hstx_ctrl().modify(|w| {
        w.set_enable(true)
});
    pac::IO_BANK0.gpio(12).ctrl().modify(|w| {
        w.set_funcsel(0)
});
    pac::PADS_BANK0.gpio(12).modify(|w| {
        w.set_ie(false)
});
    pac::PADS_BANK0.gpio(12).modify(|w| {
        w.set_od(false)
});
    pac::HSTX_CTRL.csr().modify(|w| {
        w.set_en(false)
});
    pac::HSTX_CTRL.csr().modify(|w| {
        w.set_expand_en(false)
});
    pac::HSTX_CTRL.csr().modify(|w| {
        w.set_shift(31)
});
    pac::HSTX_CTRL.csr().modify(|w| {
        w.set_n_shifts(0)
});
    pac::HSTX_CTRL.csr().modify(|w| {
        w.set_clkdiv(7)
});
    pac::HSTX_CTRL.csr().modify(|w| {
        w.set_clkphase(0)
});
    pac::HSTX_CTRL.bit(0).modify(|w| {
        w.set_sel_p(0)
});
    pac::HSTX_CTRL.bit(0).modify(|w| {
        w.set_sel_n(31)
});
    pac::HSTX_CTRL.bit(0).modify(|w| {
        w.set_inv(false)
});
    pac::HSTX_CTRL.bit(0).modify(|w| {
        w.set_clk(false)
});
    pac::HSTX_CTRL.csr().modify(|w| {
        w.set_en(true)
})
}
fn hstx_dma_start(buf: &[u32; 256]) {
    {
        let ch = pac::DMA.ch(0);
        ch.read_addr().write_value(buf.as_ptr() as u32);
        ch.write_addr().write_value(pac::HSTX_FIFO.fifo().as_ptr() as u32);
        ch.trans_count().modify(|w| {
            w.set_mode(pac::dma::vals::TransCountMode::NORMAL)
});
        ch.trans_count().modify(|w| {
            w.set_count(4294967295)
});
        ch.ctrl_trig().modify(|w| {
            w.set_en(false)
});
        ch.ctrl_trig().modify(|w| {
            w.set_data_size(pac::dma::vals::DataSize::SIZE_WORD)
});
        ch.ctrl_trig().modify(|w| {
            w.set_incr_read(true)
});
        ch.ctrl_trig().modify(|w| {
            w.set_incr_write(false)
});
        ch.ctrl_trig().modify(|w| {
            w.set_ring_size(10)
});
        ch.ctrl_trig().modify(|w| {
            w.set_ring_sel(false)
});
        ch.ctrl_trig().modify(|w| {
            w.set_chain_to(0)
});
        ch.ctrl_trig().modify(|w| {
            w.set_treq_sel(pac::dma::vals::TreqSel::HSTX)
});
        ch.ctrl_trig().modify(|w| {
            w.set_irq_quiet(true)
});
        ch.ctrl_trig().modify(|w| {
            w.set_en(true)
})
}
}
fn hstx_rebuild(buf: &mut [u32; 256], freq_hz: u32, amp_tenth: u16, phase_deg: u16) {
    {
        let cyc: u32 = (freq_hz * 8192) / 15625000;
                cyc=if cyc < 1 {
            1
} else {
            if 64 < cyc {
                64
} else {
                cyc
}
};
        {
            let poff: u32 = (phase_deg as u32 * 64) / 360;
            for w in 0..256 {
                {
                    let mut word: u32 = 0;
                    let mut bit: u32 = 1 as u32;
                    for b in 0..32 {
                        {
                            let pos: u32 = w as u32 * 32 + b as u32;
                            {
                                let sph: u32 = ((pos * cyc * 64) / 8192) % 64;
                                let duty: u32 = (SIN_Q16[((sph + poff) % 64) as usize] as u32 * amp_tenth as u32) / 1000;
                                let saw: u32 = (pos % 128) * 512;
                                if saw < duty {
                                    word=bit
}
}
                            bit<<=(1)
}
}
                                        buf[w]=word;
}
}
}
}
}
#[embassy_executor::task]
async fn hstx_task(mut buf: &'static mut [u32; 256]) {
    hstx_init();
    {
        let mut freq: u32 = 30;
        let mut amp: u16 = 800;
        hstx_rebuild(&mut *buf, freq, amp, 0);
        hstx_dma_start(&*buf);
        loop {
            while let Ok(cmd) = HSTX_CH.try_receive() {
                                freq=cmd.freq_hz.clamp(1, 2000);
                amp=cmd.amp_tenth_pct.clamp(0, 1000);
                hstx_rebuild(&mut *buf, freq, amp, cmd.phase_deg);
                hstx_dma_start(&*buf)
}
            Timer::after(Duration::from_millis(200)).await
}
}
}
async fn send_status(mut tx: &mut embassy_rp::uart::BufferedUartTx, seq: u8) {
    {
        let tg = TEMP_VAL.lock().await;
        let cg = CAP_VAL.lock().await;
        {
            let msg = proto::StatusMsg {seq: seq, temp_c10: temp_raw_to_c10(*tg), cap: *cg, flags: 0};
            let mut wire: [u8; 9] = [0; 9];
            let mut frame: [u8; 68] = [0; 68];
            proto::encode_status(&msg, &mut wire);
            if let Some(n) = proto::encode_frame(&wire, &mut frame) {
                                {
                    let r = tx.write_all(&frame[..n]).await;
                    if r.is_err() {
                        return 
}
}
}
}
}
}
async fn send_block(mut tx: &mut embassy_rp::uart::BufferedUartTx, ch: u8, m: u16) {
    {
        let g = BLOCK_BUF.lock().await;
        {
            let mut off: u16 = 0;
            while off < m {
                {
                    let k: u16 = if m - off < 30 {
                    m - off
} else {
                    30
};
                    {
                        let mut pl: [u8; 64] = [0; 64];
                        let mut frame: [u8; 68] = [0; 68];
                                                pl[0]=proto::TAG_ADC_BLOCK;
                                                pl[1]=ch;
                        proto::put_u16_le(&mut pl, 2, k);
                        for j in 0..k as usize {
                            proto::put_u16_le(&mut pl, 4 + 2 * j, g[(off as usize + j) as usize])
}
                        {
                            let len = 4 + 2 * k as usize;
}
                        if let Some(n) = proto::encode_frame(&pl[..len], &mut frame) {
                                                        {
                                let r = tx.write_all(&frame[..n]).await;
                                if r.is_err() {
                                    return 
}
}
}
}
                                        off=off + k;
}
}
}
}
}
#[embassy_executor::task]
async fn uart_task(mut uart: embassy_rp::uart::BufferedUart) {
    {
        let mut dec = proto::Decoder::new();
        let mut one: [u8; 1] = [0; 1];
        let mut seq: u8 = 0;
        let mut last = Instant::now();
        let both = uart.split();
        let mut tx = both.0;
        let mut rx = both.1;
        loop {
            {
                let r = embassy_time::with_timeout(Duration::from_millis(100), rx.read_exact(&mut one)).await;
                if let Ok(inner) = r {
                    if inner.is_ok() {
                        if let Some(n) = dec.push(one[0]) {
                                                        {
                                let mut pl: [u8; 64] = [0; 64];
                                for i in 0..n as usize {
                                                                        pl[i]=dec.payload_byte(i);
}
                                {
                                    let tag = pl[0];
                                    if tag == proto::TAG_SET_PWM && n == 10 {
                                        if let Some(cmd) = proto::decode_pwm_cmd(&pl[..10]) {
                                            PWM_CH.send(cmd).await
}
} else {
                                        if tag == proto::TAG_SET_HSTX && n == 9 {
                                            if let Some(cmd) = proto::decode_hstx_cmd(&pl[..9]) {
                                                HSTX_CH.send(cmd).await
}
} else {
                                            if tag == proto::TAG_SET_ADC && n == 7 {
                                                if let Some(cmd) = proto::decode_adc_cmd(&pl[..7]) {
                                                    ADC_CH.send(cmd).await
}
} else {
                                                if tag == proto::TAG_CAP_SELECT && n == 2 {
                                                    CAP_CH.send(pl[1]).await
} else {
                                                    if tag == proto::TAG_READ_BLOCK && n == 4 {
                                                        READ_CH.send((pl[1], proto::get_u16_le(&pl[..4], 2))).await
} else {
                                                                                                                defmt::warn!("uart: unknown tag={} len={}", tag, n);
}
}
}
}
}
}
}
}
}
}
                if 500 < last.elapsed().as_millis() as u64 {
                    send_status(&mut tx, seq).await;
                                        seq=seq.wrapping_add(1);
                                        last=Instant::now();
}
                while let Ok(ready) = BLOCK_READY.try_receive() {
                    send_block(&mut tx, ready.0, ready.1).await
}
}
}
}
    #[embassy_executor::main]
    async fn main(spawner: embassy_executor::Spawner) {
        {
            let p = embassy_rp::init(Default::default());
            let uart = Uart::new_blocking(p.UART0, p.PIN_0, p.PIN_1, UartConfig::default());
            let txb = UART_TX_BUF.init([0; 256]);
            let rxb = UART_RX_BUF.init([0; 256]);
            let buart = uart.into_buffered(Irqs, txb, rxb);
            let adc = Adc::new(p.ADC, Irqs, AdcConfig {});
            let pwm0 = Pwm::new_output_a(p.PWM_SLICE1, p.PIN_2, pwm_carrier());
            let pwm1 = Pwm::new_output_a(p.PWM_SLICE2, p.PIN_4, pwm_carrier());
            let pwm2 = Pwm::new_output_a(p.PWM_SLICE3, p.PIN_6, pwm_carrier());
            let pwm3 = Pwm::new_output_a(p.PWM_SLICE4, p.PIN_8, pwm_carrier());
            let pio = Pio::new(p.PIO0, Irqs);
            let cap_pin = pio.common.make_pio_pin(p.PIN_22);
            let prog = pio.common.load_program(CAP_PROG);
            let mut cfg = PioConfig::default();
            let hbuf = HSTX_BUF_CELL.init([0; 256]);
            let mut led = Output::new(p.PIN_25, Level::Low);
            spawner.spawn(adc_task(adc, p.PIN_26, p.PIN_27, p.PIN_28, p.ADC_TEMP_SENSOR)).unwrap();
            spawner.spawn(pwm_task(pwm0, pwm1, pwm2, pwm3)).unwrap();
            spawner.spawn(cap_task(pio.sm0.rx, p.PIN_20, p.PIN_21, p.PIN_23)).unwrap();
            spawner.spawn(hstx_task(hbuf)).unwrap();
            spawner.spawn(uart_task(buart)).unwrap();
            PWM_CH.send(proto::PwmCmd {ch: 0, freq_hz: 5, amp_tenth_pct: 800, phase_deg: 0}).await;
            PWM_CH.send(proto::PwmCmd {ch: 1, freq_hz: 5, amp_tenth_pct: 800, phase_deg: 90}).await;
            PWM_CH.send(proto::PwmCmd {ch: 2, freq_hz: 5, amp_tenth_pct: 800, phase_deg: 180}).await;
            PWM_CH.send(proto::PwmCmd {ch: 3, freq_hz: 5, amp_tenth_pct: 800, phase_deg: 270}).await;
            HSTX_CH.send(proto::HstxCmd {freq_hz: 30, amp_tenth_pct: 800, phase_deg: 0}).await;
            ADC_CH.send(proto::AdcCmd {rate_hz: 1000, phase_deg: 0}).await;
            loop {
                led.set_high();
                Timer::after(Duration::from_millis(100)).await;
                led.set_low();
                Timer::after(Duration::from_millis(900)).await
}
}
}
}