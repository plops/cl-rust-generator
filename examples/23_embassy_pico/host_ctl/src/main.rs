use crossterm::event::{self, Event, KeyCode};
use pico_link_proto as proto;
use ratatui::layout::{Constraint, Direction, Layout};
use ratatui::text::Line;
use ratatui::widgets::{Block, Paragraph};
use serialport::SerialPort;
use std::io::{Read, Write};
use std::time::{Duration, Instant};
type FocusName = &'static str;
pub struct App {
    pub focus: u8,
    pub pwm_ch: u8,
    pub freq: [u32; 4],
    pub amp: [u16; 4],
    pub phase: [u16; 4],
    pub hstx_freq: u32,
    pub hstx_amp: u16,
    pub hstx_phase: u16,
    pub adc_rate: u32,
    pub adc_phase: u16,
    pub cap_sel: u8,
    pub blk_ch: u8,
    pub temp_c10: i16,
    pub cap: u32,
    pub seq: u8,
    pub block: Vec<u16>,
    pub quit: bool,
}
pub fn app_new() -> App {
    App {
        focus: 0,
        pwm_ch: 0,
        freq: [5; 4],
        amp: [800; 4],
        phase: [0; 4],
        hstx_freq: 30,
        hstx_amp: 800,
        hstx_phase: 0,
        adc_rate: 1000,
        adc_phase: 0,
        cap_sel: 3,
        blk_ch: 0,
        temp_c10: 0,
        cap: 0,
        seq: 0,
        block: vec![],
        quit: false,
    }
}
pub fn step_freq(f: u32, up: bool) -> u32 {
    if up {
        (f + 10).clamp(1, 2000)
    } else {
        f.saturating_sub(10).max(1)
    }
}
pub fn step_rate(r: u32, up: bool) -> u32 {
    if up {
        (r + 100).clamp(1, 20000)
    } else {
        r.saturating_sub(100).max(1)
    }
}
pub fn step_amp(a: u16, up: bool) -> u16 {
    if up {
        (a + 50).clamp(0, 1000)
    } else {
        a.saturating_sub(50)
    }
}
pub fn step_phase(p: u16, up: bool) -> u16 {
    (p + (if up { 15 } else { 345 })) % 360
}
pub fn focus_name(f: u8) -> FocusName {
    if f == 0 {
        "PWM"
    } else {
        if f == 1 { "HSTX" } else { "ADC" }
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn step_freq_clamp() {
        assert_eq!(2000, step_freq(1995, true));
        assert_eq!(60, step_freq(50, true));
        assert_eq!(1, step_freq(5, false))
    }
    #[test]
    fn step_rate_clamp() {
        assert_eq!(20000, step_rate(19950, true));
        assert_eq!(1100, step_rate(1000, true));
        assert_eq!(1, step_rate(50, false))
    }
    #[test]
    fn step_amp_clamp() {
        assert_eq!(1000, step_amp(980, true));
        assert_eq!(750, step_amp(800, false));
        assert_eq!(0, step_amp(20, false))
    }
    #[test]
    fn step_phase_wrap() {
        assert_eq!(5, step_phase(350, true));
        assert_eq!(345, step_phase(0, false));
        assert_eq!(10, step_phase(355, true))
    }
    #[test]
    fn focus_names() {
        assert_eq!("PWM", focus_name(0));
        assert_eq!("HSTX", focus_name(1));
        assert_eq!("ADC", focus_name(2))
    }
}
pub fn send_pwm(port: &mut Option<Box<dyn SerialPort>>, ch: u8, freq: u32, amp: u16, phase: u16) {
    if let Some(p) = port {
        {
            let cmd = proto::PwmCmd {
                ch: ch,
                freq_hz: freq,
                amp_tenth_pct: amp,
                phase_deg: phase,
            };
            let mut wire: [u8; 10] = [0; 10];
            let mut frame: [u8; 68] = [0; 68];
            proto::encode_pwm_cmd(&cmd, &mut wire);
            if let Some(n) = proto::encode_frame(&wire, &mut frame) {
                {
                    let _ = p.write_all(&frame[..n]);
                }
            }
        }
    }
}
pub fn send_hstx(port: &mut Option<Box<dyn SerialPort>>, freq: u32, amp: u16, phase: u16) {
    if let Some(p) = port {
        {
            let cmd = proto::HstxCmd {
                freq_hz: freq,
                amp_tenth_pct: amp,
                phase_deg: phase,
            };
            let mut wire: [u8; 9] = [0; 9];
            let mut frame: [u8; 68] = [0; 68];
            proto::encode_hstx_cmd(&cmd, &mut wire);
            if let Some(n) = proto::encode_frame(&wire, &mut frame) {
                {
                    let _ = p.write_all(&frame[..n]);
                }
            }
        }
    }
}
pub fn send_adc(port: &mut Option<Box<dyn SerialPort>>, rate: u32, phase: u16) {
    if let Some(p) = port {
        {
            let cmd = proto::AdcCmd {
                rate_hz: rate,
                phase_deg: phase,
            };
            let mut wire: [u8; 7] = [0; 7];
            let mut frame: [u8; 68] = [0; 68];
            proto::encode_adc_cmd(&cmd, &mut wire);
            if let Some(n) = proto::encode_frame(&wire, &mut frame) {
                {
                    let _ = p.write_all(&frame[..n]);
                }
            }
        }
    }
}
pub fn send_cap(port: &mut Option<Box<dyn SerialPort>>, sel: u8) {
    if let Some(p) = port {
        {
            let mut pl: [u8; 2] = [0; 2];
            let mut frame: [u8; 68] = [0; 68];
            pl[0] = proto::TAG_CAP_SELECT;
            pl[1] = sel;
            if let Some(n) = proto::encode_frame(&pl, &mut frame) {
                {
                    let _ = p.write_all(&frame[..n]);
                }
            }
        }
    }
}
pub fn send_readblock(port: &mut Option<Box<dyn SerialPort>>, ch: u8, want: u16) {
    if let Some(p) = port {
        {
            let mut pl: [u8; 4] = [0; 4];
            let mut frame: [u8; 68] = [0; 68];
            pl[0] = proto::TAG_READ_BLOCK;
            pl[1] = ch;
            proto::put_u16_le(&mut pl, 2, want);
            if let Some(n) = proto::encode_frame(&pl, &mut frame) {
                {
                    let _ = p.write_all(&frame[..n]);
                }
            }
        }
    }
}
pub fn send_focused(port: &mut Option<Box<dyn SerialPort>>, app: &App) {
    if app.focus == 0 {
        send_pwm(
            port,
            app.pwm_ch,
            app.freq[app.pwm_ch as usize],
            app.amp[app.pwm_ch as usize],
            app.phase[app.pwm_ch as usize],
        )
    } else {
        if app.focus == 1 {
            send_hstx(port, app.hstx_freq, app.hstx_amp, app.hstx_phase)
        } else {
            send_adc(port, app.adc_rate, app.adc_phase)
        }
    }
}
pub fn handle_payload(app: &mut App, pl: &[u8], n: usize) {
    {
        let tag = pl[0];
        if tag == proto::TAG_STATUS && n == 9 {
            if let Some(st) = proto::decode_status(&pl[..9]) {
                app.temp_c10 = st.temp_c10;
                app.cap = st.cap;
                app.seq = st.seq;
            }
        } else {
            if tag == proto::TAG_ADC_BLOCK && 4 <= n {
                app.blk_ch = pl[1];
                {
                    let k = proto::get_u16_le(pl, 2);
                    let mut v: Vec<u16> = vec![];
                    for i in 0..k as usize {
                        v.push(proto::block_sample(pl, i))
                    }
                    app.block = v;
                }
            }
        }
    }
}
pub fn pump_serial(
    port: &mut Option<Box<dyn SerialPort>>,
    dec: &mut proto::Decoder,
    app: &mut App,
) {
    if let Some(p) = port {
        {
            let mut buf: [u8; 256] = [0; 256];
            match p.read(&mut buf) {
                Ok(n) => {
                    for i in 0..n {
                        if let Some(m) = dec.push(buf[i]) {
                            {
                                let mut pl: [u8; 64] = [0; 64];
                                for j in 0..m {
                                    pl[j] = dec.payload_byte(j);
                                }
                                handle_payload(app, &pl, m)
                            }
                        }
                    }
                }
                Err(_) => {}
            }
        }
    }
}
pub fn adjust_freq_focused(app: &mut App, up: bool) {
    if app.focus == 0 {
        {
            let ch: usize = app.pwm_ch as usize;
            app.freq[ch] = step_freq(app.freq[ch], up);
        }
    } else {
        if app.focus == 1 {
            app.hstx_freq = step_freq(app.hstx_freq, up);
        } else {
            app.adc_rate = step_rate(app.adc_rate, up);
        }
    }
}
pub fn adjust_amp_focused(app: &mut App, up: bool) {
    if app.focus == 0 {
        {
            let ch: usize = app.pwm_ch as usize;
            app.amp[ch] = step_amp(app.amp[ch], up);
        }
    } else {
        if app.focus == 1 {
            app.hstx_amp = step_amp(app.hstx_amp, up);
        } else {
            app.adc_phase = step_phase(app.adc_phase, up);
        }
    }
}
pub fn adjust_phase_focused(app: &mut App, up: bool) {
    if app.focus == 0 {
        {
            let ch: usize = app.pwm_ch as usize;
            app.phase[ch] = step_phase(app.phase[ch], up);
        }
    } else {
        if app.focus == 1 {
            app.hstx_phase = step_phase(app.hstx_phase, up);
        } else {
            app.adc_phase = step_phase(app.adc_phase, up);
        }
    }
}
pub fn pwm_lines(app: &App) -> Vec<Line<'_>> {
    {
        let mut lines: Vec<Line> = vec![Line::from("ch  f/Hz  amp  phase")];
        for ch in 0..4 {
            lines.push(Line::from(format!(
                "ch{} {:>5} {:>4} {:>3}{}",
                ch,
                app.freq[ch],
                app.amp[ch],
                app.phase[ch],
                if ch == app.pwm_ch as usize { " <" } else { "" }
            )))
        }
        lines
    }
}
pub fn draw(terminal: &mut ratatui::DefaultTerminal, app: &App) -> std::io::Result<()> {
    terminal.draw(|frame| {
        let layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(8),
                Constraint::Length(10),
                Constraint::Min(8),
            ]);
        frame.render_widget(
            Paragraph::block(
                Paragraph::new(pwm_lines(app)),
                Block::bordered().title("PWM sine"),
            ),
            layout.split(frame.area())[0],
        );
        frame.render_widget(
            Paragraph::block(
                Paragraph::new(vec![
                    Line::from(format!(
                        "HSTX f={}Hz a={} p={}",
                        app.hstx_freq, app.hstx_amp, app.hstx_phase
                    )),
                    Line::from(format!(
                        "ADC rate={}Hz phase={}",
                        app.adc_rate, app.adc_phase
                    )),
                    Line::from(format!("cap={} blk={} (c/v)", app.cap_sel, app.blk_ch)),
                ]),
                Block::bordered().title("HSTX/ADC"),
            ),
            layout.split(frame.area())[1],
        );
        frame.render_widget(
            Paragraph::block(
                Paragraph::new(vec![
                    Line::from(format!(
                        "temp={}x0.1C cap={} seq={} focus={}",
                        app.temp_c10,
                        app.cap,
                        app.seq,
                        focus_name(app.focus)
                    )),
                    Line::from(format!(
                        "block n={} first={}",
                        app.block.len(),
                        if app.block.len() == 0 {
                            0
                        } else {
                            app.block[0]
                        }
                    )),
                    Line::from("q quit|Tab focus|1-4 ch|-/= f|[/] a|;/' p"),
                    Line::from("s send|c cap|v blkch|b read"),
                ]),
                Block::bordered().title("Live"),
            ),
            layout.split(frame.area())[2],
        )
    })?;
    Ok(())
}
pub fn run(terminal: &mut ratatui::DefaultTerminal) -> std::io::Result<()> {
    {
        let args: Vec<String> = std::env::args().collect();
        let mut port_name: String = "/dev/ttyUSB0".to_string();
        let mut baud: u32 = 115200;
        let mut demo: bool = false;
        let mut i: usize = 1;
        while i < args.len() {
            if args[i] == "--port" {
                i += 1;
                port_name = args[i].clone();
            } else {
                if args[i] == "--baud" {
                    i += 1;
                    baud = args[i].parse().unwrap_or(115200);
                } else {
                    if args[i] == "--demo" {
                        demo = true;
                    }
                }
            }
            i += 1
        }
        {
            let mut port: Option<Box<dyn SerialPort>> = None;
            if !demo {
                match serialport::new(port_name, baud)
                    .timeout(Duration::from_millis(50))
                    .open()
                {
                    Ok(p) => {
                        port = Some(p);
                    }
                    Err(e) => {
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::Other,
                            e.to_string(),
                        ));
                    }
                }
            }
            {
                let mut app = app_new();
                let mut dec = proto::Decoder::new();
                let mut last = Instant::now();
                while !app.quit {
                    if event::poll(Duration::from_millis(50))? {
                        match event::read()? {
                            Event::Key(key) => {
                                if key.code == KeyCode::Char('q') {
                                    app.quit = true;
                                }
                                if key.code == KeyCode::Tab {
                                    app.focus = (app.focus + 1) % 3;
                                }
                                if key.code == KeyCode::Char('1') {
                                    app.pwm_ch = 0;
                                }
                                if key.code == KeyCode::Char('2') {
                                    app.pwm_ch = 1;
                                }
                                if key.code == KeyCode::Char('3') {
                                    app.pwm_ch = 2;
                                }
                                if key.code == KeyCode::Char('4') {
                                    app.pwm_ch = 3;
                                }
                                if key.code == KeyCode::Char('-') {
                                    adjust_freq_focused(&mut app, false)
                                }
                                if key.code == KeyCode::Char('=') {
                                    adjust_freq_focused(&mut app, true)
                                }
                                if key.code == KeyCode::Char('[') {
                                    adjust_amp_focused(&mut app, false)
                                }
                                if key.code == KeyCode::Char(']') {
                                    adjust_amp_focused(&mut app, true)
                                }
                                if key.code == KeyCode::Char(';') {
                                    adjust_phase_focused(&mut app, false)
                                }
                                if key.code == KeyCode::Char('\'') {
                                    adjust_phase_focused(&mut app, true)
                                }
                                if key.code == KeyCode::Char('s') {
                                    send_focused(&mut port, &app)
                                }
                                if key.code == KeyCode::Char('c') {
                                    app.cap_sel = (app.cap_sel + 1) % 4;
                                    send_cap(&mut port, app.cap_sel);
                                }
                                if key.code == KeyCode::Char('v') {
                                    app.blk_ch = (app.blk_ch + 1) % 3;
                                }
                                if key.code == KeyCode::Char('b') {
                                    send_readblock(&mut port, app.blk_ch, 128)
                                }
                            }
                            _ => {}
                        }
                    }
                    pump_serial(&mut port, &mut dec, &mut app);
                    if demo {
                        if 500 < last.elapsed().as_millis() as u64 {
                            app.temp_c10 = app.temp_c10 + 1;
                            app.cap = app.cap + 37;
                            app.seq = app.seq.wrapping_add(1);
                            last = Instant::now();
                        }
                    }
                    draw(terminal, &app)?
                }
                Ok(())
            }
        }
    }
}
fn main() {
    {
        let mut terminal = ratatui::init();
        run(&mut terminal).unwrap();
        ratatui::restore()
    }
}
