#![cfg_attr(not(test), no_std)]
//! pico-link: framed UART protocol shared by Pico 2 firmware and Linux host.
//! Frame: [0xAA][LEN][PAYLOAD..][CRC lo][CRC hi], CRC16-CCITT over LEN+PAYLOAD.
pub const FRAME_MAGIC: u8 = 0xAA;
pub const MAX_PAYLOAD: usize = 64;
pub const FRAME_OVERHEAD: usize = 4;
pub const TAG_SET_PWM: u8 = 1;
pub const TAG_SET_HSTX: u8 = 2;
pub const TAG_SET_ADC: u8 = 3;
pub const TAG_CAP_SELECT: u8 = 4;
pub const TAG_READ_BLOCK: u8 = 5;
pub const TAG_STATUS: u8 = 16;
pub const TAG_ADC_BLOCK: u8 = 17;
pub const TAG_ACK: u8 = 18;
pub fn crc16_part(data: &[u8], start: usize, len: usize) -> u16 {
    {
        let mut crc: u16 = 0xFFFF;
        let mut i: usize = 0;
        while i < len {
            crc = crc ^ data[start + i] as u16 * 256;
            for _ in 0..8 {
                if crc & 0x8000 != 0 {
                    crc = crc << 1 ^ 0x1021;
                } else {
                    crc = crc << 1;
                }
            }
            i += 1
        }
        crc
    }
}
pub fn crc16_ccitt(data: &[u8]) -> u16 {
    crc16_part(data, 0, data.len())
}
pub fn encode_frame(payload: &[u8], out: &mut [u8]) -> Option<usize> {
    {
        let n: usize = payload.len();
        if n == 0 || MAX_PAYLOAD < n {
            return None;
        }
        {
            let total: usize = n + FRAME_OVERHEAD;
            if out.len() < total {
                return None;
            }
            out[0] = FRAME_MAGIC;
            out[1] = n as u8;
            {
                let mut i: usize = 0;
                while i < n {
                    out[i + 2] = payload[i];
                    i += 1
                }
            }
            {
                let crc: u16 = crc16_part(out, 1, n + 1);
                out[n + 2] = (crc & 0xff) as u8;
                out[n + 3] = (crc >> 8) as u8;
            }
            return Some(total);
        }
    }
}
pub fn put_u16_le(o: &mut [u8], i: usize, v: u16) {
    o[i] = (v & 0xff) as u8;
    o[i + 1] = (v >> 8) as u8;
}
pub fn get_u16_le(d: &[u8], i: usize) -> u16 {
    d[i] as u16 | d[i + 1] as u16 * 256
}
pub fn put_u32_le(o: &mut [u8], i: usize, v: u32) {
    o[i] = (v & 0xff) as u8;
    o[i + 1] = (v >> 8 & 0xff) as u8;
    o[i + 2] = (v >> 16 & 0xff) as u8;
    o[i + 3] = (v >> 24) as u8;
}
pub fn get_u32_le(d: &[u8], i: usize) -> u32 {
    d[i] as u32 | d[i + 1] as u32 * 256 | d[i + 2] as u32 * 65536 | d[i + 3] as u32 * 16777216
}
pub struct Decoder {
    buf: [u8; 70],
    pos: usize,
    frame_len: usize,
    errors: u32,
}
impl Decoder {
    pub fn new() -> Decoder {
        Decoder {
            buf: [0; 70],
            pos: 0,
            frame_len: 0,
            errors: 0,
        }
    }
    pub fn reset(&mut self) {
        self.pos = 0;
        self.frame_len = 0;
    }
    pub fn error_count(&self) -> u32 {
        self.errors
    }
    pub fn payload_len(&self) -> usize {
        self.frame_len
    }
    pub fn payload_byte(&self, i: usize) -> u8 {
        self.buf[i]
    }
    pub fn push(&mut self, byte: u8) -> Option<usize> {
        if self.pos == 0 {
            if byte != FRAME_MAGIC {
                self.errors += 1;
                return None;
            }
            self.buf[0] = byte;
            self.pos = 1;
            return None;
        }
        self.buf[self.pos] = byte;
        self.pos += 1;
        if self.pos == 2 {
            {
                let n: usize = self.buf[1] as usize;
                if n == 0 || MAX_PAYLOAD < n {
                    self.pos = 0;
                    self.errors += 1;
                    return None;
                }
            }
        }
        {
            let n: usize = self.buf[1] as usize;
            if self.pos < n + FRAME_OVERHEAD {
                return None;
            }
            {
                let crc: u16 = crc16_part(&self.buf, 1, n + 1);
                let got: u16 = self.buf[n + 2] as u16 | self.buf[n + 3] as u16 * 256;
                if crc != got {
                    self.pos = 0;
                    self.errors += 1;
                    return None;
                }
                {
                    let mut i: usize = 0;
                    while i < n {
                        self.buf[i] = self.buf[i + 2];
                        i += 1
                    }
                }
                self.frame_len = n;
                self.pos = 0;
                return Some(n);
            }
        }
    }
}
pub struct PwmCmd {
    pub ch: u8,
    pub freq_hz: u32,
    pub amp_tenth_pct: u16,
    pub phase_deg: u16,
}
pub fn encode_pwm_cmd(cmd: &PwmCmd, out: &mut [u8; 10]) {
    out[0] = TAG_SET_PWM;
    out[1] = cmd.ch;
    put_u32_le(out, 2, cmd.freq_hz);
    put_u16_le(out, 6, cmd.amp_tenth_pct);
    put_u16_le(out, 8, cmd.phase_deg)
}
pub fn decode_pwm_cmd(p: &[u8]) -> Option<PwmCmd> {
    if p.len() != 10 || p[0] != TAG_SET_PWM {
        return None;
    }
    return Some(PwmCmd {
        ch: p[1],
        freq_hz: get_u32_le(p, 2),
        amp_tenth_pct: get_u16_le(p, 6),
        phase_deg: get_u16_le(p, 8),
    });
}
pub struct HstxCmd {
    pub freq_hz: u32,
    pub amp_tenth_pct: u16,
    pub phase_deg: u16,
}
pub fn encode_hstx_cmd(cmd: &HstxCmd, out: &mut [u8; 9]) {
    out[0] = TAG_SET_HSTX;
    put_u32_le(out, 1, cmd.freq_hz);
    put_u16_le(out, 5, cmd.amp_tenth_pct);
    put_u16_le(out, 7, cmd.phase_deg)
}
pub fn decode_hstx_cmd(p: &[u8]) -> Option<HstxCmd> {
    if p.len() != 9 || p[0] != TAG_SET_HSTX {
        return None;
    }
    return Some(HstxCmd {
        freq_hz: get_u32_le(p, 1),
        amp_tenth_pct: get_u16_le(p, 5),
        phase_deg: get_u16_le(p, 7),
    });
}
pub struct AdcCmd {
    pub rate_hz: u32,
    pub phase_deg: u16,
}
pub fn encode_adc_cmd(cmd: &AdcCmd, out: &mut [u8; 7]) {
    out[0] = TAG_SET_ADC;
    put_u32_le(out, 1, cmd.rate_hz);
    put_u16_le(out, 5, cmd.phase_deg)
}
pub fn decode_adc_cmd(p: &[u8]) -> Option<AdcCmd> {
    if p.len() != 7 || p[0] != TAG_SET_ADC {
        return None;
    }
    return Some(AdcCmd {
        rate_hz: get_u32_le(p, 1),
        phase_deg: get_u16_le(p, 5),
    });
}
pub struct StatusMsg {
    pub seq: u8,
    pub temp_c10: i16,
    pub cap: u32,
    pub flags: u8,
}
pub fn encode_status(msg: &StatusMsg, out: &mut [u8; 9]) {
    out[0] = TAG_STATUS;
    out[1] = msg.seq;
    put_u16_le(out, 2, msg.temp_c10 as u16);
    put_u32_le(out, 4, msg.cap);
    out[8] = msg.flags;
}
pub fn decode_status(p: &[u8]) -> Option<StatusMsg> {
    if p.len() != 9 || p[0] != TAG_STATUS {
        return None;
    }
    return Some(StatusMsg {
        seq: p[1],
        temp_c10: get_u16_le(p, 2) as i16,
        cap: get_u32_le(p, 4),
        flags: p[8],
    });
}
pub fn block_sample(p: &[u8], i: usize) -> u16 {
    get_u16_le(p, 3 + i * 2)
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn crc_known_vector() {
        {
            let v = b"123456789";
            assert_eq!(0x29B1, crc16_ccitt(v))
        }
    }
    #[test]
    fn frame_round_trip() {
        {
            let payload: [u8; 3] = [1, 2, 3];
            let mut out: [u8; 16] = [0; 16];
            let Some(n) = encode_frame(&payload, &mut out) else {
                return;
            };
            assert_eq!(n, 7);
            {
                let mut dec = Decoder::new();
                let mut got: Option<usize> = None;
                for i in 0..n {
                    got = dec.push(out[i]);
                }
                let Some(m) = got else { return };
                assert_eq!(m, 3);
                assert_eq!(1, dec.payload_byte(0));
                assert_eq!(3, dec.payload_byte(2))
            }
        }
    }
    #[test]
    fn frame_rejects_bad_crc() {
        {
            let payload: [u8; 2] = [9, 9];
            let mut out: [u8; 16] = [0; 16];
            let Some(n) = encode_frame(&payload, &mut out) else {
                return;
            };
            out[3] = out[3] ^ 0xff;
            {
                let mut dec = Decoder::new();
                for i in 0..n {
                    {
                        let _ = dec.push(out[i]);
                    }
                }
                assert_eq!(1, dec.error_count())
            }
        }
    }
    #[test]
    fn frame_rejects_garbage() {
        {
            let mut dec = Decoder::new();
            dec.push(0);
            dec.push(1);
            assert_eq!(2, dec.error_count())
        }
    }
    #[test]
    fn frame_rejects_empty_and_oversize() {
        {
            let empty: [u8; 0] = [0; 0];
            let mut out: [u8; 80] = [0; 80];
            match encode_frame(&empty, &mut out) {
                Some(_) => return,
                None => {}
            }
            {
                let big: [u8; 65] = [7; 65];
                match encode_frame(&big, &mut out) {
                    Some(_) => return,
                    None => {}
                }
            }
        }
    }
    #[test]
    fn pwm_cmd_round_trip() {
        {
            let cmd = PwmCmd {
                ch: 3,
                freq_hz: 440,
                amp_tenth_pct: 800,
                phase_deg: 90,
            };
            let mut wire: [u8; 10] = [0; 10];
            encode_pwm_cmd(&cmd, &mut wire);
            let Some(back) = decode_pwm_cmd(&wire) else {
                return;
            };
            assert_eq!(3, back.ch);
            assert_eq!(440, back.freq_hz);
            assert_eq!(800, back.amp_tenth_pct);
            assert_eq!(90, back.phase_deg)
        }
    }
    #[test]
    fn status_negative_temp() {
        {
            let msg = StatusMsg {
                seq: 7,
                temp_c10: (-55),
                cap: 123456,
                flags: 1,
            };
            let mut wire: [u8; 9] = [0; 9];
            encode_status(&msg, &mut wire);
            let Some(back) = decode_status(&wire) else {
                return;
            };
            assert_eq!(7, back.seq);
            assert_eq!((-55), back.temp_c10);
            assert_eq!(123456, back.cap)
        }
    }
}
