//! 03c — Range-Decoder (Decode-Subset von `opus-pure` range_coder.rs).
//!
//! Grundlage fuer CELT-Header-Walk und Allokation: Entropie-Reads vom
//! Paket-Anfang, Roh-Bits vom Paket-Ende. Bit-exakt zum Original.

const EC_SYM_BITS: u32 = 8;
const EC_SYM_MAX: u32 = 255;
const EC_CODE_TOP: u32 = 1 << 31;
const EC_CODE_BOT: u32 = EC_CODE_TOP >> 8;
const EC_CODE_EXTRA: u32 = 7;
pub(crate) const BITRES: i32 = 3;

/// Bare range decoder over borrowed bytes; raw bits come from the packet end.
pub(crate) struct RangeDec<'a> {
    pub(crate) buf: &'a [u8],
    pub(crate) storage: u32,
    pub(crate) end_offs: u32,
    pub(crate) end_window: u32,
    pub(crate) nend_bits: i32,
    pub(crate) nbits_total: i32,
    pub(crate) offs: u32,
    pub(crate) rng: u32,
    pub(crate) val: u32,
    pub(crate) ext: u32,
    pub(crate) rem: i32,
}

impl<'a> RangeDec<'a> {
    pub(crate) fn new(data: &'a [u8]) -> Self {
        let mut rc = Self {
            buf: data,
            storage: data.len() as u32,
            end_offs: 0,
            end_window: 0,
            nend_bits: 0,
            // 33 - ((32-7)/8)*8 = 9 (libopus ec_dec_init).
            nbits_total: 9,
            offs: 0,
            rng: 1 << EC_CODE_EXTRA,
            val: 0,
            ext: 0,
            rem: 0,
        };
        rc.rem = rc.read_byte() as i32;
        rc.val = rc
            .rng
            .wrapping_sub(1)
            .wrapping_sub(rc.rem as u32 >> (EC_SYM_BITS - EC_CODE_EXTRA));
        rc.normalize();
        rc
    }

    fn normalize(&mut self) {
        let mut guard = 0u32;
        while self.rng <= EC_CODE_BOT {
            guard += 1;
            if guard > 100 {
                self.rng = EC_CODE_BOT + 1;
                break;
            }
            self.nbits_total += EC_SYM_BITS as i32;
            self.rng <<= EC_SYM_BITS;
            let sym = self.rem;
            self.rem = self.read_byte() as i32;
            let combined = ((sym << EC_SYM_BITS) | self.rem) >> (EC_SYM_BITS - EC_CODE_EXTRA);
            self.val = (self.val << EC_SYM_BITS).wrapping_add(EC_SYM_MAX & !combined as u32)
                & (EC_CODE_TOP - 1);
        }
    }

    fn read_byte(&mut self) -> u8 {
        if self.offs < self.storage {
            let b = self.buf[self.offs as usize];
            self.offs += 1;
            b
        } else {
            0
        }
    }

    pub(crate) fn dec_uint(&mut self, ft: u32) -> u32 {
        if ft <= 1 {
            return 0;
        }
        let ftb = 32 - (ft - 1).leading_zeros() as i32;
        if ftb > 8 {
            let s = (ftb - 8) as u32;
            let ft = ((ft - 1) >> s) + 1;
            let fs = self.decode(ft);
            self.update(fs, fs.wrapping_add(1), ft);
            (fs << s) | self.dec_bits(s)
        } else {
            let fs = self.decode(ft);
            self.update(fs, fs.wrapping_add(1), ft);
            fs
        }
    }

    pub(crate) fn dec_bits(&mut self, bits: u32) -> u32 {
        if bits == 0 {
            return 0;
        }
        let mut window = self.end_window;
        let mut used = self.nend_bits;
        if used < bits as i32 {
            loop {
                let byte = if self.end_offs < self.storage {
                    self.end_offs += 1;
                    self.buf[(self.storage - self.end_offs) as usize]
                } else {
                    0
                };
                window |= (byte as u32) << used;
                used += 8;
                if used > 32 - 8 {
                    break;
                }
            }
        }
        let ret = window & ((1 << bits) - 1);
        self.end_window = window >> bits;
        self.nend_bits = used - bits as i32;
        self.nbits_total += bits as i32;
        ret
    }

    pub(crate) fn tell_frac(&self) -> i32 {
        static CORR: [u32; 8] = [35733, 38967, 42495, 46340, 50535, 55109, 60097, 65535];
        let nbits = self.nbits_total << BITRES;
        let l = 32 - self.rng.leading_zeros() as i32;
        let r = self.rng >> (l - 16);
        let b = (r >> 12).wrapping_sub(8);
        let b = b + (r > CORR[(b & 7) as usize]) as u32;
        nbits - (l << 3) - b as i32
    }

    pub(crate) fn tell(&self) -> i32 {
        self.nbits_total - (32 - self.rng.leading_zeros() as i32)
    }

    pub(crate) fn decode_bit_logp(&mut self, logp: u32) -> bool {
        let s = self.rng >> logp;
        let ret = self.val < s;
        if !ret {
            self.val = self.val.wrapping_sub(s);
            self.rng = self.rng.wrapping_sub(s);
        } else {
            self.rng = s;
        }
        self.normalize();
        ret
    }

    pub(crate) fn decode_icdf(&mut self, icdf: &[u8], ftb: u32) -> i32 {
        let mut s = self.rng;
        let d = self.val;
        let r = s >> ftb;
        let mut ret = 0;
        let mut t;
        loop {
            t = s;
            s = r.wrapping_mul(icdf[ret] as u32);
            ret += 1;
            if d >= s {
                break;
            }
        }
        self.val = d.wrapping_sub(s);
        self.rng = t.wrapping_sub(s);
        self.normalize();
        (ret - 1) as i32
    }

    pub(crate) fn decode(&mut self, ft: u32) -> u32 {
        let r = self.rng / ft;
        self.ext = r;
        let s = self.val / r;
        ft - ft.min(s.wrapping_add(1))
    }

    fn update(&mut self, fl: u32, fh: u32, ft: u32) {
        let s = self.ext.wrapping_mul(ft.wrapping_sub(fh));
        self.val = self.val.wrapping_sub(s);
        self.rng = if fl > 0 {
            self.ext.wrapping_mul(fh.wrapping_sub(fl))
        } else {
            self.rng.wrapping_sub(s)
        };
        self.normalize();
    }

    fn laplace_get_freq1(fs0: u32, decay: i32) -> u32 {
        let ft = 32768 - 32 - fs0;
        ((ft as i32 * (16384 - decay)) >> 15) as u32
    }

    pub(crate) fn laplace_decode(&mut self, fs: u32, decay: i32) -> i32 {
        let fm = self.decode(1 << 15);
        let mut fl = 0;
        let mut fs_val = fs;
        let mut val = 0;
        if fm >= fs_val {
            val += 1;
            fl = fs_val;
            fs_val = Self::laplace_get_freq1(fs_val, decay) + 1;
            while fs_val > 1 && fm >= fl + 2 * fs_val {
                fs_val *= 2;
                fl += fs_val;
                fs_val = (((fs_val as i32 - 2) * decay) >> 15) as u32 + 1;
                val += 1;
            }
            if fs_val <= 1 {
                let di = (fm - fl) >> 1;
                val += di as i32;
                fl += 2 * di;
            }
            if fm < fl + fs_val {
                val = -val;
            } else {
                fl += fs_val;
            }
        }
        self.update(fl, fl.wrapping_add(fs_val.min(32768 - fl)), 1 << 15);
        val
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tell_is_one_after_init() {
        // Jede Frame-Synchronisation haengt daran: nach dem Init muss
        // `tell() == 1` gelten (sonst wird das Silence-Bit uebersprungen).
        for len in [0, 1, 5, 64, 1275] {
            let v = vec![0xABu8; len];
            let rc = RangeDec::new(&v);
            assert_eq!(rc.tell(), 1, "len {len}");
        }
    }

    #[test]
    fn laplace_golden_byte() {
        // Golden check from opus-pure's own test: decoding byte 0xE0 with
        // fs=100<<7, decay=120<<6 must yield -3 (decoder-only half).
        let mut rc = RangeDec::new(&[224]);
        assert_eq!(rc.laplace_decode(100 << 7, 120 << 6), -3);
    }
}
