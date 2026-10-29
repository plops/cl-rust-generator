//! Sentinel-1 Level-0 Space-Packet-Header, Rekonstruktionstabellen und
//! mehrsträngiger FDBAQ/BAQ/Bypass-Decoder (`std::thread::scope`).
//!
//! Beinhaltet das korrigierte Bit-Layout für Bytes 60..66 gemäß ESA
//! Sentinel-1 SAR Space Packet Protocol Data Unit (S1-IF-ASD-PL-0007):
//! - Byte 60..61: `elevation_beam_address` (4 Bit), `azimuth_beam_address` (10 Bit)
//! - Byte 62: `calibration_mode` (Bits 7..6), `tx_pulse_number` (Bits 4..0)
//! - Byte 63: `signal_type` (Bits 7..4), `swap_flag` (Bit 0)
//! - Byte 64: `swath_number` (`1..6` = SM, `10..12` = IW1..IW3, `20..24` = EW1..EW5)
//! - Byte 65..66: `number_of_quads` (u16 Big-Endian)

use crate::types::{Complex32, Error};

pub const HEADER_LEN: usize = 68;
pub const SYNC_MARKER: u32 = 0x352E_F853;
pub const SYMBOLS_PER_BLOCK: usize = 128;

pub const B0: [f32; 4] = [3.0, 3.0, 3.160, 3.530];
pub const B1: [f32; 4] = [4.0, 4.0, 4.080, 4.370];
pub const B2: [f32; 6] = [6.0, 6.0, 6.0, 6.150, 6.50, 6.880];
pub const B3: [f32; 7] = [9.0, 9.0, 9.0, 9.0, 9.360, 9.50, 10.10];
pub const B4: [f32; 9] = [15.0, 15.0, 15.0, 15.0, 15.0, 15.0, 15.220, 15.50, 16.050];

pub const NRL0: [f32; 4] = [0.36370, 1.09150, 1.82080, 2.64060];
pub const NRL1: [f32; 5] = [0.30420, 0.91270, 1.52160, 2.13130, 2.84260];
pub const NRL2: [f32; 7] = [0.23050, 0.69160, 1.15280, 1.6140, 2.07540, 2.53690, 3.11910];
pub const NRL3: [f32; 10] = [
    0.17020, 0.51070, 0.85110, 1.19160, 1.53210, 1.87260, 2.21310, 2.55360, 2.89420, 3.37440,
];
pub const NRL4: [f32; 16] = [
    0.1130, 0.33890, 0.56490, 0.79080, 1.01670, 1.24280, 1.46870, 1.69470, 1.92060, 2.14660,
    2.37250, 2.59850, 2.82440, 3.05040, 3.27640, 3.66230,
];

pub const A3: [f32; 4] = [3.0, 3.0, 3.120, 3.550];
pub const A4: [f32; 6] = [7.0, 7.0, 7.0, 7.170, 7.40, 7.760];
pub const A5: [f32; 11] = [
    15.0, 15.0, 15.0, 15.0, 15.0, 15.0, 15.440, 15.560, 16.110, 16.380, 16.650,
];

pub const NRLA3: [f32; 4] = [0.2490, 0.76810, 1.36550, 2.18640];
pub const NRLA4: [f32; 8] = [
    0.1290, 0.390, 0.66010, 0.94710, 1.26230, 1.62610, 2.07930, 2.74670,
];
pub const NRLA5: [f32; 16] = [
    6.60e-2, 0.19850, 0.3320, 0.46770, 0.60610, 0.74870, 0.89640, 1.0510, 1.21430, 1.38960, 1.580,
    1.79140, 2.03290, 2.32340, 2.69710, 3.26920,
];

pub const SF: [f32; 256] = [
    0.0, 0.630, 1.250, 1.880, 2.510, 3.130, 3.760, 4.390, 5.010, 5.640, 6.270, 6.890, 7.520, 8.150,
    8.770, 9.40, 10.030, 10.650, 11.280, 11.910, 12.530, 13.160, 13.790, 14.410, 15.040, 15.670,
    16.290, 16.920, 17.550, 18.170, 18.80, 19.430, 20.050, 20.680, 21.310, 21.930, 22.560, 23.190,
    23.810, 24.440, 25.070, 25.690, 26.320, 26.950, 27.570, 28.20, 28.830, 29.450, 30.080, 30.710,
    31.330, 31.960, 32.590, 33.210, 33.840, 34.470, 35.090, 35.720, 36.350, 36.970, 37.60, 38.230,
    38.850, 39.480, 40.110, 40.730, 41.360, 41.990, 42.610, 43.240, 43.870, 44.490, 45.120, 45.750,
    46.370, 47.0, 47.630, 48.250, 48.880, 49.510, 50.130, 50.760, 51.390, 52.010, 52.640, 53.270,
    53.890, 54.520, 55.150, 55.770, 56.40, 57.030, 57.650, 58.280, 58.910, 59.530, 60.160, 60.790,
    61.410, 62.040, 62.980, 64.240, 65.490, 66.740, 68.0, 69.250, 70.50, 71.760, 73.010, 74.260,
    75.520, 76.770, 78.020, 79.280, 80.530, 81.780, 83.040, 84.290, 85.540, 86.80, 88.050, 89.30,
    90.560, 91.810, 93.060, 94.320, 95.570, 96.820, 98.080, 99.330, 100.580, 101.840, 103.090,
    104.340, 105.60, 106.850, 108.10, 109.350, 110.610, 111.860, 113.110, 114.370, 115.620,
    116.870, 118.130, 119.380, 120.630, 121.890, 123.140, 124.390, 125.650, 126.90, 128.150,
    129.410, 130.660, 131.910, 133.170, 134.420, 135.670, 136.930, 138.180, 139.430, 140.690,
    141.940, 143.190, 144.450, 145.70, 146.950, 148.210, 149.460, 150.710, 151.970, 153.220,
    154.470, 155.730, 156.980, 158.230, 159.490, 160.740, 161.990, 163.250, 164.50, 165.750,
    167.010, 168.260, 169.510, 170.770, 172.020, 173.270, 174.530, 175.780, 177.030, 178.290,
    179.540, 180.790, 182.050, 183.30, 184.550, 185.810, 187.060, 188.310, 189.570, 190.820,
    192.070, 193.330, 194.580, 195.830, 197.090, 198.340, 199.590, 200.850, 202.10, 203.350,
    204.610, 205.860, 207.110, 208.370, 209.620, 210.870, 212.130, 213.380, 214.630, 215.890,
    217.140, 218.390, 219.650, 220.90, 222.150, 223.410, 224.660, 225.910, 227.170, 228.420,
    229.670, 230.930, 232.180, 233.430, 234.690, 235.940, 237.190, 238.450, 239.70, 240.950,
    242.210, 243.460, 244.710, 245.970, 247.220, 248.470, 249.730, 250.980, 252.230, 253.490,
    254.740, 255.990, 255.990,
];

/// Geparster 68-Byte Sentinel-1 Space-Packet-Header plus Datei-Offset.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PacketRecord {
    pub offset: usize,
    pub packet_data_len: u16,
    pub coarse_time: u32,
    pub fine_time: u16,
    pub sync_marker: u32,
    pub data_take_id: u32,
    pub ecc_number: u8,
    pub rx_channel_id: u8,
    pub instrument_config_id: u32,
    pub subcom_index: u8,
    pub subcom_word: u16,
    pub space_packet_count: u32,
    pub pri_count: u32,
    pub error_flag: bool,
    pub baq_mode: u8,
    pub baq_block_len: u8,
    pub rgdec: u8,
    pub rx_gain_code: u8,
    /// Roh-16-Bit Sign-Magnitude (SM-T1: Bit 15 = Vorzeichen, Bits 14..0 = Betrag).
    pub txprr_code: u16,
    /// Roh-16-Bit Sign-Magnitude (SM-T1: Bit 15 = Vorzeichen, Bits 14..0 = Betrag).
    pub txpsf_code: u16,
    pub txpl_code: u32,
    pub rank: u8,
    pub pri_code: u32,
    pub swst_code: u32,
    pub swl_code: u32,
    pub ssb_flag: bool,
    pub polarisation: u8,
    pub temp_comp: u8,
    pub elevation_beam_address: u8,
    pub azimuth_beam_address: u16,
    pub calibration_mode: u8,
    pub tx_pulse_number: u8,
    pub signal_type: u8,
    pub swap_flag: bool,
    pub swath_number: u8,
    pub number_of_quads: u16,
}

impl PacketRecord {
    /// Geparsten Header aus einem 68-Byte-Slice + Datei-Offset erzeugen.
    pub fn parse(h: &[u8], offset: usize) -> Result<Self, Error> {
        if h.len() < HEADER_LEN {
            return Err(Error(format!("Header bei Offset {offset} zu kurz")));
        }
        let packet_data_len = u16::from_be_bytes([h[4], h[5]]) + 1;
        let coarse_time = u32::from_be_bytes([h[6], h[7], h[8], h[9]]);
        let fine_time = u16::from_be_bytes([h[10], h[11]]);
        let sync_marker = u32::from_be_bytes([h[12], h[13], h[14], h[15]]);
        if sync_marker != SYNC_MARKER {
            return Err(Error(format!(
                "Ungültiger Sync-Marker 0x{sync_marker:08X} bei Offset {offset}"
            )));
        }
        let data_take_id = u32::from_be_bytes([h[16], h[17], h[18], h[19]]);
        let ecc_number = h[20];
        let rx_channel_id = h[21] & 0x0F;
        let instrument_config_id = u32::from_be_bytes([h[22], h[23], h[24], h[25]]);
        let subcom_index = h[26];
        let subcom_word = u16::from_be_bytes([h[27], h[28]]);
        let space_packet_count = u32::from_be_bytes([h[29], h[30], h[31], h[32]]);
        let pri_count = u32::from_be_bytes([h[33], h[34], h[35], h[36]]);
        let error_flag = (h[37] >> 7) != 0;
        let baq_mode = h[37] & 0x1F;
        let baq_block_len = h[38];
        let rgdec = h[40];
        let rx_gain_code = h[41];
        let txprr_code = u16::from_be_bytes([h[42], h[43]]);
        let txpsf_code = u16::from_be_bytes([h[44], h[45]]);
        let txpl_code = (u32::from(h[46]) << 16) | (u32::from(h[47]) << 8) | u32::from(h[48]);
        let rank = h[49] & 0x1F;
        let pri_code = (u32::from(h[50]) << 16) | (u32::from(h[51]) << 8) | u32::from(h[52]);
        let swst_code = (u32::from(h[53]) << 16) | (u32::from(h[54]) << 8) | u32::from(h[55]);
        let swl_code = (u32::from(h[56]) << 16) | (u32::from(h[57]) << 8) | u32::from(h[58]);
        let ssb_flag = (h[59] >> 7) != 0;
        let polarisation = (h[59] >> 4) & 0x07;
        let temp_comp = (h[59] >> 2) & 0x03;
        let elevation_beam_address = (h[60] >> 4) & 0x0F;
        let azimuth_beam_address = (u16::from(h[60] & 0x03) << 8) | u16::from(h[61]);
        let calibration_mode = (h[62] >> 6) & 0x03;
        let tx_pulse_number = h[62] & 0x1F;
        let signal_type = (h[63] >> 4) & 0x0F;
        let swap_flag = (h[63] & 0x01) != 0;
        let swath_number = h[64];
        let number_of_quads = u16::from_be_bytes([h[65], h[66]]);

        Ok(Self {
            offset,
            packet_data_len,
            coarse_time,
            fine_time,
            sync_marker,
            data_take_id,
            ecc_number,
            rx_channel_id,
            instrument_config_id,
            subcom_index,
            subcom_word,
            space_packet_count,
            pri_count,
            error_flag,
            baq_mode,
            baq_block_len,
            rgdec,
            rx_gain_code,
            txprr_code,
            txpsf_code,
            txpl_code,
            rank,
            pri_code,
            swst_code,
            swl_code,
            ssb_flag,
            polarisation,
            temp_comp,
            elevation_beam_address,
            azimuth_beam_address,
            calibration_mode,
            tx_pulse_number,
            signal_type,
            swap_flag,
            swath_number,
            number_of_quads,
        })
    }

    /// GPS-Sekundenzeitstempel `coarse + (fine + 0.5) * 2^-16`.
    #[inline]
    pub fn timestamp_s(&self) -> f64 {
        f64::from(self.coarse_time) + (f64::from(self.fine_time) + 0.5) / 65536.0
    }

    /// Ist dies ein nominales Radar-Echo-Paket (kein Noise, keine Kalibration)?
    #[inline]
    pub fn is_nominal_echo(&self) -> bool {
        self.signal_type == 0
            && self.calibration_mode == 0
            && !self.error_flag
            && matches!(self.baq_mode, 0 | 3 | 4 | 5 | 12 | 13 | 14)
    }

    /// Anzahl komplexer Range-Samples `2 * number_of_quads`.
    #[inline]
    pub fn num_samples(&self) -> usize {
        2 * usize::from(self.number_of_quads)
    }
}

/// Scannt alle Space-Packet-Header einer gemappten Level-0 `.dat`-Datei.
pub fn scan_packet_headers(bytes: &[u8]) -> Result<Vec<PacketRecord>, Error> {
    let mut records = Vec::with_capacity(bytes.len() / 12000 + 64);
    let mut pos = 0usize;
    while pos + HEADER_LEN <= bytes.len() {
        let rec = PacketRecord::parse(&bytes[pos..pos + HEADER_LEN], pos)?;
        let next = pos + 6 + usize::from(rec.packet_data_len);
        if next > bytes.len() {
            break;
        }
        records.push(rec);
        pos = next;
    }
    if records.is_empty() {
        return Err(Error(
            "Keine gültigen Space-Packets in Datei gefunden".into(),
        ));
    }
    Ok(records)
}

#[derive(Clone, Copy)]
struct ReconParams {
    simple_limit: u8,
    max_mcode: u32,
    simple: &'static [f32],
    nrl: &'static [f32],
}

#[inline]
fn reconstruct(params: ReconParams, thidx: u8, mcode: u32, symbol_sign: f32) -> Result<f32, Error> {
    if thidx <= params.simple_limit {
        if mcode < params.max_mcode {
            Ok(symbol_sign * mcode as f32)
        } else if mcode == params.max_mcode {
            let b = *params
                .simple
                .get(usize::from(thidx))
                .ok_or_else(|| Error("Ungültiger simple-Tabellenindex".into()))?;
            Ok(symbol_sign * b)
        } else {
            Err(Error(format!("mcode {mcode} zu groß")))
        }
    } else {
        let nrl = *params
            .nrl
            .get(mcode as usize)
            .ok_or_else(|| Error("Ungültiger NRL-Index".into()))?;
        let s = SF[usize::from(thidx)];
        Ok(symbol_sign * nrl * s)
    }
}

struct BitReader<'a> {
    bytes: &'a [u8],
    pos: usize,
    bit: u8,
}

impl<'a> BitReader<'a> {
    #[inline]
    fn new(bytes: &'a [u8], start: usize) -> Self {
        Self {
            bytes,
            pos: start,
            bit: 0,
        }
    }

    #[inline]
    fn bit(&mut self) -> Result<bool, Error> {
        let byte = *self
            .bytes
            .get(self.pos)
            .ok_or_else(|| Error(format!("nerwartetes Paket-Ende bei Byte {}", self.pos)))?;
        let res = ((byte >> (7 - self.bit)) & 1) != 0;
        self.bit += 1;
        if self.bit > 7 {
            self.bit = 0;
            self.pos += 1;
        }
        Ok(res)
    }

    #[inline]
    fn bits(&mut self, n: u32) -> Result<u32, Error> {
        let mut v = 0u32;
        for _ in 0..n {
            v = (v << 1) | u32::from(self.bit()?);
        }
        Ok(v)
    }

    #[inline]
    fn consume_padding_bits(&mut self) {
        if self.pos.is_multiple_of(2) {
            if self.bit != 0 {
                self.pos += 2;
                self.bit = 0;
            }
        } else {
            self.pos += 1;
            self.bit = 0;
        }
    }
}

#[inline]
fn decode_huffman(brc: u8, s: &mut BitReader) -> Result<u32, Error> {
    match brc {
        0 => Ok(if !s.bit()? {
            0
        } else if !s.bit()? {
            1
        } else if !s.bit()? {
            2
        } else {
            3
        }),
        1 => Ok(if !s.bit()? {
            0
        } else if !s.bit()? {
            1
        } else if !s.bit()? {
            2
        } else if !s.bit()? {
            3
        } else {
            4
        }),
        2 => Ok(if !s.bit()? {
            0
        } else if !s.bit()? {
            1
        } else if !s.bit()? {
            2
        } else if !s.bit()? {
            3
        } else if !s.bit()? {
            4
        } else if !s.bit()? {
            5
        } else {
            6
        }),
        3 => Ok(if !s.bit()? {
            u32::from(s.bit()?)
        } else if !s.bit()? {
            2
        } else if !s.bit()? {
            3
        } else if !s.bit()? {
            4
        } else if !s.bit()? {
            5
        } else if !s.bit()? {
            6
        } else if !s.bit()? {
            7
        } else if !s.bit()? {
            8
        } else {
            9
        }),
        4 => Ok(if !s.bit()? {
            if !s.bit()? {
                0
            } else if !s.bit()? {
                1
            } else {
                2
            }
        } else if !s.bit()? {
            if !s.bit()? { 3 } else { 4 }
        } else if !s.bit()? {
            if !s.bit()? { 5 } else { 6 }
        } else if !s.bit()? {
            7
        } else if !s.bit()? {
            8
        } else if !s.bit()? {
            9
        } else if !s.bit()? {
            if !s.bit()? { 10 } else { 11 }
        } else if !s.bit()? {
            if !s.bit()? { 12 } else { 13 }
        } else if !s.bit()? {
            14
        } else {
            15
        }),
        _ => Err(Error(format!("Ungültiger BRC {brc}"))),
    }
}

#[inline]
fn fdbaq_params(brc: u8) -> Result<ReconParams, Error> {
    match brc {
        0 => Ok(ReconParams {
            simple_limit: 3,
            max_mcode: 3,
            simple: &B0,
            nrl: &NRL0,
        }),
        1 => Ok(ReconParams {
            simple_limit: 3,
            max_mcode: 4,
            simple: &B1,
            nrl: &NRL1,
        }),
        2 => Ok(ReconParams {
            simple_limit: 5,
            max_mcode: 6,
            simple: &B2,
            nrl: &NRL2,
        }),
        3 => Ok(ReconParams {
            simple_limit: 6,
            max_mcode: 9,
            simple: &B3,
            nrl: &NRL3,
        }),
        4 => Ok(ReconParams {
            simple_limit: 8,
            max_mcode: 15,
            simple: &B4,
            nrl: &NRL4,
        }),
        _ => Err(Error(format!("Ungültiger BRC {brc}"))),
    }
}

/// Dekodiert ein einzelnes Paket (`rec`) direkt in den Ziel-Slice `dst_row[..2*nq]`.
pub fn decode_packet_into(
    file_bytes: &[u8],
    rec: &PacketRecord,
    dst_row: &mut [Complex32],
) -> Result<(), Error> {
    let nq = usize::from(rec.number_of_quads);
    if dst_row.len() < 2 * nq {
        return Err(Error("Ziel-Zeilenpuffer zu klein für Paket".into()));
    }
    let mut reader = BitReader::new(file_bytes, rec.offset + HEADER_LEN);
    let n_blocks = (2 * nq).div_ceil(256);

    match rec.baq_mode {
        12..=14 => {
            let mut brcs = Vec::with_capacity(n_blocks);
            let mut thidxs = Vec::with_capacity(n_blocks);
            let mut ie_m = Vec::with_capacity(nq);
            let mut io_m = Vec::with_capacity(nq);

            // IE
            while ie_m.len() < nq {
                let brc = reader.bits(3)? as u8;
                if brc > 4 {
                    return Err(Error(format!("Ungültiger BRC {brc}")));
                }
                brcs.push(brc);
                for _ in 0..SYMBOLS_PER_BLOCK {
                    if ie_m.len() >= nq {
                        break;
                    }
                    let sign = if reader.bit()? { -1.0f32 } else { 1.0f32 };
                    let mcode = decode_huffman(brc, &mut reader)?;
                    ie_m.push((sign, mcode));
                }
            }
            reader.consume_padding_bits();

            // IO
            let mut blk = 0;
            while io_m.len() < nq {
                let brc = brcs[blk];
                for _ in 0..SYMBOLS_PER_BLOCK {
                    if io_m.len() >= nq {
                        break;
                    }
                    let sign = if reader.bit()? { -1.0f32 } else { 1.0f32 };
                    let mcode = decode_huffman(brc, &mut reader)?;
                    io_m.push((sign, mcode));
                }
                blk += 1;
            }
            reader.consume_padding_bits();

            // QE (liest thidx) + gleichzeitige IE-Rekonstruktion
            blk = 0;
            let mut idx = 0;
            while idx < nq {
                let brc = brcs[blk];
                let thidx = reader.bits(8)? as u8;
                thidxs.push(thidx);
                let params = fdbaq_params(brc)?;
                for _ in 0..SYMBOLS_PER_BLOCK {
                    if idx >= nq {
                        break;
                    }
                    let q_sign = if reader.bit()? { -1.0f32 } else { 1.0f32 };
                    let q_mcode = decode_huffman(brc, &mut reader)?;
                    let qe_val = reconstruct(params, thidx, q_mcode, q_sign)?;
                    let (i_sign, i_mcode) = ie_m[idx];
                    let ie_val = reconstruct(params, thidx, i_mcode, i_sign)?;
                    dst_row[2 * idx] = Complex32::new(ie_val, qe_val);
                    idx += 1;
                }
                blk += 1;
            }
            reader.consume_padding_bits();

            // QO + gleichzeitige IO-Rekonstruktion
            blk = 0;
            idx = 0;
            while idx < nq {
                let brc = brcs[blk];
                let thidx = thidxs[blk];
                let params = fdbaq_params(brc)?;
                for _ in 0..SYMBOLS_PER_BLOCK {
                    if idx >= nq {
                        break;
                    }
                    let q_sign = if reader.bit()? { -1.0f32 } else { 1.0f32 };
                    let q_mcode = decode_huffman(brc, &mut reader)?;
                    let qo_val = reconstruct(params, thidx, q_mcode, q_sign)?;
                    let (i_sign, i_mcode) = io_m[idx];
                    let io_val = reconstruct(params, thidx, i_mcode, i_sign)?;
                    dst_row[2 * idx + 1] = Complex32::new(io_val, qo_val);
                    idx += 1;
                }
                blk += 1;
            }
            Ok(())
        }
        3..=5 => {
            let (width, mask, params) = match rec.baq_mode {
                3 => (
                    3u32,
                    0x3u32,
                    ReconParams {
                        simple_limit: 3,
                        max_mcode: 3,
                        simple: &A3,
                        nrl: &NRLA3,
                    },
                ),
                4 => (
                    4u32,
                    0x7u32,
                    ReconParams {
                        simple_limit: 5,
                        max_mcode: 7,
                        simple: &A4,
                        nrl: &NRLA4,
                    },
                ),
                _ => (
                    5u32,
                    0xFu32,
                    ReconParams {
                        simple_limit: 10,
                        max_mcode: 15,
                        simple: &A5,
                        nrl: &NRLA5,
                    },
                ),
            };
            let split = |code: u32| -> (f32, u32) {
                let s = if ((code >> (width - 1)) & 1) == 0 {
                    1.0
                } else {
                    -1.0
                };
                (s, code & mask)
            };
            let mut ie_m = Vec::with_capacity(nq);
            while ie_m.len() < nq {
                for _ in 0..SYMBOLS_PER_BLOCK {
                    if ie_m.len() >= nq {
                        break;
                    }
                    ie_m.push(split(reader.bits(width)?));
                }
            }
            reader.consume_padding_bits();
            let mut io_m = Vec::with_capacity(nq);
            while io_m.len() < nq {
                for _ in 0..SYMBOLS_PER_BLOCK {
                    if io_m.len() >= nq {
                        break;
                    }
                    io_m.push(split(reader.bits(width)?));
                }
            }
            reader.consume_padding_bits();
            let mut thidxs = Vec::with_capacity(n_blocks);
            let mut idx = 0;
            while idx < nq {
                let thidx = reader.bits(8)? as u8;
                thidxs.push(thidx);
                for _ in 0..SYMBOLS_PER_BLOCK {
                    if idx >= nq {
                        break;
                    }
                    let (qs, qm) = split(reader.bits(width)?);
                    let qe_val = reconstruct(params, thidx, qm, qs)?;
                    let (is, im) = ie_m[idx];
                    let ie_val = reconstruct(params, thidx, im, is)?;
                    dst_row[2 * idx] = Complex32::new(ie_val, qe_val);
                    idx += 1;
                }
            }
            reader.consume_padding_bits();
            let mut blk = 0;
            idx = 0;
            while idx < nq {
                let thidx = thidxs[blk];
                for _ in 0..SYMBOLS_PER_BLOCK {
                    if idx >= nq {
                        break;
                    }
                    let (qs, qm) = split(reader.bits(width)?);
                    let qo_val = reconstruct(params, thidx, qm, qs)?;
                    let (is, im) = io_m[idx];
                    let io_val = reconstruct(params, thidx, im, is)?;
                    dst_row[2 * idx + 1] = Complex32::new(io_val, qo_val);
                    idx += 1;
                }
                blk += 1;
            }
            Ok(())
        }
        0 => {
            let sm10 = |code: u32| -> f32 {
                let sign = if ((code >> 9) & 1) == 0 { 1.0 } else { -1.0 };
                sign * (code & 0x1FF) as f32
            };
            let mut ie = Vec::with_capacity(nq);
            for _ in 0..nq {
                ie.push(sm10(reader.bits(10)?));
            }
            reader.consume_padding_bits();
            let mut io = Vec::with_capacity(nq);
            for _ in 0..nq {
                io.push(sm10(reader.bits(10)?));
            }
            reader.consume_padding_bits();
            for (k, &ie_v) in ie.iter().enumerate() {
                let qe_v = sm10(reader.bits(10)?);
                dst_row[2 * k] = Complex32::new(ie_v, qe_v);
            }
            reader.consume_padding_bits();
            for (k, &io_v) in io.iter().enumerate() {
                let qo_v = sm10(reader.bits(10)?);
                dst_row[2 * k + 1] = Complex32::new(io_v, qo_v);
            }
            Ok(())
        }
        other => Err(Error(format!("Nicht unterstützter BAQ-Modus {other}"))),
    }
}
