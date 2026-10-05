//! Sub-commutated ancillary-data decoder
//! (`copernicus_06_decode_sub_commutated_data.cpp`).
//!
//! Each space packet carries one 16-bit ancillary word plus its index in the
//! 65-word ancillary block. Once word 64 arrives with words 1..=64 all valid,
//! the block is reinterpreted as [`AncillaryData`] and appended to
//! `o_anxillary.csv` (the C++ file name, typo included).

use crate::error::{Error, Result};
use crate::utils::{fmt_g3, fmt_g3_f32};

/// Number of 16-bit words per ancillary block.
pub const ANCILLARY_WORDS: usize = 65;

/// Decoded ancillary (housekeeping) data block (`ancillary_data_t`).
///
/// Tile temperatures are stored as arrays indexed `tile - 1`; tiles 2..=15
/// carry an `efe_h_ta` sensor and tiles 1..=14 the other three sensors,
/// exactly like the individual C++ struct fields.
#[derive(Debug, Clone, Default)]
pub struct AncillaryData {
    pub x_axis_position: f64,
    pub y_axis_position: f64,
    pub z_axis_position: f64,
    pub x_velocity: f32,
    pub y_velocity: f32,
    pub z_velocity: f32,
    pub pod_solution_data_stamp: [u16; 4],
    pub quaternion: [f32; 4],
    pub angular_rate: [f32; 3],
    pub gps_data_timestamp: [u16; 4],
    pub pointing_status: u16,
    pub temperature_update_status: u16,
    /// Tiles 1..=14.
    pub tile_efe_h_temperature: [u8; 14],
    /// Tiles 1..=14.
    pub tile_efe_v_temperature: [u8; 14],
    /// Tiles 1..=14.
    pub tile_active_ta_temperature: [u8; 14],
    /// Tiles 2..=15.
    pub tile_efe_h_ta_temperature: [u8; 14],
    pub tgu_temperature: u16,
}

impl AncillaryData {
    /// Reinterpret 65 little-endian words as ancillary data.
    ///
    /// The C++ code `memcpy`s the 130 raw bytes into a 142-byte struct; the
    /// trailing bytes (tile 12 active-TA onward plus the TGU temperature)
    /// therefore keep whatever the zero-initialized global state held, i.e.
    /// zero. This port pads with zeros explicitly.
    pub fn from_words(words: &[u16; ANCILLARY_WORDS]) -> AncillaryData {
        let mut raw = [0u8; 142];
        for (i, w) in words.iter().enumerate() {
            raw[2 * i..2 * i + 2].copy_from_slice(&w.to_le_bytes());
        }
        let mut pos = 0;
        let mut take = |n: usize| {
            let s = &raw[pos..pos + n];
            pos += n;
            s
        };
        let f64le = |s: &[u8]| f64::from_le_bytes(s.try_into().unwrap());
        let f32le = |s: &[u8]| f32::from_le_bytes(s.try_into().unwrap());
        let u16le = |s: &[u8]| u16::from_le_bytes(s.try_into().unwrap());
        let mut data = AncillaryData {
            x_axis_position: f64le(take(8)),
            y_axis_position: f64le(take(8)),
            z_axis_position: f64le(take(8)),
            x_velocity: f32le(take(4)),
            y_velocity: f32le(take(4)),
            z_velocity: f32le(take(4)),
            ..AncillaryData::default()
        };
        for stamp in data.pod_solution_data_stamp.iter_mut() {
            *stamp = u16le(take(2));
        }
        for q in data.quaternion.iter_mut() {
            *q = f32le(take(4));
        }
        for r in data.angular_rate.iter_mut() {
            *r = f32le(take(4));
        }
        for t in data.gps_data_timestamp.iter_mut() {
            *t = u16le(take(2));
        }
        data.pointing_status = u16le(take(2));
        data.temperature_update_status = u16le(take(2));
        // Tile 1 has no efe_h_ta sensor; tiles 2..=14 have all four sensors.
        for tile in 0..14 {
            if tile > 0 {
                data.tile_efe_h_ta_temperature[tile - 1] = take(1)[0];
            }
            data.tile_efe_h_temperature[tile] = take(1)[0];
            data.tile_efe_v_temperature[tile] = take(1)[0];
            data.tile_active_ta_temperature[tile] = take(1)[0];
        }
        // Tile 15 contributes only its efe_h_ta sensor.
        data.tile_efe_h_ta_temperature[13] = take(1)[0];
        data.tgu_temperature = u16le(take(2));
        debug_assert_eq!(pos, 142);
        data
    }

    /// CSV header for `o_anxillary.csv`, verbatim from the C++ program.
    pub fn csv_header() -> &'static str {
        "space_packet_count,x_axis_position,y_axis_position,z_axis_position,\
         x_velocity,y_velocity,z_velocity,\
         pod_solution_data_stamp_0,pod_solution_data_stamp_1,\
         pod_solution_data_stamp_2,pod_solution_data_stamp_3,\
         quaternion_0,quaternion_1,quaternion_2,quaternion_3,\
         angular_rate_x,angular_rate_y,angular_rate_z,\
         gps_data_timestamp_0,gps_data_timestamp_1,\
         gps_data_timestamp_2,gps_data_timestamp_3,\
         pointing_status,temperature_update_status,\
         tile_1_efe_h_temperature,tile_1_efe_v_temperature,\
         tile_1_active_ta_temperature,\
         tile_2_efe_h_ta_temperature,tile_2_efe_h_temperature,\
         tile_2_efe_v_temperature,tile_2_active_ta_temperature,\
         tile_3_efe_h_ta_temperature,tile_3_efe_h_temperature,\
         tile_3_efe_v_temperature,tile_3_active_ta_temperature,\
         tile_4_efe_h_ta_temperature,tile_4_efe_h_temperature,\
         tile_4_efe_v_temperature,tile_4_active_ta_temperature,\
         tile_5_efe_h_ta_temperature,tile_5_efe_h_temperature,\
         tile_5_efe_v_temperature,tile_5_active_ta_temperature,\
         tile_6_efe_h_ta_temperature,tile_6_efe_h_temperature,\
         tile_6_efe_v_temperature,tile_6_active_ta_temperature,\
         tile_7_efe_h_ta_temperature,tile_7_efe_h_temperature,\
         tile_7_efe_v_temperature,tile_7_active_ta_temperature,\
         tile_8_efe_h_ta_temperature,tile_8_efe_h_temperature,\
         tile_8_efe_v_temperature,tile_8_active_ta_temperature,\
         tile_9_efe_h_ta_temperature,tile_9_efe_h_temperature,\
         tile_9_efe_v_temperature,tile_9_active_ta_temperature,\
         tile_10_efe_h_ta_temperature,tile_10_efe_h_temperature,\
         tile_10_efe_v_temperature,tile_10_active_ta_temperature,\
         tile_11_efe_h_ta_temperature,tile_11_efe_h_temperature,\
         tile_11_efe_v_temperature,tile_11_active_ta_temperature,\
         tile_12_efe_h_ta_temperature,tile_12_efe_h_temperature,\
         tile_12_efe_v_temperature,tile_12_active_ta_temperature,\
         tile_13_efe_h_ta_temperature,tile_13_efe_h_temperature,\
         tile_13_efe_v_temperature,tile_13_active_ta_temperature,\
         tile_14_efe_h_ta_temperature,tile_14_efe_h_temperature,\
         tile_14_efe_v_temperature,tile_14_active_ta_temperature,\
         tile_15_efe_h_ta_temperature,tgu_temperature"
    }

    /// One CSV row; floats use `%.3g` like the C++ output stream.
    pub fn csv_row(&self, space_packet_count: u32) -> String {
        let mut cols: Vec<String> = Vec::with_capacity(67);
        cols.push(space_packet_count.to_string());
        cols.push(fmt_g3(self.x_axis_position));
        cols.push(fmt_g3(self.y_axis_position));
        cols.push(fmt_g3(self.z_axis_position));
        cols.push(fmt_g3_f32(self.x_velocity));
        cols.push(fmt_g3_f32(self.y_velocity));
        cols.push(fmt_g3_f32(self.z_velocity));
        for s in self.pod_solution_data_stamp {
            cols.push(s.to_string());
        }
        for q in self.quaternion {
            cols.push(fmt_g3_f32(q));
        }
        for r in self.angular_rate {
            cols.push(fmt_g3_f32(r));
        }
        for t in self.gps_data_timestamp {
            cols.push(t.to_string());
        }
        cols.push(self.pointing_status.to_string());
        cols.push(self.temperature_update_status.to_string());
        for tile in 0..14 {
            if tile > 0 {
                cols.push(self.tile_efe_h_ta_temperature[tile - 1].to_string());
            }
            cols.push(self.tile_efe_h_temperature[tile].to_string());
            cols.push(self.tile_efe_v_temperature[tile].to_string());
            cols.push(self.tile_active_ta_temperature[tile].to_string());
        }
        cols.push(self.tile_efe_h_ta_temperature[13].to_string());
        cols.push(self.tgu_temperature.to_string());
        cols.join(",")
    }
}

/// Accumulates sub-commutated words until a full ancillary block arrives.
#[derive(Debug)]
pub struct SubCommutatedDecoder {
    words: [u16; ANCILLARY_WORDS],
    valid: [bool; ANCILLARY_WORDS],
}

impl Default for SubCommutatedDecoder {
    fn default() -> SubCommutatedDecoder {
        SubCommutatedDecoder {
            words: [0; ANCILLARY_WORDS],
            valid: [false; ANCILLARY_WORDS],
        }
    }
}

impl SubCommutatedDecoder {
    /// Fresh decoder (`init_sub_commutated_data_decoder`).
    pub fn new() -> SubCommutatedDecoder {
        SubCommutatedDecoder::default()
    }

    /// Feed one word (`feed_sub_commutated_data_decoder`).
    ///
    /// Returns the decoded block once word 64 completes a fully valid block
    /// (words 1..=64 present), and resets itself like the C++ version.
    /// An index above 64 is an error (the C++ `.at()` throws, uncaught).
    pub fn feed(&mut self, word: u16, index: usize) -> Result<Option<AncillaryData>> {
        if index >= ANCILLARY_WORDS {
            return Err(Error::BadAncillaryIndex { index });
        }
        self.words[index] = word;
        self.valid[index] = true;
        if index == ANCILLARY_WORDS - 1 {
            if self.valid[1..].iter().any(|v| !v) {
                return Ok(None);
            }
            let decoded = AncillaryData::from_words(&self.words);
            *self = SubCommutatedDecoder::new();
            return Ok(Some(decoded));
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn full_block_decodes_and_resets() {
        let mut dec = SubCommutatedDecoder::new();
        for i in 0..64 {
            assert!(dec.feed(1000 + i as u16, i).unwrap().is_none());
        }
        // Word 0 may stay invalid; only 1..=64 are required.
        let data = dec.feed(2000, 64).unwrap().expect("complete block");
        // words[0..2] = 1000, 1001 -> x_axis_position LE bytes.
        let mut raw = [0u8; 8];
        raw[0..2].copy_from_slice(&1000u16.to_le_bytes());
        raw[2..4].copy_from_slice(&1001u16.to_le_bytes());
        raw[4..6].copy_from_slice(&1002u16.to_le_bytes());
        raw[6..8].copy_from_slice(&1003u16.to_le_bytes());
        assert_eq!(data.x_axis_position, f64::from_le_bytes(raw));
        // Decoder was reset: feeding word 64 again finds nothing valid.
        assert!(dec.feed(7, 64).unwrap().is_none());
    }

    #[test]
    fn missing_word_yields_no_block() {
        let mut dec = SubCommutatedDecoder::new();
        for i in [0, 2, 3] {
            dec.feed(1, i).unwrap();
        }
        assert!(dec.feed(1, 64).unwrap().is_none());
    }

    #[test]
    fn index_above_64_is_an_error() {
        let mut dec = SubCommutatedDecoder::new();
        assert!(matches!(
            dec.feed(0, 65),
            Err(Error::BadAncillaryIndex { index: 65 })
        ));
    }

    #[test]
    fn trailing_fields_default_to_zero() {
        // Only 130 of the 142 struct bytes arrive; the tail reads zero.
        let words = [0u16; ANCILLARY_WORDS];
        let data = AncillaryData::from_words(&words);
        assert_eq!(data.tgu_temperature, 0);
        assert_eq!(data.tile_active_ta_temperature[11], 0);
    }

    #[test]
    fn csv_header_and_row_have_equal_columns() {
        let header_cols = AncillaryData::csv_header().split(',').count();
        let data = AncillaryData::from_words(&[0u16; ANCILLARY_WORDS]);
        let row_cols = data.csv_row(42).split(',').count();
        assert_eq!(header_cols, row_cols);
        assert!(data.csv_row(42).starts_with("42,0,0,0,"));
    }
}
