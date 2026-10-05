//! Space-packet header parsing.
//!
//! Decodes the 68 header bytes (`6 + 62`) into the 54 named fields used by
//! the decode loop in `copernicus_00_main.cpp` and the field dump in
//! `copernicus_03_process_packet_headers.cpp`. Multi-byte integers are
//! big-endian; bit fields use the same masks and shifts as the C++ code.

use crate::error::{Error, Result};
use crate::utils::{FREF, HEADER_LEN, SYNC_MARKER};

/// The 54 decoded header fields, in packet order.
#[derive(Debug, Clone, Default)]
pub struct PacketHeader {
    pub packet_version_number: u32,
    pub packet_type: u32,
    pub secondary_header_flag: u32,
    pub application_process_id_process_id: u32,
    pub application_process_id_packet_category: u32,
    pub sequence_flags: u32,
    pub sequence_count: u32,
    pub data_length: u32,
    pub coarse_time: u32,
    pub fine_time: u32,
    pub sync_marker: u32,
    pub data_take_id: u32,
    pub ecc_number: u32,
    pub ignore_0: u32,
    pub test_mode: u32,
    pub rx_channel_id: u32,
    pub instrument_configuration_id: u32,
    pub sub_commutated_index: u32,
    pub sub_commutated_data: u32,
    pub space_packet_count: u32,
    pub pri_count: u32,
    pub error_flag: u32,
    pub ignore_1: u32,
    pub baq_mode: u32,
    pub baq_block_length: u32,
    pub ignore_2: u32,
    pub range_decimation: u32,
    pub rx_gain: u32,
    pub tx_ramp_rate_polarity: u32,
    pub tx_ramp_rate_magnitude: u32,
    pub tx_pulse_start_frequency_polarity: u32,
    pub tx_pulse_start_frequency_magnitude: u32,
    pub tx_pulse_length: u32,
    pub ignore_3: u32,
    pub rank: u32,
    pub pulse_repetition_interval: u32,
    pub sampling_window_start_time: u32,
    pub sampling_window_length: u32,
    pub sab_ssb_calibration_p: u32,
    pub sab_ssb_polarisation: u32,
    pub sab_ssb_temp_comp: u32,
    pub sab_ssb_ignore_0: u32,
    pub sab_ssb_elevation_beam_address: u32,
    pub sab_ssb_ignore_1: u32,
    pub sab_ssb_azimuth_beam_address: u32,
    pub ses_ssb_cal_mode: u32,
    pub ses_ssb_ignore_0: u32,
    pub ses_ssb_tx_pulse_number: u32,
    pub ses_ssb_signal_type: u32,
    pub ses_ssb_ignore_1: u32,
    pub ses_ssb_swap: u32,
    pub ses_ssb_swath_number: u32,
    pub number_of_quads: u32,
    pub ignore_4: u32,
}

/// Column names in packet order, matching the `state._packet_header` keys.
pub const FIELD_NAMES: [&str; 54] = [
    "packet_version_number",
    "packet_type",
    "secondary_header_flag",
    "application_process_id_process_id",
    "application_process_id_packet_category",
    "sequence_flags",
    "sequence_count",
    "data_length",
    "coarse_time",
    "fine_time",
    "sync_marker",
    "data_take_id",
    "ecc_number",
    "ignore_0",
    "test_mode",
    "rx_channel_id",
    "instrument_configuration_id",
    "sub_commutated_index",
    "sub_commutated_data",
    "space_packet_count",
    "pri_count",
    "error_flag",
    "ignore_1",
    "baq_mode",
    "baq_block_length",
    "ignore_2",
    "range_decimation",
    "rx_gain",
    "tx_ramp_rate_polarity",
    "tx_ramp_rate_magnitude",
    "tx_pulse_start_frequency_polarity",
    "tx_pulse_start_frequency_magnitude",
    "tx_pulse_length",
    "ignore_3",
    "rank",
    "pulse_repetition_interval",
    "sampling_window_start_time",
    "sampling_window_length",
    "sab_ssb_calibration_p",
    "sab_ssb_polarisation",
    "sab_ssb_temp_comp",
    "sab_ssb_ignore_0",
    "sab_ssb_elevation_beam_address",
    "sab_ssb_ignore_1",
    "sab_ssb_azimuth_beam_address",
    "ses_ssb_cal_mode",
    "ses_ssb_ignore_0",
    "ses_ssb_tx_pulse_number",
    "ses_ssb_signal_type",
    "ses_ssb_ignore_1",
    "ses_ssb_swap",
    "ses_ssb_swath_number",
    "number_of_quads",
    "ignore_4",
];

/// Dashed field names for the human-readable header dump.
pub const DUMP_NAMES: [&str; 54] = [
    "packet-version-number",
    "packet-type",
    "secondary-header-flag",
    "application-process-id-process-id",
    "application-process-id-packet-category",
    "sequence-flags",
    "sequence-count",
    "data-length",
    "coarse-time",
    "fine-time",
    "sync-marker",
    "data-take-id",
    "ecc-number",
    "ignore-0",
    "test-mode",
    "rx-channel-id",
    "instrument-configuration-id",
    "sub-commutated-index",
    "sub-commutated-data",
    "space-packet-count",
    "pri-count",
    "error-flag",
    "ignore-1",
    "baq-mode",
    "baq-block-length",
    "ignore-2",
    "range-decimation",
    "rx-gain",
    "tx-ramp-rate-polarity",
    "tx-ramp-rate-magnitude",
    "tx-pulse-start-frequency-polarity",
    "tx-pulse-start-frequency-magnitude",
    "tx-pulse-length",
    "ignore-3",
    "rank",
    "pulse-repetition-interval",
    "sampling-window-start-time",
    "sampling-window-length",
    "sab-ssb-calibration-p",
    "sab-ssb-polarisation",
    "sab-ssb-temp-comp",
    "sab-ssb-ignore-0",
    "sab-ssb-elevation-beam-address",
    "sab-ssb-ignore-1",
    "sab-ssb-azimuth-beam-address",
    "ses-ssb-cal-mode",
    "ses-ssb-ignore-0",
    "ses-ssb-tx-pulse-number",
    "ses-ssb-signal-type",
    "ses-ssb-ignore-1",
    "ses-ssb-swap",
    "ses-ssb-swath-number",
    "number-of-quads",
    "ignore-4",
];

/// Bit widths used by the header dump, in packet order.
pub const FIELD_WIDTHS: [u32; 54] = [
    3, 1, 1, 7, 4, 2, 14, 16, 32, 16, 32, 32, 8, 1, 3, 4, 32, 8, 16, 32, 32, 1, 2, 5, 8, 8, 8, 8,
    1, 15, 1, 15, 24, 3, 5, 24, 24, 24, 1, 3, 2, 2, 4, 2, 10, 2, 1, 5, 4, 3, 1, 8, 16, 8,
];

fn u16be(p: &[u8], i: usize) -> u32 {
    u32::from(p[i]) << 8 | u32::from(p[i + 1])
}

fn u24be(p: &[u8], i: usize) -> u32 {
    u32::from(p[i]) << 16 | u32::from(p[i + 1]) << 8 | u32::from(p[i + 2])
}

fn u32be(p: &[u8], i: usize) -> u32 {
    u32::from(p[i]) << 24
        | u32::from(p[i + 1]) << 16
        | u32::from(p[i + 2]) << 8
        | u32::from(p[i + 3])
}

impl PacketHeader {
    /// Parse the 68 header bytes. Fails if fewer bytes are available.
    pub fn parse(p: &[u8]) -> Result<PacketHeader> {
        if p.len() < HEADER_LEN {
            return Err(Error::HeaderTooShort { offset: 0 });
        }
        Ok(PacketHeader {
            packet_version_number: u32::from(p[0] >> 5) & 0x7,
            packet_type: u32::from(p[0] >> 4) & 0x1,
            secondary_header_flag: u32::from(p[0] >> 3) & 0x1,
            application_process_id_process_id: u32::from(p[1] >> 4) + 0x10 * u32::from(p[0] & 0x7),
            application_process_id_packet_category: u32::from(p[1]) & 0xF,
            sequence_flags: u32::from(p[2] >> 6) & 0x3,
            sequence_count: u32::from(p[3]) + 0x100 * u32::from(p[2] & 0x3F),
            data_length: u16be(p, 4),
            coarse_time: u32be(p, 6),
            fine_time: u16be(p, 10),
            sync_marker: u32be(p, 12),
            data_take_id: u32be(p, 16),
            ecc_number: u32::from(p[20]),
            ignore_0: u32::from(p[21] >> 7) & 0x1,
            test_mode: u32::from(p[21] >> 4) & 0x7,
            rx_channel_id: u32::from(p[21]) & 0xF,
            instrument_configuration_id: u32be(p, 22),
            sub_commutated_index: u32::from(p[26]),
            sub_commutated_data: u16be(p, 27),
            space_packet_count: u32be(p, 29),
            pri_count: u32be(p, 33),
            error_flag: u32::from(p[37] >> 7) & 0x1,
            ignore_1: u32::from(p[37] >> 5) & 0x3,
            baq_mode: u32::from(p[37]) & 0x1F,
            baq_block_length: u32::from(p[38]),
            ignore_2: u32::from(p[39]),
            range_decimation: u32::from(p[40]),
            rx_gain: u32::from(p[41]),
            tx_ramp_rate_polarity: u32::from(p[42] >> 7) & 0x1,
            tx_ramp_rate_magnitude: u32::from(p[43]) + 0x100 * u32::from(p[42] & 0x7F),
            tx_pulse_start_frequency_polarity: u32::from(p[44] >> 7) & 0x1,
            tx_pulse_start_frequency_magnitude: u32::from(p[45]) + 0x100 * u32::from(p[44] & 0x7F),
            tx_pulse_length: u24be(p, 46),
            ignore_3: u32::from(p[49] >> 5) & 0x7,
            rank: u32::from(p[49]) & 0x1F,
            pulse_repetition_interval: u24be(p, 50),
            sampling_window_start_time: u24be(p, 53),
            sampling_window_length: u24be(p, 56),
            sab_ssb_calibration_p: u32::from(p[59] >> 7) & 0x1,
            sab_ssb_polarisation: u32::from(p[59] >> 4) & 0x7,
            sab_ssb_temp_comp: u32::from(p[59] >> 2) & 0x3,
            sab_ssb_ignore_0: u32::from(p[59]) & 0x3,
            sab_ssb_elevation_beam_address: u32::from(p[60] >> 4) & 0xF,
            sab_ssb_ignore_1: u32::from(p[60] >> 2) & 0x3,
            sab_ssb_azimuth_beam_address: u32::from(p[61]) + 0x100 * u32::from(p[60] & 0x3),
            ses_ssb_cal_mode: u32::from(p[62] >> 6) & 0x3,
            ses_ssb_ignore_0: u32::from(p[62] >> 5) & 0x1,
            ses_ssb_tx_pulse_number: u32::from(p[62]) & 0x1F,
            ses_ssb_signal_type: u32::from(p[63] >> 4) & 0xF,
            ses_ssb_ignore_1: u32::from(p[63] >> 1) & 0x7,
            ses_ssb_swap: u32::from(p[63]) & 0x1,
            ses_ssb_swath_number: u32::from(p[64]),
            number_of_quads: u16be(p, 65),
            ignore_4: u32::from(p[67]),
        })
    }

    /// All 54 field values in packet order.
    pub fn values(&self) -> [u32; 54] {
        [
            self.packet_version_number,
            self.packet_type,
            self.secondary_header_flag,
            self.application_process_id_process_id,
            self.application_process_id_packet_category,
            self.sequence_flags,
            self.sequence_count,
            self.data_length,
            self.coarse_time,
            self.fine_time,
            self.sync_marker,
            self.data_take_id,
            self.ecc_number,
            self.ignore_0,
            self.test_mode,
            self.rx_channel_id,
            self.instrument_configuration_id,
            self.sub_commutated_index,
            self.sub_commutated_data,
            self.space_packet_count,
            self.pri_count,
            self.error_flag,
            self.ignore_1,
            self.baq_mode,
            self.baq_block_length,
            self.ignore_2,
            self.range_decimation,
            self.rx_gain,
            self.tx_ramp_rate_polarity,
            self.tx_ramp_rate_magnitude,
            self.tx_pulse_start_frequency_polarity,
            self.tx_pulse_start_frequency_magnitude,
            self.tx_pulse_length,
            self.ignore_3,
            self.rank,
            self.pulse_repetition_interval,
            self.sampling_window_start_time,
            self.sampling_window_length,
            self.sab_ssb_calibration_p,
            self.sab_ssb_polarisation,
            self.sab_ssb_temp_comp,
            self.sab_ssb_ignore_0,
            self.sab_ssb_elevation_beam_address,
            self.sab_ssb_ignore_1,
            self.sab_ssb_azimuth_beam_address,
            self.ses_ssb_cal_mode,
            self.ses_ssb_ignore_0,
            self.ses_ssb_tx_pulse_number,
            self.ses_ssb_signal_type,
            self.ses_ssb_ignore_1,
            self.ses_ssb_swap,
            self.ses_ssb_swath_number,
            self.number_of_quads,
            self.ignore_4,
        ]
    }

    /// True for calibration packets (`cal_p` set).
    pub fn is_calibration(&self) -> bool {
        self.sab_ssb_calibration_p != 0
    }

    /// Elevation beam address (0..=15).
    pub fn elevation(&self) -> u32 {
        self.sab_ssb_elevation_beam_address
    }

    /// Calibration type: elevation address masked to 3 bits.
    pub fn cal_type(&self) -> u32 {
        self.elevation() & 7
    }

    /// Delay in samples between transmit-pulse start and data acquisition:
    /// 40 + sampling-window-start-time.
    pub fn data_delay(&self) -> u32 {
        40 + self.sampling_window_start_time
    }

    /// Sampling-window start time in microseconds.
    pub fn swst_us(&self) -> f64 {
        f64::from(self.sampling_window_start_time) / FREF
    }

    /// Signed TX ramp-rate magnitude: `(-1)^polarity * magnitude`.
    pub fn tx_ramp_rate_signed(&self) -> f64 {
        // Polaritaet 1 = positiv (sentinel1decoder `_txprr`:
        // `sign = (-1)**(1 - (vals >> 15))`, per Echtdaten-Test belegt).
        let sign = if self.tx_ramp_rate_polarity == 0 {
            -1.0
        } else {
            1.0
        };
        sign * f64::from(self.tx_ramp_rate_magnitude)
    }

    /// TX ramp rate in MHz/us.
    pub fn tx_ramp_rate(&self) -> f64 {
        (FREF * FREF / 2_097_152.0) * self.tx_ramp_rate_signed()
    }

    /// TX pulse start frequency in MHz.
    pub fn tx_pulse_start_frequency(&self) -> f64 {
        // Polaritaet 1 = positiv (sentinel1decoder `_txpsf`, analog `_txprr`).
        let sign = if self.tx_pulse_start_frequency_polarity == 0 {
            -1.0
        } else {
            1.0
        };
        self.tx_ramp_rate() / (FREF * 4.0)
            + (FREF / 16384.0) * sign * f64::from(self.tx_pulse_start_frequency_magnitude)
    }

    /// TX pulse length in microseconds.
    pub fn tx_pulse_length_us(&self) -> f64 {
        f64::from(self.tx_pulse_length) / FREF
    }

    /// Packet time in seconds relative to `time0` (coarse + fine time).
    pub fn time_relative(&self, time0: f64) -> f64 {
        let ftime = 1.52587890625e-5 * (0.5 + f64::from(self.fine_time));
        f64::from(self.coarse_time) + ftime - time0
    }

    /// Check the sync marker, mirroring `assert(sync_marker == 0x352EF853)`.
    pub fn check_sync(&self, packet_idx: usize) -> Result<()> {
        if self.sync_marker != SYNC_MARKER {
            return Err(Error::BadSync {
                packet_idx,
                found: self.sync_marker,
            });
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    pub(crate) fn sample_header() -> [u8; HEADER_LEN] {
        let mut p = [0u8; HEADER_LEN];
        // sync marker 0x352EF853
        p[12..16].copy_from_slice(&[0x35, 0x2E, 0xF8, 0x53]);
        // number_of_quads = 0x1234
        p[65] = 0x12;
        p[66] = 0x34;
        // baq_mode = 12, error flag set
        p[37] = 0x80 | 12;
        p[38] = 31; // baq block length
                    // calibration packet, polarisation 5, ele = 0xA
        p[59] = 0x80 | (5 << 4);
        p[60] = 0xA0 | 0x02;
        p[61] = 0x34; // azi = 0x234
                      // signal_type = 7, swath = 9
        p[63] = 0x70;
        p[64] = 9;
        // swst = 3548, swl = 100
        p[53] = 0x00;
        p[54] = 0x0D;
        p[55] = 0xDC;
        p[56] = 0x00;
        p[57] = 0x00;
        p[58] = 0x64;
        // space packet count / pri count
        p[29..33].copy_from_slice(&[0x00, 0x00, 0x01, 0x00]);
        p[33..37].copy_from_slice(&[0x00, 0x00, 0x02, 0x00]);
        // tx fields
        p[42] = 0x81;
        p[43] = 0x02; // txprr mag = 0x102
        p[44] = 0x00;
        p[45] = 0x03; // txpsf mag = 3
        p[46..49].copy_from_slice(&[0x00, 0x00, 0x64]); // txpl = 100
        p[49] = 0x1F; // rank 31
        p[21] = 0x7A; // test_mode 7, rx 0xA
        p[20] = 0xEE; // ecc
        p[26] = 0x2A; // sub index
        p[27] = 0xAB;
        p[28] = 0xCD; // sub data 0xABCD
        p
    }

    #[test]
    fn parses_all_fields() {
        let h = PacketHeader::parse(&sample_header()).unwrap();
        assert_eq!(h.sync_marker, SYNC_MARKER);
        assert_eq!(h.number_of_quads, 0x1234);
        assert_eq!(h.baq_mode, 12);
        assert_eq!(h.error_flag, 1);
        assert!(h.is_calibration());
        assert_eq!(h.sab_ssb_polarisation, 5);
        assert_eq!(h.elevation(), 0xA);
        assert_eq!(h.cal_type(), 0xA & 7);
        assert_eq!(h.sab_ssb_azimuth_beam_address, 0x234);
        assert_eq!(h.ses_ssb_signal_type, 7);
        assert_eq!(h.ses_ssb_swath_number, 9);
        assert_eq!(h.sampling_window_start_time, 3548);
        assert_eq!(h.data_delay(), 3548 + 40);
        assert_eq!(h.sampling_window_length, 100);
        assert_eq!(h.space_packet_count, 256);
        assert_eq!(h.pri_count, 512);
        assert_eq!(h.tx_ramp_rate_polarity, 1);
        assert_eq!(h.tx_ramp_rate_magnitude, 0x102);
        assert_eq!(h.tx_pulse_length, 100);
        assert_eq!(h.rank, 31);
        assert_eq!(h.test_mode, 7);
        assert_eq!(h.rx_channel_id, 0xA);
        assert_eq!(h.ecc_number, 0xEE);
        assert_eq!(h.sub_commutated_index, 0x2A);
        assert_eq!(h.sub_commutated_data, 0xABCD);
    }

    #[test]
    fn derived_tx_quantities() {
        let h = PacketHeader::parse(&sample_header()).unwrap();
        assert_eq!(h.tx_ramp_rate_signed(), 258.0);
        let expected_rate = (FREF * FREF / 2_097_152.0) * 258.0;
        assert!((h.tx_ramp_rate() - expected_rate).abs() < 1e-12);
        assert!((h.tx_pulse_length_us() - 100.0 / FREF).abs() < 1e-12);
        // TXPSF-Polaritaet 0 = negativ (Betrag 3).
        let expected_psf = expected_rate / (FREF * 4.0) + (FREF / 16384.0) * -3.0;
        assert!((h.tx_pulse_start_frequency() - expected_psf).abs() < 1e-12);
    }

    #[test]
    fn sync_check_rejects_bad_marker() {
        let mut p = sample_header();
        p[12] = 0x00;
        let h = PacketHeader::parse(&p).unwrap();
        assert!(matches!(
            h.check_sync(3),
            Err(Error::BadSync { packet_idx: 3, .. })
        ));
    }

    #[test]
    fn values_align_with_field_names() {
        let h = PacketHeader::parse(&sample_header()).unwrap();
        let values = h.values();
        assert_eq!(values.len(), FIELD_NAMES.len());
        assert_eq!(values.len(), DUMP_NAMES.len());
        assert_eq!(values.len(), FIELD_WIDTHS.len());
        // Spot check: number_of_quads is the second-to-last column.
        let idx = FIELD_NAMES
            .iter()
            .position(|n| *n == "number_of_quads")
            .unwrap();
        assert_eq!(values[idx], 0x1234);
    }

    #[test]
    fn short_slice_is_rejected() {
        assert!(matches!(
            PacketHeader::parse(&[0u8; 10]),
            Err(Error::HeaderTooShort { .. })
        ));
    }
}
