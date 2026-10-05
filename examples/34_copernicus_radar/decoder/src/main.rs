//! Sentinel-1 SAR raw-data decoder binary (`copernicus_00_main.cpp`).
//!
//! Pipeline: memory-map the input `.dat` file, collect packet headers,
//! accumulate ancillary data, histogram calibration/signal packets, find the
//! elevation beam with the most data, decode its echoes into a complex
//! range image (`.cf`) and calibration image, and report per-packet details
//! to CSV files.

use std::collections::HashMap;
use std::fs::{self, File, OpenOptions};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::Instant;

use copernicus_radar::collect_headers::collect_packet_headers;
use copernicus_radar::decode_packet::decode_fdbaq;
use copernicus_radar::decode_type_ab::decode_type_a_or_b;
use copernicus_radar::decode_type_c::{decode_baq3, decode_baq4, decode_baq5};
use copernicus_radar::header::{PacketHeader, FIELD_NAMES};
use copernicus_radar::header_export::{export_packet_headers_csv, PacketHeaderTable};
use copernicus_radar::mmap::MappedFile;
use copernicus_radar::process_headers::process_packet_headers;
use copernicus_radar::sub_commutated::{AncillaryData, SubCommutatedDecoder};
use copernicus_radar::utils::{fmt_g3, BitReader, HEADER_LEN};
use copernicus_radar::{Error, Result, State};

use num_complex::Complex32;

/// Default input path, kept from the C++ program (override with argv[1]).
const DEFAULT_FILENAME: &str = "/home/martin/Downloads/\
     s1a-s3-raw-s-hh-20210221t213548-20210221t213613-036693-044fed.dat";
/// Range width of the calibration image.
const CAL_N0: usize = 6000;
/// Default cap on stored azimuth echoes (the C++ hard-codes 512).
const DEFAULT_MAX_ECHOES: usize = 512;

/// Log one line in the C++ format: elapsed-ns, location, message, `k=v` pairs.
macro_rules! log {
    ($state:expr, $msg:expr, $(($k:expr, $v:expr)),* $(,)?) => {{
        print!("{:>10} {}:{} {} {}", $state.elapsed_nanos(), file!(), line!(),
               module_path!().rsplit("::").next().unwrap_or("main"), $msg);
        $(print!(" {:>8}={}", $k, $v);)*
        println!();
    }};
}

#[derive(Debug)]
struct Config {
    filename: PathBuf,
    csv_dir: PathBuf,
    cf_dir: PathBuf,
    max_echoes: usize,
    dump_headers: bool,
    animate: bool,
    export_headers: bool,
}

impl Config {
    fn parse(args: &[String]) -> std::result::Result<Config, String> {
        let mut cfg = Config {
            filename: PathBuf::from(DEFAULT_FILENAME),
            csv_dir: PathBuf::from("."),
            cf_dir: PathBuf::from("/dev/shm"),
            max_echoes: DEFAULT_MAX_ECHOES,
            dump_headers: false,
            animate: false,
            export_headers: false,
        };
        let mut positional = false;
        let mut iter = args.iter().peekable();
        while let Some(arg) = iter.next() {
            match arg.as_str() {
                "--csv-dir" => {
                    cfg.csv_dir = PathBuf::from(iter.next().ok_or("--csv-dir needs a value")?);
                }
                "--cf-dir" => {
                    cfg.cf_dir = PathBuf::from(iter.next().ok_or("--cf-dir needs a value")?);
                }
                "--max-echoes" => {
                    cfg.max_echoes = iter
                        .next()
                        .ok_or("--max-echoes needs a value")?
                        .parse()
                        .map_err(|_| "--max-echoes needs an integer")?;
                }
                "--dump-headers" => cfg.dump_headers = true,
                "--animate" => cfg.animate = true,
                "--export-headers" => cfg.export_headers = true,
                "--help" | "-h" => return Err("help".to_string()),
                s if s.starts_with('-') => return Err(format!("unknown flag: {s}")),
                _ if !positional => {
                    cfg.filename = PathBuf::from(arg);
                    positional = true;
                }
                _ => return Err("only one input file argument is allowed".to_string()),
            }
        }
        Ok(cfg)
    }

    fn usage(program: &str) -> String {
        format!(
            "usage: {program} [input.dat] [--csv-dir DIR] [--cf-dir DIR] \
             [--max-echoes N] [--dump-headers] [--animate] [--export-headers]"
        )
    }
}

/// Append-only CSV writer that emits the header row before the first record.
struct CsvWriter {
    file: Option<File>,
    path: PathBuf,
    header: &'static str,
    started: bool,
}

impl CsvWriter {
    fn new(path: PathBuf, header: &'static str) -> CsvWriter {
        CsvWriter {
            file: None,
            path,
            header,
            started: false,
        }
    }

    fn append(&mut self, row: &str) -> Result<()> {
        if !self.started {
            let mut file = OpenOptions::new()
                .create(true)
                .append(true)
                .open(&self.path)?;
            // Mirror `if (0 == outfile.tellp())`: header only into an empty file.
            if file.metadata()?.len() == 0 {
                writeln!(file, "{}", self.header)?;
            }
            self.file = Some(file);
            self.started = true;
        }
        let file = self.file.as_mut().unwrap();
        writeln!(file, "{row}")?;
        Ok(())
    }
}

/// Per-packet context shared by both CSV row formats.
struct RowContext {
    packet_idx: usize,
    offset: usize,
    cal_iter: usize,
    ele_count: usize,
}

fn range_row(h: &PacketHeader, ctx: &RowContext) -> String {
    format!(
        "{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}",
        h.sab_ssb_azimuth_beam_address,
        h.baq_block_length,
        h.baq_mode,
        ctx.cal_iter,
        h.ses_ssb_cal_mode,
        h.sab_ssb_calibration_p,
        h.data_delay(),
        h.elevation(),
        ctx.ele_count,
        h.number_of_quads,
        ctx.offset,
        ctx.packet_idx,
        h.sab_ssb_polarisation,
        h.pri_count,
        h.rank,
        h.rx_channel_id,
        h.range_decimation,
        h.ses_ssb_signal_type,
        h.space_packet_count,
        h.ses_ssb_swath_number,
        h.sampling_window_length,
        h.sampling_window_start_time,
        h.test_mode,
        fmt_g3(h.tx_pulse_length_us()),
        h.tx_pulse_length,
        fmt_g3(h.tx_ramp_rate()),
        fmt_g3(h.tx_ramp_rate_signed()),
        fmt_g3(h.tx_pulse_start_frequency()),
    )
}

fn cal_row(h: &PacketHeader, ctx: &RowContext) -> String {
    format!(
        "{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{},{}",
        h.sab_ssb_azimuth_beam_address,
        h.baq_block_length,
        h.baq_mode,
        ctx.cal_iter,
        ctx.ele_count,
        h.ses_ssb_cal_mode,
        h.sab_ssb_calibration_p,
        h.cal_type(),
        h.data_delay(),
        h.number_of_quads,
        ctx.offset,
        ctx.packet_idx,
        h.sab_ssb_polarisation,
        h.pri_count,
        h.rank,
        h.range_decimation,
        h.rx_channel_id,
        h.ses_ssb_signal_type,
        h.space_packet_count,
        h.ses_ssb_swath_number,
        h.sampling_window_length,
        h.sampling_window_start_time,
        h.test_mode,
        fmt_g3(h.tx_pulse_length_us()),
        h.tx_pulse_length,
        fmt_g3(h.tx_ramp_rate()),
        fmt_g3(h.tx_ramp_rate_signed()),
        fmt_g3(h.tx_pulse_start_frequency()),
    )
}

const RANGE_CSV_HEADER: &str = "azi,baq_n,baqmod,cal_iter,cal_mode,cal_p,\
     data_delay,ele,ele_count,number_of_quads,offset,packet_idx,pol,pri_count,\
     rank,rx,rgdec,signal_type,space_packet_count,swath,swl,swst,tstmod,txpl,\
     txpl_,txprr,txprr_,txpsf";
const CAL_CSV_HEADER: &str = "azi,baq_n,baqmod,cal_iter,ele_count,cal_mode,\
     cal_p,cal_type,data_delay,number_of_quads,offset,packet_idx,pol,pri_count,\
     rank,rgdec,rx,signal_type,space_packet_count,swath,swl,swst,tstmod,txpl,\
     txpl_,txprr,txprr_,txpsf";

fn print_sorted<H: std::hash::Hash + Eq + Ord + std::fmt::Display>(
    state: &State,
    msg: &str,
    key_name: &str,
    val_name: &str,
    map: &HashMap<H, u64>,
    scale: f64,
) {
    let mut keys: Vec<&H> = map.keys().collect();
    keys.sort();
    // The C++ iterates an unordered_map (nondeterministic order); sorting
    // keeps reports stable.
    for k in keys {
        let v = map[k] as f64 / scale;
        if scale == 1.0 {
            log!(state, msg, (key_name, k), (val_name, map[k]));
        } else {
            log!(state, msg, (key_name, k), (val_name, fmt_g3(v)));
        }
    }
}

fn write_complex_cf(path: &Path, samples: &[Complex32]) -> Result<()> {
    let file = File::create(path)?;
    let mut w = std::io::BufWriter::new(file);
    for s in samples {
        w.write_all(&s.re.to_le_bytes())?;
        w.write_all(&s.im.to_le_bytes())?;
    }
    w.flush()?;
    Ok(())
}

fn remove_if_exists(path: &Path) {
    let _ = fs::remove_file(path);
}

fn run(cfg: &Config) -> Result<()> {
    let mut state = State {
        start_time: Some(Instant::now()),
        filename: cfg.filename.to_string_lossy().into_owned(),
        ..State::default()
    };

    // init_mmap
    let mapped = MappedFile::open(&cfg.filename)?;
    log!(
        state,
        "size",
        ("filesize", mapped.filesize()),
        ("filename", cfg.filename.display())
    );
    state.mmap_filesize = mapped.filesize();
    let data: &[u8] = mapped.bytes();

    // init_collect_packet_headers
    let packets = collect_packet_headers(data)?;
    log!(state, "collect", ("packets", packets.len()));
    state.header_offset = packets.offsets;
    state.header_data = packets.headers;

    if cfg.dump_headers {
        let mut out = std::io::stdout().lock();
        if let Err(e) = process_packet_headers(&mut out, &state.header_data, cfg.animate) {
            eprintln!("header dump failed: {e}");
            std::process::exit(1);
        }
        return Ok(());
    }

    // First pass: ancillary words, calibration/signal histograms.
    let anxillary_path = cfg.csv_dir.join("o_anxillary.csv");
    remove_if_exists(&anxillary_path);
    let mut anxillary = CsvWriter::new(anxillary_path, AncillaryData::csv_header());
    let mut sub_decoder = SubCommutatedDecoder::new();
    let mut map_ele: HashMap<u32, u64> = HashMap::new();
    let mut map_cal: HashMap<u32, u64> = HashMap::new();
    let mut map_sig: HashMap<u32, u64> = HashMap::new();
    let mut cal_count: usize = 0;
    for header in state.header_data.iter() {
        let h = PacketHeader::parse(header)?;
        if let Some(block) = sub_decoder.feed(
            h.sub_commutated_data as u16,
            h.sub_commutated_index as usize,
        )? {
            anxillary.append(&block.csv_row(h.space_packet_count))?;
        }
        // map_sig stays local, like the C++ version (only _map_cal and
        // _map_ele are mirrored into the global state).
        *map_sig.entry(h.ses_ssb_signal_type).or_insert(0) += 1;
        if h.is_calibration() {
            cal_count += 1;
            *map_cal.entry(h.cal_type()).or_insert(0) += 1;
            *state.map_cal.entry(h.cal_type()).or_insert(0) += 1;
            log!(
                state,
                "cal",
                ("cal_p", 1),
                ("cal_type", h.cal_type()),
                ("number_of_quads", h.number_of_quads),
                ("baq_mode", h.baq_mode),
                ("test_mode", h.test_mode)
            );
        } else {
            *map_ele.entry(h.elevation()).or_insert(0) += u64::from(h.number_of_quads);
            *state.map_ele.entry(h.elevation()).or_insert(0) += u64::from(h.number_of_quads);
        }
    }

    print_sorted(
        &state,
        "map_cal",
        "cal_type",
        "number_of_cal",
        &map_cal,
        1.0,
    );
    print_sorted(
        &state,
        "map_sig",
        "sig_type",
        "number_of_sig",
        &map_sig,
        1.0,
    );
    let mut ma = -1.0f64;
    let mut ma_ele: i64 = -1;
    {
        let mut keys: Vec<u32> = map_ele.keys().copied().collect();
        keys.sort();
        for ele in keys {
            let mquads = map_ele[&ele] as f64 / 1.0e6;
            if ma < mquads {
                ma = mquads;
                ma_ele = i64::from(ele);
            }
            log!(
                state,
                "map_ele",
                ("elevation_beam_address", ele),
                ("number_of_Mquads", mquads)
            );
        }
    }
    log!(state, "largest ele", ("ma_ele", ma_ele), ("ma", fmt_g3(ma)));
    log!(state, "calibrations", ("cal_count", cal_count));

    // Second pass: range-delay extent and azimuth histogram for ma_ele.
    let mut mi_data_delay = 10_000_000u32;
    let mut ma_data_delay = 0u32;
    let mut ma_data_end = 0u32;
    let mut ele_number_echoes: usize = 0;
    let mut map_azi: HashMap<u32, u64> = HashMap::new();
    for header in &state.header_data {
        let h = PacketHeader::parse(header)?;
        if !h.is_calibration() && i64::from(h.elevation()) == ma_ele {
            ele_number_echoes += 1;
            let dd = h.data_delay();
            mi_data_delay = mi_data_delay.min(dd);
            ma_data_delay = ma_data_delay.max(dd);
            ma_data_end = ma_data_end.max(dd + 2 * h.number_of_quads);
            *map_azi.entry(h.sab_ssb_azimuth_beam_address).or_insert(0) +=
                u64::from(h.number_of_quads);
        }
    }
    log!(
        state,
        "data_delay",
        ("mi_data_delay", mi_data_delay),
        ("ma_data_delay", ma_data_delay),
        ("ma_data_end", ma_data_end),
        ("ele_number_echoes", ele_number_echoes)
    );
    print_sorted(
        &state,
        "map_azi",
        "azi_beam_address",
        "number_of_Mquads",
        &map_azi,
        1.0e6,
    );
    if ele_number_echoes == 0 {
        // The C++ code allocates a negative-sized array here and crashes.
        return Err(Error::NoPackets(
            "no signal packets for the selected elevation beam",
        ));
    }

    // Allocate the range image, capped like the C++ ele_number_echoes = 512.
    let stored_echoes = ele_number_echoes.min(cfg.max_echoes);
    let n0 = ma_data_end as usize + (ma_data_delay - mi_data_delay) as usize;
    log!(
        state,
        "start big allocation",
        ("((ma_data_end)+(((ma_data_delay)-(mi_data_delay))))", n0),
        ("ele_number_echoes", stored_echoes)
    );
    let mut sar_image = vec![Complex32::default(); n0 * stored_echoes];
    log!(
        state,
        "end big allocation",
        (
            "(((1.00e-6f))*(n0)*(ele_number_echoes))",
            fmt_g3(1.0e-6 * n0 as f64 * stored_echoes as f64)
        )
    );

    let range_path = cfg.csv_dir.join("o_range.csv");
    let cal_range_path = cfg.csv_dir.join("o_cal_range.csv");
    remove_if_exists(&cfg.csv_dir.join("o_all.csv"));
    remove_if_exists(&range_path);
    remove_if_exists(&cal_range_path);
    let mut range_csv = CsvWriter::new(range_path, RANGE_CSV_HEADER);
    let mut cal_range_csv = CsvWriter::new(cal_range_path, CAL_CSV_HEADER);
    let mut cal_image = vec![Complex32::default(); CAL_N0 * cal_count];

    // Decode loop.
    let mut columns: Vec<Vec<f32>> = vec![Vec::new(); FIELD_NAMES.len()];
    let mut cal_iter: usize = 0;
    let mut ele_count: usize = 0;
    let mut cap_warned = false;
    for (packet_idx, header) in state.header_data.iter().enumerate() {
        let h = PacketHeader::parse(header)?;
        let offset = state.header_offset[packet_idx];
        h.check_sync(packet_idx)?;
        for (col, v) in columns.iter_mut().zip(h.values()) {
            col.push(v as f32);
        }
        let ctx = RowContext {
            packet_idx,
            offset,
            cal_iter,
            ele_count,
        };
        let mut reader = BitReader::new(data, offset + HEADER_LEN);
        let quads = h.number_of_quads as usize;
        let decoded = if h.is_calibration() {
            if h.baq_mode != 0 {
                return Err(Error::UnsupportedBaqMode {
                    packet_idx,
                    baq_mode: h.baq_mode,
                });
            }
            decode_type_a_or_b(&mut reader, quads)
        } else if i64::from(h.elevation()) == ma_ele {
            // modes 12/13/14 carry per-block bit-rate codes (FDBAQ);
            // modes 3/4/5 are fixed-rate BAQ without them.
            match h.baq_mode {
                0 => decode_type_a_or_b(&mut reader, quads),
                3 => decode_baq3(&mut reader, quads),
                4 => decode_baq4(&mut reader, quads),
                5 => decode_baq5(&mut reader, quads),
                12..=14 => decode_fdbaq(&mut reader, quads),
                _ => {
                    return Err(Error::UnsupportedBaqMode {
                        packet_idx,
                        baq_mode: h.baq_mode,
                    });
                }
            }
        } else {
            continue;
        };
        match decoded {
            Ok(packet) => {
                let n = packet.sample_count();
                if n != 2 * quads {
                    log!(
                        state,
                        "unexpected number of quads",
                        ("n", n),
                        ("number_of_quads", quads)
                    );
                }
                let samples = packet.to_complex();
                if h.is_calibration() {
                    let row = &mut cal_image[cal_iter * CAL_N0..(cal_iter + 1) * CAL_N0];
                    let take = samples.len().min(CAL_N0);
                    row[..take].copy_from_slice(&samples[..take]);
                    if samples.len() > CAL_N0 && !cap_warned {
                        log!(
                            state,
                            "cal row truncated",
                            ("samples", samples.len()),
                            ("cal_n0", CAL_N0)
                        );
                    }
                    cal_range_csv.append(&cal_row(&h, &ctx))?;
                    cal_iter += 1;
                } else {
                    if ele_count < stored_echoes {
                        let base = (h.data_delay() - mi_data_delay) as usize + n0 * ele_count;
                        let end = (base + samples.len()).min((ele_count + 1) * n0);
                        sar_image[base..end].copy_from_slice(&samples[..end - base]);
                    } else if !cap_warned {
                        cap_warned = true;
                        log!(
                            state,
                            "echo cap reached, decoding only",
                            ("max_echoes", stored_echoes)
                        );
                    }
                    range_csv.append(&range_row(&h, &ctx))?;
                    ele_count += 1;
                }
            }
            Err(e) if !e.is_fatal() => {
                // Mirror `catch (std::out_of_range)`: snapshot the header
                // table collected so far and continue with the next packet.
                log!(
                    state,
                    "exception",
                    ("packet_idx", packet_idx),
                    ("static_cast<int>(cal_p)", h.sab_ssb_calibration_p)
                );
                for (name, col) in FIELD_NAMES.iter().zip(columns.iter()) {
                    state.packet_header.insert(name.to_string(), col.clone());
                }
                let _ = e;
            }
            Err(e) => return Err(e),
        }
    }

    // Store the echo and calibration images.
    let cf_dir = if cfg.cf_dir.is_dir() {
        cfg.cf_dir.clone()
    } else {
        log!(
            state,
            "cf dir missing, using csv dir",
            ("cf_dir", cfg.cf_dir.display())
        );
        cfg.csv_dir.clone()
    };
    let nbytes = n0 * stored_echoes * std::mem::size_of::<Complex32>();
    log!(state, "store echo", ("nbytes", nbytes));
    let sar_path = cf_dir.join(format!("o_range{n0}_echoes{stored_echoes}.cf"));
    match write_complex_cf(&sar_path, &sar_image) {
        Ok(()) => log!(state, "store echo finished", ("path", sar_path.display())),
        Err(e) => {
            let fallback = cfg
                .csv_dir
                .join(format!("o_range{n0}_echoes{stored_echoes}.cf"));
            log!(state, "store echo failed, trying fallback", ("error", e));
            write_complex_cf(&fallback, &sar_image)?;
            log!(state, "store echo finished", ("path", fallback.display()));
        }
    }

    let nbytes = CAL_N0 * cal_count * std::mem::size_of::<Complex32>();
    log!(state, "store cal", ("nbytes", nbytes));
    let cal_path = cf_dir.join(format!("o_cal_range{CAL_N0}_echoes{cal_count}.cf"));
    match write_complex_cf(&cal_path, &cal_image) {
        Ok(()) => log!(state, "store cal finished", ("path", cal_path.display())),
        Err(e) => {
            let fallback = cfg
                .csv_dir
                .join(format!("o_cal_range{CAL_N0}_echoes{cal_count}.cf"));
            log!(state, "store cal failed, trying fallback", ("error", e));
            write_complex_cf(&fallback, &cal_image)?;
            log!(state, "store cal finished", ("path", fallback.display()));
        }
    }

    if cfg.export_headers {
        let mut table = PacketHeaderTable::new();
        for (name, col) in FIELD_NAMES.iter().zip(columns.iter()) {
            table.insert(name.to_string(), col.clone());
        }
        let path = cfg.csv_dir.join("o_packet_header.csv");
        export_packet_headers_csv(&path, &table)?;
        log!(state, "exported packet headers", ("path", path.display()));
    }
    Ok(())
    // `mapped` is unmapped on drop (`destroy_mmap`); images are freed.
}

fn main() {
    let program = std::env::args()
        .next()
        .unwrap_or_else(|| "copernicus-radar".to_string());
    let args: Vec<String> = std::env::args().skip(1).collect();
    let cfg = match Config::parse(&args) {
        Ok(cfg) => cfg,
        Err(e) if e == "help" => {
            println!("{}", Config::usage(&program));
            return;
        }
        Err(e) => {
            eprintln!("{e}\n{}", Config::usage(&program));
            std::process::exit(2);
        }
    };
    if let Err(e) = run(&cfg) {
        eprintln!("error: {e}");
        std::process::exit(1);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn csv_row_builders_emit_expected_columns() {
        let mut header = [0u8; HEADER_LEN];
        header[12..16].copy_from_slice(&[0x35, 0x2E, 0xF8, 0x53]);
        header[65] = 0x00;
        header[66] = 0x10;
        header[37] = 12;
        header[59] = 0x10; // signal packet, ele nibble in p[60]
        header[60] = 0x20;
        let h = PacketHeader::parse(&header).unwrap();
        let ctx = RowContext {
            packet_idx: 3,
            offset: 100,
            cal_iter: 0,
            ele_count: 5,
        };
        let range_cols = RANGE_CSV_HEADER.split(',').count();
        assert_eq!(range_row(&h, &ctx).split(',').count(), range_cols);
        let cal_cols = CAL_CSV_HEADER.split(',').count();
        assert_eq!(cal_row(&h, &ctx).split(',').count(), cal_cols);
        assert!(range_row(&h, &ctx).contains(",100,3,"));
    }

    #[test]
    fn config_defaults_and_flags() {
        let cfg = Config::parse(&[]).unwrap();
        assert_eq!(cfg.max_echoes, DEFAULT_MAX_ECHOES);
        assert!(!cfg.dump_headers);
        let cfg = Config::parse(&[
            "in.dat".to_string(),
            "--max-echoes".to_string(),
            "4".to_string(),
            "--dump-headers".to_string(),
            "--csv-dir".to_string(),
            "/tmp/x".to_string(),
        ])
        .unwrap();
        assert_eq!(cfg.filename, PathBuf::from("in.dat"));
        assert_eq!(cfg.max_echoes, 4);
        assert!(cfg.dump_headers);
        assert_eq!(cfg.csv_dir, PathBuf::from("/tmp/x"));
        assert!(Config::parse(&["--bogus".to_string()]).is_err());
    }
}
