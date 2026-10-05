//! Packet-header table export (via `--export-headers`).
//!
//! Collects one float column per header field over all packets and writes
//! the table to a CSV file with [`export_packet_headers_csv`]. This replaces
//! the C++ module's embedded IPython shell, which the C++ `main` never
//! invoked.

use std::collections::BTreeMap;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::Path;

use crate::error::Result;

/// One float column per header field (`state._packet_header`).
pub type PacketHeaderTable = BTreeMap<String, Vec<f32>>;

/// Write the packet-header table to `path` as CSV: field names as the header
/// row, one row per packet.
pub fn export_packet_headers_csv(path: &Path, table: &PacketHeaderTable) -> Result<()> {
    let file = File::create(path)?;
    let mut w = BufWriter::new(file);
    let names: Vec<&str> = table.keys().map(String::as_str).collect();
    writeln!(w, "{}", names.join(","))?;
    let rows = table.values().map(Vec::len).max().unwrap_or(0);
    for row in 0..rows {
        let values: Vec<String> = names
            .iter()
            .map(|n| {
                table
                    .get(*n)
                    .and_then(|col| col.get(row))
                    .map_or_else(String::new, |v| format!("{v}"))
            })
            .collect();
        writeln!(w, "{}", values.join(","))?;
    }
    w.flush()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exports_columns_row_wise() {
        let dir = std::env::temp_dir().join("copernicus_radar_test_header_export");
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("headers.csv");
        let mut table = PacketHeaderTable::new();
        table.insert("b_field".to_string(), vec![1.0, 2.0]);
        table.insert("a_field".to_string(), vec![3.0, 4.0]);
        export_packet_headers_csv(&path, &table).unwrap();
        let text = std::fs::read_to_string(&path).unwrap();
        assert_eq!(text, "a_field,b_field\n3,1\n4,2\n");
        std::fs::remove_file(&path).unwrap();
    }
}
