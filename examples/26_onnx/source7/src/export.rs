//! export.rs — Dumpt `faces_db.bin` in einfache Rohformate für Python/cuML.
//!
//! Schreibt drei Dateien in ein Ausgabeverzeichnis (Default `export/`):
//!   * `emb.f32` — n·512 little-endian float32, zeilenweise (row-major),
//!     exakt die in der DB gespeicherten Embeddings.
//!   * `labels.i32` — n little-endian int32 Personen-IDs (Ground-Truth).
//!   * `meta.json` — `{ "n": .., "dim": 512, "n_persons": .. }`.
//!
//! So kann NumPy die Matrix per `np.fromfile(..., dtype='<f4').reshape(n,512)`
//! laden — ohne bincode in Python nachzubauen. Kein Fenster, keine Modelle.
//!
//! Bedienung: `export [--db faces_db.bin] [--out export]`

#![allow(dead_code)]

#[path = "06_face_database.rs"]
mod db;
#[path = "08_latent.rs"]
mod latent;
#[path = "01_types.rs"]
mod types;

use db::FaceDatabase;
use latent::LatentData;
use std::io::Write;

fn main() {
    let mut db_path = "faces_db.bin".to_string();
    let mut out_dir = "export".to_string();
    let mut it = std::env::args().skip(1);
    while let Some(f) = it.next() {
        match f.as_str() {
            "--db" => db_path = it.next().unwrap_or_else(|| bail("--db braucht Wert")),
            "--out" => out_dir = it.next().unwrap_or_else(|| bail("--out braucht Wert")),
            "--help" | "-h" => {
                println!("export [--db faces_db.bin] [--out export]");
                return;
            }
            x => bail(&format!("unbekannt: {x}")),
        }
    }

    let database = FaceDatabase::load(&db_path);
    let ld = LatentData::from_database(&database);
    let n = ld.len();
    let dim = ld.data.ncols();
    println!(
        "geladen: {n} Exemplare, {} Personen aus {db_path}",
        ld.n_persons
    );
    if n == 0 {
        bail("leere DB");
    }

    std::fs::create_dir_all(&out_dir).unwrap_or_else(|e| bail(&format!("mkdir: {e}")));

    // Embeddings row-major als f32.
    let emb_path = format!("{out_dir}/emb.f32");
    {
        let mut f = std::io::BufWriter::new(
            std::fs::File::create(&emb_path).unwrap_or_else(|e| bail(&format!("emb: {e}"))),
        );
        // data ist row-major (Array2 standard layout); als slice schreiben.
        let slice = ld
            .data
            .as_slice()
            .expect("Array2 sollte contiguous/standard-layout sein");
        let bytes: &[u8] = bytemuck_cast(slice);
        f.write_all(bytes)
            .unwrap_or_else(|e| bail(&format!("emb write: {e}")));
    }

    // Labels (Person-ID) als i32.
    let lab_path = format!("{out_dir}/labels.i32");
    {
        let mut f = std::io::BufWriter::new(
            std::fs::File::create(&lab_path).unwrap_or_else(|e| bail(&format!("labels: {e}"))),
        );
        for m in &ld.meta {
            let id = m.person_id as i32;
            f.write_all(&id.to_le_bytes())
                .unwrap_or_else(|e| bail(&format!("labels write: {e}")));
        }
    }

    // Meta.
    let meta_path = format!("{out_dir}/meta.json");
    std::fs::write(
        &meta_path,
        format!(
            "{{\"n\": {n}, \"dim\": {dim}, \"n_persons\": {}}}\n",
            ld.n_persons
        ),
    )
    .unwrap_or_else(|e| bail(&format!("meta: {e}")));

    println!("geschrieben: {emb_path} ({n}x{dim} f32), {lab_path} (i32), {meta_path}");
}

/// Reinterpretiert einen `&[f32]` als `&[u8]` (little-endian Host = x86_64).
fn bytemuck_cast(s: &[f32]) -> &[u8] {
    // Sicher: f32 hat keine Padding-Bytes; Länge in Bytes = 4·len.
    unsafe { std::slice::from_raw_parts(s.as_ptr().cast::<u8>(), std::mem::size_of_val(s)) }
}

fn bail(msg: &str) -> ! {
    eprintln!("{msg}");
    std::process::exit(2)
}
