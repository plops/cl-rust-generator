//! Write the sample-based demo songs to `songs/` (they reference `samples/`, see README).
//! `cargo run --example write_songs`

use dnb_tracker::{demo_songs, song_io};
use std::path::Path;

fn main() -> anyhow::Result<()> {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("songs");
    std::fs::create_dir_all(&dir)?;
    for (file, song) in demo_songs::mixed_songs() {
        let path = dir.join(file);
        song_io::save(&path, &song)?;
        println!(
            "{} [{}] {}",
            path.display(),
            song.sources().label(),
            song.title
        );
    }
    Ok(())
}
