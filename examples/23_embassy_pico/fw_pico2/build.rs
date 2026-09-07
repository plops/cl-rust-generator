//! Copies memory.x into OUT_DIR and passes link args.
//! RP235x note: embassy-rp 0.10 emits link-rp.x (BOOT2 section) for
//! RP2040 only, so we link -Tlink.x (cortex-m-rt) + -Tdefmt.x only.
//! RP235x boots via the IMAGE_DEF block loop (.start_block) that
//! embassy-rp provides; no BOOT2 blob is linked (BOOT2 region in
//! memory.x stays unused).
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

fn main() {
    let out = &PathBuf::from(env::var_os("OUT_DIR").unwrap());
    File::create(out.join("memory.x"))
        .unwrap()
        .write_all(include_bytes!("memory.x"))
        .unwrap();
    println!("cargo:rustc-link-search={}", out.display());
    println!("cargo:rerun-if-changed=memory.x");
    println!("cargo:rerun-if-changed=build.rs");
    // RP235x: embassy-rp puts IMAGE_DEF in .start_block. Left alone it is
    // an orphan whose size (not a multiple of 8) misaligns .text and trips
    // cortex-m-rt's vector-table alignment ASSERT (rust-lld also does not
    // shift later sections for INSERT AFTER). Pin it at flash offset 0
    // (the BOOT2 slot, unused on RP235x): vector table and .text keep
    // their default places and the ROM finds IMAGE_DEF at offset 0.
    let rp235x = "SECTIONS {\n    .start_block ORIGIN(BOOT2) : {\n        KEEP(*(.start_block .start_block.*));\n    } > BOOT2\n}\n";
    File::create(out.join("link-rp235x.x"))
        .unwrap()
        .write_all(rp235x.as_bytes())
        .unwrap();
    println!("cargo:rustc-link-arg-bins=--nmagic");
    println!("cargo:rustc-link-arg-bins=-Tlink.x");
    println!("cargo:rustc-link-arg-bins=-Tlink-rp235x.x");
    println!("cargo:rustc-link-arg-bins=-Tdefmt.x");
}
