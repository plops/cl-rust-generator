(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)

;; gen11_fwproj.lisp -- generates the fw_pico2 Cargo project files
;; (Cargo.toml, memory.x, .cargo/config.toml, build.rs).
;; Pinned against embassy-rp 0.10.0 / embassy-executor 0.10.0 /
;; embassy-time 0.5.1 / embassy-sync 0.8.0 (verified via crates.io
;; 2026-09-07). Chip feature rp235xa, target thumbv8m.main-none-eabihf.
;; Note: no `-Tlink-rp.x`: embassy-rp 0.10 emits link-rp.x (BOOT2
;; section) for RP2040 only; RP235x boots via the IMAGE_DEF block loop
;; (.start_block) that embassy-rp emits itself.

(let ((base (asdf:system-relative-pathname 'cl-rust-generator
					   #P"examples/23_embassy_pico/fw_pico2/")))
  (ensure-directories-exist (merge-pathnames #P"src/main.rs" base))

  (defun fw-write (rel content)
    (let ((fn (merge-pathnames rel base)))
      (ensure-directories-exist fn)
      (with-open-file (s fn :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
	(write-sequence content s))
      fn))

  (fw-write #P"Cargo.toml"
	    "[package]
name = \"pico2-fw\"
version = \"0.1.0\"
edition = \"2024\"
description = \"Pico 2 Embassy firmware: ADC+PIO-cap+PWM+HSTX, UART proto link (generated from gen10_firmware.lisp, do not edit by hand)\"
license = \"MIT OR Apache-2.0\"

[[bin]]
name = \"pico2-fw\"
path = \"src/main.rs\"

[dependencies]
embassy-rp = { version = \"0.10.0\", features = [\"rp235xa\", \"critical-section-impl\", \"time-driver\", \"defmt\", \"unstable-pac\"] }
embassy-executor = { version = \"0.10.0\", features = [\"platform-cortex-m\", \"executor-thread\", \"defmt\"] }
embassy-time = { version = \"0.5.1\", features = [\"defmt\"] }
embassy-sync = { version = \"0.8.0\", features = [\"defmt\"] }
embassy-futures = \"0.1.2\"
defmt = \"1.0.1\"
defmt-rtt = \"1.0.0\"
panic-probe = { version = \"1.0.0\", features = [\"print-defmt\"] }
cortex-m = { version = \"0.7.6\", features = [\"inline-asm\"] }
cortex-m-rt = \"0.7.0\"
static_cell = \"2.1\"
pio = \"0.3\"
embedded-io-async = { version = \"0.7.0\", features = [\"defmt\"] }
pico-link-proto = { path = \"../proto\" }

[profile.release]
opt-level = \"s\"
lto = true
debug = true
")

  (fw-write #P"memory.x"
	    "/* Pico 2 (RP2350A) memory layout: 4 MB external QSPI flash, 520 KB SRAM.
   FLASH starts at 0x200 (not 0x100): the RP2350 vector table is 276 bytes,
   so cortex-m-rt requires 512-byte alignment. The BOOT2 slot holds
   IMAGE_DEF (.start_block, placed by link-rp235x.x); RP235x links no BOOT2. */
MEMORY {
    BOOT2 : ORIGIN = 0x10000000, LENGTH = 0x100
    FLASH : ORIGIN = 0x10000200, LENGTH = 4096K - 0x200
    RAM   : ORIGIN = 0x20000000, LENGTH = 520K
}
")

  (fw-write #P".cargo/config.toml"
	    "[target.'cfg(all(target_arch = \"arm\", target_os = \"none\"))']
runner = \"probe-rs run --chip RP235x\"

[build]
target = \"thumbv8m.main-none-eabihf\"

[env]
DEFMT_LOG = \"debug\"
")

  (fw-write #P"build.rs"
	    "//! Copies memory.x into OUT_DIR and passes link args.
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
    let out = &PathBuf::from(env::var_os(\"OUT_DIR\").unwrap());
    File::create(out.join(\"memory.x\"))
        .unwrap()
        .write_all(include_bytes!(\"memory.x\"))
        .unwrap();
    println!(\"cargo:rustc-link-search={}\", out.display());
    println!(\"cargo:rerun-if-changed=memory.x\");
    println!(\"cargo:rerun-if-changed=build.rs\");
    // RP235x: embassy-rp puts IMAGE_DEF in .start_block. Left alone it is
    // an orphan whose size (not a multiple of 8) misaligns .text and trips
    // cortex-m-rt's vector-table alignment ASSERT (rust-lld also does not
    // shift later sections for INSERT AFTER). Pin it at flash offset 0
    // (the BOOT2 slot, unused on RP235x): vector table and .text keep
    // their default places and the ROM finds IMAGE_DEF at offset 0.
    let rp235x = \"SECTIONS {\\n    .start_block ORIGIN(BOOT2) : {\\n        KEEP(*(.start_block .start_block.*));\\n    } > BOOT2\\n}\\n\";
    File::create(out.join(\"link-rp235x.x\"))
        .unwrap()
        .write_all(rp235x.as_bytes())
        .unwrap();
    println!(\"cargo:rustc-link-arg-bins=--nmagic\");
    println!(\"cargo:rustc-link-arg-bins=-Tlink.x\");
    println!(\"cargo:rustc-link-arg-bins=-Tlink-rp235x.x\");
    println!(\"cargo:rustc-link-arg-bins=-Tdefmt.x\");
}
"))
