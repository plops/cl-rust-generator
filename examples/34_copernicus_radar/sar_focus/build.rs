//! Link against NVIDIA cuFFT (already in the CUDA toolkit, no new package).
//!
//! The `cufft` calls themselves are declared as minimal `extern "C"` FFI in
//! `08_cufft.rs`, operating on `cuda-oxide` device pointers. `CUDA_ROOT` (or
//! `CUDA_PATH`) may override the toolkit location.

fn main() {
    let cuda_root = std::env::var("CUDA_ROOT")
        .or_else(|_| std::env::var("CUDA_PATH"))
        .unwrap_or_else(|_| "/usr/local/cuda".to_string());
    println!("cargo:rustc-link-search=native={cuda_root}/lib64");
    println!("cargo:rustc-link-search=native={cuda_root}/targets/x86_64-linux/lib");
    println!("cargo:rustc-link-lib=cufft");
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed=CUDA_ROOT");
    println!("cargo:rerun-if-env-changed=CUDA_PATH");
}
