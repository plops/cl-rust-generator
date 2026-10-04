//! Minimales cuFFT-FFI für GPU-FFTs auf `cuda-oxide`-Buffern.
//!
//! `cudarc` bringt (Stand DeepWiki-Recherche) keine cuFFT-Bindings mit, und
//! die comunitàren cuFFT-Crates sind unreife Alphas — daher 60 Zeilen
//! stabiles FFI gegen NVIDIAs `libcufft.so` (Teil des vorhandenen
//! Toolkits, Link via `build.rs`). Das ist „FFT via Bibliothek“, nicht
//! handgeschrieben: Die Pläne laufen auf NVIDIAs Implementierung.
//!
//! Interop: `cuda-oxide` nutzt den Primary Context; `cu_deviceptr()` und
//! `cu_stream()` liefern die Handles, die cuFFT erwartet. Alle Pläne werden
//! an den Pipeline-Stream gebunden (`cufftSetStream`), sodass Kernel und
//! FFTs korrekt geordnet sind.

use crate::types::{Error, err};
use cuda_core::CudaStream;
use std::ffi::c_void;

type CufftHandle = i32;
const CUFFT_FORWARD: i32 = -1;
const CUFFT_INVERSE: i32 = 1;
const CUFFT_C2C: i32 = 0x29;

unsafe extern "C" {
    fn cufftPlan1d(plan: *mut CufftHandle, nx: i32, type_: i32, batch: i32) -> i32;
    #[allow(clippy::too_many_arguments)]
    fn cufftPlanMany(
        plan: *mut CufftHandle,
        rank: i32,
        n: *const i32,
        inembed: *const i32,
        istride: i32,
        idist: i32,
        onembed: *const i32,
        ostride: i32,
        odist: i32,
        type_: i32,
        batch: i32,
    ) -> i32;
    fn cufftExecC2C(plan: CufftHandle, idata: *mut c_void, odata: *mut c_void, dir: i32) -> i32;
    fn cufftSetStream(plan: CufftHandle, stream: *mut c_void) -> i32;
    fn cufftDestroy(plan: CufftHandle) -> i32;
}

fn check(code: i32, what: &str) -> Result<(), Error> {
    if code == 0 {
        return Ok(());
    }
    let why = match code {
        1 => "ungültiger Plan",
        2 => "Allokation fehlgeschlagen",
        3 => "ungültiger Typ",
        4 => "ungültiger Wert",
        5 => "interner Treiberfehler",
        6 => "Ausführung fehlgeschlagen",
        7 => "Setup fehlgeschlagen",
        8 => "ungültige Größe",
        9 => "nicht-aligned",
        10 => "unvollständige Parameter",
        11 => "ungültiges Device",
        _ => "unbekannt",
    };
    Err(Error(format!("cuFFT {what}: {why} ({code})")))
}

/// FFT-Richtung.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Direction {
    Forward,
    Inverse,
}

/// cuFFT-Plan für komplexe 1D-FFTs (C2C, `f32`, in-place-fähig).
pub struct CufftPlan {
    handle: CufftHandle,
}

impl CufftPlan {
    /// `batch` kontiguierliche FFTs der Länge `nx` (Zeilen-FFTs).
    pub fn plan_rows(nx: usize, batch: usize) -> Result<Self, Error> {
        let mut handle = 0;
        // SAFETY: cuFFT-Plan-Erzeugung mit gültigen Größen.
        let code = unsafe {
            cufftPlan1d(
                &mut handle,
                i32::try_from(nx).map_err(err)?,
                CUFFT_C2C,
                i32::try_from(batch).map_err(err)?,
            )
        };
        check(code, "plan_rows")?;
        Ok(Self { handle })
    }

    /// `batch` FFTs der Länge `n` mit Schrittweite `stride` und
    /// Batch-Abstand `dist` (Spalten-FFTs: `stride` = Zeilenlänge).
    pub fn plan_strided(n: usize, stride: usize, dist: usize, batch: usize) -> Result<Self, Error> {
        let mut handle = 0;
        let nn = i32::try_from(n).map_err(err)?;
        // SAFETY: 1D-Plan mit explizitem Layout, Einbettung = Länge.
        let code = unsafe {
            cufftPlanMany(
                &mut handle,
                1,
                &nn,
                &nn,
                i32::try_from(stride).map_err(err)?,
                i32::try_from(dist).map_err(err)?,
                &nn,
                i32::try_from(stride).map_err(err)?,
                i32::try_from(dist).map_err(err)?,
                CUFFT_C2C,
                i32::try_from(batch).map_err(err)?,
            )
        };
        check(code, "plan_strided")?;
        Ok(Self { handle })
    }

    /// Bindet den Plan an den `cuda-oxide`-Stream (Ordnung vs. Kernel).
    pub fn set_stream(&mut self, stream: &CudaStream) -> Result<(), Error> {
        // SAFETY: gültiger Plan + lebendiger Stream; CUstream-Handles sind
        // mit cudaStream_t austauschbar (Primary Context).
        let code = unsafe { cufftSetStream(self.handle, stream.cu_stream() as *mut c_void) };
        check(code, "set_stream")
    }

    /// Führt den Plan in-place auf `device_ptr` aus (Gerätezeiger).
    ///
    /// # Safety
    /// `device_ptr` muss ein gültiger Device-Zeiger auf genug `cufftComplex`
    /// sein, allokiert im selben Context.
    pub unsafe fn exec_inplace(&self, device_ptr: u64, dir: Direction) -> Result<(), Error> {
        let code = unsafe {
            cufftExecC2C(
                self.handle,
                device_ptr as *mut c_void,
                device_ptr as *mut c_void,
                match dir {
                    Direction::Forward => CUFFT_FORWARD,
                    Direction::Inverse => CUFFT_INVERSE,
                },
            )
        };
        check(code, "exec")
    }
}

impl Drop for CufftPlan {
    fn drop(&mut self) {
        // SAFETY: gültiger, genau einmal zerstörter Plan.
        unsafe {
            cufftDestroy(self.handle);
        }
    }
}
