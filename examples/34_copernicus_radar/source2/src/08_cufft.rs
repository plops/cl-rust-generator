//! Schlankes, sicheres FFI-Binding gegen NVIDIAs `libcufft.so` für 1D-Batch-FFTs
//! (Zeilen-FFTs und strided Spalten-FFTs) auf einem `cuda_core::CudaStream`.

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
    Err(Error(format!("cuFFT-Fehler in {what}: Code {code}")))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Direction {
    Forward,
    Inverse,
}

/// RAII-Wrapper für einen 1D-Batch-cuFFT-Plan (`C2C`, `f32`).
pub struct CufftPlan {
    handle: CufftHandle,
}

impl CufftPlan {
    /// Plant `batch` zusammenhängende Zeilen-FFTs der Länge `nx`.
    pub fn plan_rows(nx: usize, batch: usize) -> Result<Self, Error> {
        let mut handle = 0;
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

    /// Plant `batch` strided Spalten-FFTs der Länge `n` mit Schrittweite `stride` und Abstand `dist`.
    pub fn plan_strided(n: usize, stride: usize, dist: usize, batch: usize) -> Result<Self, Error> {
        let mut handle = 0;
        let nn = i32::try_from(n).map_err(err)?;
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

    /// Bindet den Plan an den gegebenen `CudaStream`.
    pub fn set_stream(&mut self, stream: &CudaStream) -> Result<(), Error> {
        let code = unsafe { cufftSetStream(self.handle, stream.cu_stream() as *mut c_void) };
        check(code, "set_stream")
    }

    /// Führt die FFT in-place auf dem Device-Puffer `device_ptr` aus.
    ///
    /// # Safety
    /// `device_ptr` muss auf einen gültigen `DeviceBuffer<Complex32>` ausreichender Größe zeigen.
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
        check(code, "exec_inplace")
    }
}

impl Drop for CufftPlan {
    fn drop(&mut self) {
        unsafe {
            cufftDestroy(self.handle);
        }
    }
}
