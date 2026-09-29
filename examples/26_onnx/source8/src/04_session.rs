//! `04_session` — ONNX-Runtime-Session: Execution Provider (CPU/CUDA),
//! Threads, Optimierungsstufe, Warmup-Probe und Input-Geometrie.
//!
//! CUDA gilt erst als aktiv, wenn eine Probe-Inferenz gelingt: Der
//! Session-Commit prüft nur die Registrierbarkeit, fehlendes cuDNN fällt
//! erst beim ersten Conv auf (Learning aus source7).

use ort::ep::CPU;
#[cfg(feature = "cuda")]
use ort::ep::CUDA;
use ort::session::Session;
use ort::session::builder::GraphOptimizationLevel;
use ort::value::TensorRef;

/// Gewünschtes Rechengerät.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Device {
    /// CUDA versuchen, sonst CPU.
    Auto,
    Cpu,
    Cuda,
}

impl Device {
    pub fn parse(s: &str) -> Result<Self, String> {
        match s {
            "auto" => Ok(Self::Auto),
            "cpu" => Ok(Self::Cpu),
            "cuda" => Ok(Self::Cuda),
            _ => Err(format!("unbekanntes Gerät '{s}' (auto|cpu|cuda)")),
        }
    }
}

/// Geladenes Modell samt Metadaten.
pub struct Model {
    pub session: Session,
    pub input_name: String,
    /// Input-Breite/-Höhe aus dem Graphen (statisch exportiert).
    pub in_w: usize,
    pub in_h: usize,
    /// Tatsächlich rechnender Provider (`CPU`/`CUDA`).
    pub provider: &'static str,
}

impl Model {
    /// Baut eine Session aus ONNX-Bytes. `threads = 0` → ORT-Default.
    pub fn load(bytes: &[u8], device: Device, threads: usize) -> Result<Self, String> {
        if device != Device::Cpu {
            match cuda_session(bytes).and_then(|s| Self::finish(s, "CUDA")) {
                Ok(m) => return Ok(m),
                Err(e) if device == Device::Cuda => return Err(e),
                Err(e) => eprintln!("hinweis: CUDA nicht nutzbar ({e}), nutze CPU"),
            }
        }
        let err = |e: ort::Error| e.to_string();
        let mut b = Session::builder()
            .map_err(err)?
            .with_optimization_level(GraphOptimizationLevel::Level3)
            .map_err(|e| e.to_string())?
            .with_execution_providers([CPU::default().build()])
            .map_err(|e| e.to_string())?;
        if threads > 0 {
            b = b.with_intra_threads(threads).map_err(|e| e.to_string())?;
        }
        let s = b.commit_from_memory(bytes).map_err(err)?;
        Self::finish(s, "CPU")
    }

    /// Liest die Input-Geometrie und beweist per Probe-Lauf die Funktion.
    fn finish(mut session: Session, provider: &'static str) -> Result<Self, String> {
        let input = &session.inputs()[0];
        let input_name = input.name().to_string();
        let shape: Vec<i64> = input
            .dtype()
            .tensor_shape()
            .ok_or("Input ist kein Tensor")?
            .iter()
            .copied()
            .collect();
        let [1, 3, h, w] = shape[..] else {
            return Err(format!("erwarte Input [1,3,H,W], habe {shape:?}"));
        };
        if h <= 0 || w <= 0 {
            return Err("dynamische Input-Größe nicht unterstützt".into());
        }
        let (in_w, in_h) = (w as usize, h as usize);
        let zeros = vec![0.0f32; 3 * in_w * in_h];
        let probe = TensorRef::from_array_view(([1, 3, in_h, in_w], &zeros[..]))
            .map_err(|e| e.to_string())?;
        session
            .run(ort::inputs![input_name.as_str() => probe])
            .map_err(|e| format!("Probe-Inferenz ({provider}): {e}"))?;
        Ok(Self {
            session,
            input_name,
            in_w,
            in_h,
            provider,
        })
    }
}

#[cfg(feature = "cuda")]
fn cuda_session(bytes: &[u8]) -> Result<Session, String> {
    Session::builder()
        .and_then(|b| Ok(b.with_optimization_level(GraphOptimizationLevel::Level3)?))
        .and_then(|b| Ok(b.with_execution_providers([CUDA::default().build().error_on_failure()])?))
        .and_then(|mut b| b.commit_from_memory(bytes))
        .map_err(|e| e.to_string())
}

#[cfg(not(feature = "cuda"))]
fn cuda_session(_bytes: &[u8]) -> Result<Session, String> {
    Err("ohne --features cuda gebaut".into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn device_parse() {
        assert_eq!(Device::parse("cuda"), Ok(Device::Cuda));
        assert_eq!(Device::parse("auto"), Ok(Device::Auto));
        assert!(Device::parse("tpu").is_err());
    }

    #[test]
    fn garbage_model_is_an_error_not_a_panic() {
        assert!(Model::load(b"not onnx", Device::Cpu, 1).is_err());
    }
}
