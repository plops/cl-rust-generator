//! `09_models` — Modellpfade, Modellwahl und lazy Session-Cache.
//!
//! `auto` nimmt das sprachspezifische Modell aus `02_lang`
//! (Kyrillisch/Thai/… kennt nur das jeweilige v5-Modell), `universal`
//! immer PP-OCRv6 — Taste `M` vergleicht beide. Sessions werden erst
//! beim ersten Gebrauch geladen und dann wiederverwendet.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::detect::Detector;
use crate::lang::{Lang, UNIVERSAL};
use crate::recognize::{Recognizer, load_dict};

/// Detektionsmodell (DBNet, alle Sprachen).
pub const DET_MODEL: &str = "PP-OCRv6_small_det_onnx";

/// Erkennungsmodell-Wahl.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum ModelChoice {
    /// Sprachspezifisches Modell aus der Sprachtabelle.
    #[default]
    Auto,
    /// Immer PP-OCRv6 (zum Vergleich).
    Universal,
}

/// Erkennungsmodell-Ordner für Sprache + Wahl.
#[must_use]
pub fn rec_model_for(lang: &Lang, choice: ModelChoice) -> &'static str {
    match choice {
        ModelChoice::Auto => lang.model,
        ModelChoice::Universal => UNIVERSAL,
    }
}

/// Modellverzeichnis mit lazy Detektor- und Erkenner-Cache.
pub struct Models {
    dir: PathBuf,
    detector: Option<Detector>,
    rec: HashMap<String, Recognizer>,
    dicts: HashMap<String, Vec<String>>,
}

impl Models {
    /// Öffnet ein Modellverzeichnis (`<dir>/<modell>/inference.{onnx,yml}`).
    #[must_use]
    pub fn open(dir: &Path) -> Self {
        Self {
            dir: dir.to_path_buf(),
            detector: None,
            rec: HashMap::new(),
            dicts: HashMap::new(),
        }
    }

    fn onnx_path(&self, model: &str) -> PathBuf {
        self.dir.join(model).join("inference.onnx")
    }

    fn yml_path(&self, model: &str) -> PathBuf {
        self.dir.join(model).join("inference.yml")
    }

    /// Detektor (wird beim ersten Aufruf geladen).
    pub fn detector(&mut self) -> Result<&mut Detector, String> {
        if self.detector.is_none() {
            self.detector = Some(Detector::open(&self.onnx_path(DET_MODEL))?);
        }
        Ok(self.detector.as_mut().expect("just loaded"))
    }

    /// Wörterbuch eines Erkennungsmodells (nur Datei, keine Session).
    pub fn dict(&mut self, model: &str) -> Result<&[String], String> {
        if !self.dicts.contains_key(model) {
            let path = self.yml_path(model);
            let yaml =
                std::fs::read_to_string(&path).map_err(|e| format!("{}: {e}", path.display()))?;
            let d = load_dict(&yaml);
            if d.is_empty() {
                return Err(format!("{}: empty character_dict", path.display()));
            }
            self.dicts.insert(model.to_string(), d);
        }
        Ok(&self.dicts[model])
    }

    /// Erkenner für ein Modell (wird beim ersten Aufruf geladen).
    pub fn recognizer(&mut self, model: &str) -> Result<&mut Recognizer, String> {
        if !self.rec.contains_key(model) {
            let dict = self.dict(model)?.to_vec();
            let rec = Recognizer::open(&self.onnx_path(model), dict)?;
            self.rec.insert(model.to_string(), rec);
        }
        Ok(self.rec.get_mut(model).expect("just loaded"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::{LANGS, by_code};

    #[test]
    fn auto_uses_language_model_universal_always_v6() {
        let de = &LANGS[by_code("de").unwrap()];
        let ru = &LANGS[by_code("ru").unwrap()];
        assert_eq!(rec_model_for(de, ModelChoice::Auto), UNIVERSAL);
        assert_eq!(
            rec_model_for(ru, ModelChoice::Auto),
            "eslav_PP-OCRv5_mobile_rec_onnx"
        );
        for l in LANGS {
            assert_eq!(rec_model_for(l, ModelChoice::Universal), UNIVERSAL);
            assert_eq!(rec_model_for(l, ModelChoice::default()), l.model);
        }
    }

    #[test]
    fn missing_dir_gives_clear_error() {
        let mut m = Models::open(Path::new("/nonexistent-models-xyz"));
        assert!(m.detector().is_err());
        assert!(m.recognizer("foo").is_err());
    }

    #[test]
    fn real_dicts_parse_with_apostrophe() {
        let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("models");
        if !dir.join(DET_MODEL).exists() {
            panic!("models missing: run ./scripts/fetch_models.sh first");
        }
        let mut m = Models::open(&dir);
        let d = m.dict(UNIVERSAL).expect("universal dict");
        assert!(d.len() > 1000, "unexpected dict size {}", d.len());
        assert!(d.iter().any(|s| s == "'"), "'''' escape not resolved");
        let e = m
            .dict("eslav_PP-OCRv5_mobile_rec_onnx")
            .expect("eslav dict");
        assert!(e.iter().any(|s| s == "ж"));
    }
}
