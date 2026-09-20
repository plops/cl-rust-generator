//! `03_infer` — YOLOv8-Inferenz via ort (S2).
//!
//! Preprocess (Letterbox 640×640), `output0`-Decode, Confidence-Filter,
//! NMS und Ruecktransformation auf Region-Koordinaten. Decode/NMS sind
//! ohne Modell an synthetischen Tensoren testbar.
