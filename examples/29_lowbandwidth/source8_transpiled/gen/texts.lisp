(load (merge-pathnames "00_util.lisp" *load-pathname*))

(in-package :cl-rust-generator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (readtable-case *readtable*) :invert))

;;;; texts.lisp --- non-Rust outputs (Cargo manifests, scripts, docs).
;;;; Grows with every task; T0 only carries the workspace manifest.

(defun workspace-cargo-toml ()
  "[workspace]
resolver = \"3\"
members = [\"common\", \"server\", \"client\"]

[workspace.package]
version = \"0.1.0\"
edition = \"2024\"
authors = [\"Wol Pumba <wolpumba@gmail.com>\"]
license = \"MIT\"

[profile.release]
opt-level = 3

# Debug-Builds: Abhängigkeiten optimieren (Encoder/Decoder sonst ~20x langsamer).
[profile.dev.package.\"*\"]
opt-level = 3
")
