(load (merge-pathnames "gen00_utils.lisp" *load-pathname*))
(load (merge-pathnames "gen01_models.lisp" *load-pathname*))
(load (merge-pathnames "gen02_db.lisp" *load-pathname*))
(load (merge-pathnames "gen03_ai.lisp" *load-pathname*))
(load (merge-pathnames "gen04_routes.lisp" *load-pathname*))
(load (merge-pathnames "gen05_main.lisp" *load-pathname*))

(in-package :cl-rust-generator)

;;;; gen06_generate.lisp --- entry point: write the MVP crate.
;;;; Run from the repo root:
;;;;   sbcl --eval '(ql:register-local-projects)' --load examples/22_summarizer/gen06_generate.lisp --quit

(defun generate-mvp ()
  (let ((*omit-redundant-parens* t)
        (*rustfmt-arguments* '("--edition" "2024")))
    (write-text-file "Cargo.toml" *cargo-toml*)
    (copy-migrations)
    (write-source (mvp-path "src/models.rs") (models-rs))
    (write-source (mvp-path "src/db.rs") (db-rs))
    (write-source (mvp-path "src/ai.rs") (ai-rs))
    (write-source (mvp-path "src/routes.rs") (routes-rs))
    (write-source (mvp-path "src/main.rs") (main-rs))))

(generate-mvp)
