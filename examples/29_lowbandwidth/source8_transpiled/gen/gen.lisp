(load (merge-pathnames "00_util.lisp" *load-pathname*))
(load (merge-pathnames "texts.lisp" *load-pathname*))

(in-package :cl-rust-generator)

;;;; gen.lisp --- entry point: generate the whole source8_transpiled workspace.
;;;; Run from the repo root:
;;;;   sbcl --eval '(ql:register-local-projects)' --load examples/29_lowbandwidth/source8_transpiled/gen/gen.lisp --quit

(defun generate-all ()
  (let ((*omit-redundant-parens* t)
        (*rustfmt-arguments* '("--edition" "2024")))
    (write-text-file "Cargo.toml" (workspace-cargo-toml))))

(generate-all)
