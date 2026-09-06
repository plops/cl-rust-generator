;;;; check-toolkit.lisp --- Phase 2 validation: expand the toolkit builders,
;;;; assert on the emitted Rust, and print the expansions for human review.
;;;; Run: sbcl --script check-toolkit.lisp   (from examples/22_summarizer/)
;;;; Exits non-zero on the first failed assertion.

(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(load (merge-pathnames "toolkit.lisp" *load-pathname*))

(in-package :cl-rust-generator)

(defun check (label form &rest needles)
  (let ((rust (emit-rs :code form)))
    (format t "--- ~a ---~%~a~%~%" label rust)
    (loop for n in needles do
      (unless (search n rust)
        (format *error-output* "FAIL ~a: missing ~s~%" label n)
        (sb-ext:exit :code 1)))))

(check "handler"
  (handler 'index '()
    '(declare (values "impl IntoResponse"))
    '(return (Html (string "<h1>summarizer</h1>"))))
  "async fn index" "State(app): State<AppState>" "impl IntoResponse" "Html(")

(check "routes"
  (routes '(("/" get index)
            ("/submit" post submit)
            ("/status" post status)
            ("/search" post search)))
  "pub fn build_router" "Router::new()" "get(index)" "post(submit)"
  "post(status)" "post(search)" "with_state(state)")

(check "model"
  (model 'SummaryRow '(identifier i64) '(model String) '(summary String))
  "#[derive(Debug, Clone" "struct SummaryRow" "identifier: i64")

(check "db-exec"
  (db-exec "INSERT INTO summaries (model, transcript) VALUES (?, ?)" 'model 'transcript)
  "sqlx::query(" ".bind(model)" ".bind(transcript)" ".execute(db).await")

(check "db-fetch-option"
  (db-fetch-option "SummaryRow"
    "SELECT identifier, model, summary FROM summaries WHERE identifier = ?"
    'identifier)
  "sqlx::query_as::<_, SummaryRow>" ".bind(identifier)" ".fetch_optional(db).await")

;;;; Phase 3 golden patterns: quote raw syntax (builders stay unquoted).

(check "ok-true-fold"
  '(if-let ((Ok true) (f)) (do0 (g)))
  "if let Ok(true) = f")

(check "drop-discard"
  '(stmt (drop (await (do_work))))
  "drop(do_work().await);")

(check "unnest-let"
  '(if-let ((Some c) x)
     (let ((f (dot c (first))))
       (if-let ((Some f) f) (g f))))
  "let f = c.first();" "if let Some(f) = f")

(check "cfg-test-module"
  '(attr "cfg(test)"
     (space "mod tests"
       (block (use (super *)))))
  "#[cfg(test)]" "mod tests" "use super::*;")

(check "bracket-comma"
  '(ref (bracket (comma 1.0 2.0)))
  "&[1.0, 2.0]")

(format t "toolkit OK~%")
