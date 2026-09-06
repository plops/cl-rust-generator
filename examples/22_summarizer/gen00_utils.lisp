(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(load (merge-pathnames "toolkit.lisp" *load-pathname*))

(in-package :cl-rust-generator)

;;;; gen00_utils.lisp --- shared paths, builders and file writers for the
;;;; 22_summarizer MVP.  Load first; gen01..gen05 define one module each,
;;;; gen06 writes everything out.
;;;;
;;;; A scoping rule for the whole generator (verified against parse-let):
;;;; a (let ...) binding is visible only inside that let's BODY.  Sibling
;;;; forms after a closed let cannot see its variables, so sequential
;;;; bindings must merge into ONE let (immutable) or nest.

(defparameter *source-dir* #P"examples/22_summarizer/summarizer-mvp/")

(defun mvp-path (name)
  (asdf:system-relative-pathname 'cl-rust-generator
                                 (merge-pathnames name *source-dir*)))

(defun pub_ (form)
  "Wrap a defun/defstruct0/defenum item as a pub item.  Attributes go
outside: (attr ... (space \"pub\" ...)) is invalid Rust."
  `(space "pub" ,form))

(defun fallible (expr)
  "Wrap a future-returning EXPR as future.await.map_err(|e| e.to_string())?."
  `(? (dot (await ,expr)
            (map_err (lambda (e)
                       (dot e (to_string)))))))

(defun write-text-file (relpath content)
  (let ((fn (mvp-path relpath)))
    (ensure-directories-exist fn)
    (with-open-file (s fn :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (write-sequence content s))
    fn))

(defun copy-migrations ()
  "Copy the upstream migration set verbatim: identical schema, zero drift."
  (let ((srcs (directory (asdf:system-relative-pathname
                          'cl-rust-generator
                          #P"../rs-summarizer/migrations/*.sql"))))
    (assert srcs nil "no upstream migrations found")
    (loop for src in srcs
          for dst = (mvp-path (format nil "migrations/~a" (file-namestring src)))
          do (ensure-directories-exist dst)
             (uiop:copy-file src dst))))

(defparameter *cargo-toml*
  "[package]
name = \"summarizer-mvp\"
version = \"0.1.0\"
edition = \"2024\"

[dependencies]
tokio = { version = \"1\", features = [\"full\"] }
axum = \"0.8\"
sqlx = { version = \"0.9\", features = [\"sqlite\", \"runtime-tokio\", \"macros\"] }
serde = { version = \"1\", features = [\"derive\"] }
serde_json = \"1\"
reqwest = { version = \"0.12\", features = [\"json\"] }
chrono = \"0.4\"
tracing = \"0.1\"
tracing-subscriber = \"0.3\"
")

(defparameter *index-html*
  "<!doctype html><html><body><h1>summarizer-mvp</h1><form method='post' action='/submit'><input name='model' value='gemini-3.5-flash-lite' size='40'/><br/><textarea name='transcript' rows='20' cols='80'></textarea><br/><button type='submit'>summarize</button></form><h2>search</h2><form method='post' action='/search'><input name='query' size='60'/><button type='submit'>search</button></form></body></html>")

(defun index-html-item ()
  `(space "const INDEX_HTML: &str =" ,(format nil "~s;" *index-html*)))
