(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)

;;;; toolkit.lisp --- small reusable builders for the 22_summarizer MVP.
;;;;
;;;; The generator is a shallow transformer, so these are ordinary Lisp
;;;; functions returning s-expressions (same style as LPRINT in
;;;; examples/21_mandelbrot/gen00.lisp).  Gen files splice them with
;;;; backquote-comma, e.g. `(do0 ,(model 'SummaryRow ...)).

(defun handler (name params &rest body)
  "Build (defun-async NAME (...) BODY ...) for an axum handler.
State(app) is always injected as the first parameter; PARAMS are the
remaining (usually extractor) parameters, written as strings when they
carry types, e.g. \"Form(input): Form<SubmitForm>\".  For handlers that
ignore shared state, bypass this builder with a literal defun-async
using State(_app) so rustc sees no unused binding."
  `(defun-async ,name ("State(app): State<AppState>" ,@params) ,@body))

(defun routes (specs)
  "Build the build_router fn from ((path method handler) ...) SPECS.
METHOD is a bare routing symbol (get/post, imported from axum::routing);
the Router::new() head is a verbatim string because DOT joins bare
symbols without call parentheses."
  `(space "pub" (defun build_router (state)
     (declare (type AppState state)
              (values Router))
     ;; Tail expression, no (return ...): progn drops the final semicolon.
     (dot "Router::new()"
          ,@(loop for (path method handler) in specs
                  collect `(route (string ,path) (,method ,handler)))
          (with_state state)))))

(defun model (name &rest slots)
  "Build a DB/API struct with the project's standard derive set.
SLOTS are (slot-name slot-type) pairs for DEFSTRUCT0."
  `(attr "derive(Debug, Clone, serde::Serialize, serde::Deserialize)"
     (defstruct0 ,name ,@slots)))

(defun db-exec (sql &rest binds)
  "Build sqlx::query(SQL).bind()....execute(db).await from an SQL string
and bind expressions."
  `(await (dot (sqlx--query (string ,sql))
               ,@(loop for b in binds collect `(bind ,b))
               (execute db))))

(defun db-fetch-option (rowtype sql &rest binds)
  "Build sqlx::query_as::<_, ROWTYPE>(SQL).bind()....fetch_optional(db).await.
The turbofish head is a verbatim string: the generator has no generics
form, and a string keeps the call readable."
  `(await (dot (,(format nil "sqlx::query_as::<_, ~a>" rowtype)
                (string ,sql))
               ,@(loop for b in binds collect `(bind ,b))
               (fetch_optional db))))

(defun demo-expansions ()
  "Item-level demo forms for the read+rustfmt validation in Phase 2."
  `(("handler" ,(handler 'index '()
                         '(declare (values "impl IntoResponse"))
                         '(return (Html (string "<h1>summarizer</h1>")))))
    ("routes" ,(routes '(("/" get index)
                         ("/submit" post submit)
                         ("/status" post status)
                         ("/search" post search))))
    ("model" ,(model 'SummaryRow
                     '(identifier i64)
                     '(model String)
                     '(summary String)))))
