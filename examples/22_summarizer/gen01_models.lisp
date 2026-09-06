(load (merge-pathnames "gen00_utils.lisp" *load-pathname*))

(in-package :cl-rust-generator)

;;;; gen01_models.lisp --- models.rs: status enum, row and form structs.
;;;; Verified emission: enum + as_str + three derived structs.

(defun models-rs ()
  `(do0
     (attr "derive(Debug, Clone, Copy, PartialEq, Eq)"
       ,(pub_ '(defenum GenerationStatus Queued Running Succeeded Failed)))
     (space "impl GenerationStatus"
       (progn
         ,(pub_ '(defun as_str ("&self")
                   (declare (values String))
                   (case (deref self)
                     ((scope GenerationStatus Queued) (dot (string "queued") (to_string)))
                     ((scope GenerationStatus Running) (dot (string "running") (to_string)))
                     ((scope GenerationStatus Succeeded) (dot (string "succeeded") (to_string)))
                     ((scope GenerationStatus Failed) (dot (string "failed") (to_string))))))))
     (attr "derive(Debug, Clone, serde::Serialize, sqlx::FromRow)"
       ,(pub_ '(defstruct0 SummaryRow
                 ("pub identifier" i64)
                 ("pub model" String)
                 ("pub transcript" String)
                 ("pub summary" String)
                 ("pub summary_done" bool)
                 ("pub generation_status" String)
                 ("pub generation_error_message" String))))
     (attr "derive(Debug, serde::Deserialize)"
       ,(pub_ '(defstruct0 SubmitForm
                 ("pub model" String)
                 ("pub transcript" String))))
     (attr "derive(Debug, serde::Deserialize)"
       ,(pub_ '(defstruct0 SearchForm
                 ("pub query" String))))))
