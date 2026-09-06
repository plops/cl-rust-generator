(load (merge-pathnames "gen00_utils.lisp" *load-pathname*))

(in-package :cl-rust-generator)

;;;; gen03_ai.lisp --- ai.rs: Gemini REST structs, cosine similarity,
;;;; byte codecs, summary + embedding calls.  Sequential bindings merge
;;;; into one let; only mutated bindings use let*.

(defun ai-rs ()
  `(do0
     (attr "derive(Debug, Clone, serde::Serialize)"
       ,(pub_ `(defstruct0 ReqPart ("pub text" String))))
     (attr "derive(Debug, Clone, serde::Serialize)"
       ,(pub_ `(defstruct0 ReqContent ("pub parts" "Vec<ReqPart>"))))
     (attr "derive(Debug, Clone, serde::Serialize)"
       ,(pub_ `(defstruct0 ReqGen ("pub contents" "Vec<ReqContent>"))))
     (attr "derive(Debug, Clone, serde::Serialize)"
       ,(pub_ `(defstruct0 ReqEmbed ("pub content" ReqContent))))
     (attr "derive(Debug, Clone, serde::Serialize)"
       ,(pub_ `(defstruct0 GenOutput
                 ("pub summary" String)
                 ("pub in_tokens" i64)
                 ("pub out_tokens" i64))))
     (attr "derive(Debug, Clone, serde::Deserialize)"
       ,(pub_ `(defstruct0 RespPart ("pub text" "Option<String>"))))
     (attr "derive(Debug, Clone, serde::Deserialize)"
       ,(pub_ `(defstruct0 RespContent ("pub parts" "Option<Vec<RespPart>>"))))
     (attr "derive(Debug, Clone, serde::Deserialize)"
       ,(pub_ `(defstruct0 RespCandidate ("pub content" "Option<RespContent>"))))
     (attr "derive(Debug, Clone, serde::Deserialize)" "serde(rename_all = \"camelCase\")"
       ,(pub_ `(defstruct0 RespUsage
                 ("pub prompt_token_count" "Option<u64>")
                 ("pub candidates_token_count" "Option<u64>"))))
     (attr "derive(Debug, Clone, serde::Deserialize)" "serde(rename_all = \"camelCase\")"
       ,(pub_ `(defstruct0 RespGen
                 ("pub candidates" "Option<Vec<RespCandidate>>")
                 ("pub usage_metadata" "Option<RespUsage>"))))
     (attr "derive(Debug, Clone, serde::Deserialize)"
       ,(pub_ `(defstruct0 RespEmbedValues ("pub values" "Option<Vec<f32>>"))))
     (attr "derive(Debug, Clone, serde::Deserialize)"
       ,(pub_ `(defstruct0 RespEmbed ("pub embedding" "Option<RespEmbedValues>"))))
     ,(pub_ `(defun cosine_similarity (a b)
               (declare (type "&[f32]" a b)
                        (values f32))
               (if (or (dot a (is_empty))
                       (dot b (is_empty)))
                   (return 0.0))
               (let ((len (dot a (len) (min (dot b (len))))))
                 (let* ((acc 0.0)
                        (na 0.0)
                        (nb 0.0))
                   (declare (type f32 acc na nb))
                   (for (i (range 0 len))
                     ;; (incf place delta) emits place += delta.
                     (incf acc (* (aref a i) (aref b i)))
                     (incf na (* (aref a i) (aref a i)))
                     (incf nb (* (aref b i) (aref b i))))
                   (if (or (== na 0.0)
                           (== nb 0.0))
                       (return 0.0))
                   (/ acc (* (dot na (sqrt))
                             (dot nb (sqrt))))))))
     ,(pub_ `(defun embedding_to_bytes (embedding)
               (declare (type "&[f32]" embedding)
                        (values "Vec<u8>"))
               ;; Tail expression, no (return ...): progn drops the final
               ;; semicolon so clippy sees no needless_return.
               (dot embedding
                    (iter)
                    (flat_map (lambda (x)
                                (dot x (to_le_bytes))))
                    (collect))))
     ,(pub_ `(defun bytes_to_embedding (bytes)
               (declare (type "&[u8]" bytes)
                        (values "Vec<f32>"))
               ;; Tail expression.  The DSL has no turbofish method-call
               ;; form, so the as_chunks::<4> head is a verbatim string
               ;; receiver; as_chunks yields &[u8; 4], hence (deref c).
               (dot "bytes.as_chunks::<4>().0"
                    (iter)
                    (map (lambda (c)
                           (f32--from_le_bytes (deref c))))
                    (collect))))
     ,(pub_ `(defun build_prompt (transcript)
               (declare (type &str transcript)
                        (values String))
               (format! (string "Summarize the following transcript. Reply with facts, decisions and numbers first, then a short abstract: {}")
                        transcript)))
     ,(pub_ `(defun-async generate_summary (client api_key model transcript)
      (declare
       (type "reqwest::Client" &client)
       (type &str api_key model transcript)
       (values "Result<GenOutput, String>"))
      (let
       ((url (format! (string "https://generativelanguage.googleapis.com/v1beta/models/{}:generateContent") model)) (request (make-instance ReqGen :contents (vec! (make-instance ReqContent :parts (vec! (make-instance ReqPart :text (build_prompt transcript))))))) (response ,(fallible `(dot client (post url) (query (ref (bracket (tuple (string "key") api_key)))) (json (ref request)) (send)))))
       (unless
        (dot response (status) (is_success))
        (return (Err (format! (string "gemini http error: {}") (dot response (status))))))
       (let
        ((body ,(fallible `(dot response (json)))))
        (declare (type RespGen body))
        (let*
         ((summary (dot (string "") (to_string))) (in_tokens 0) (out_tokens 0))
         (declare (type String summary) (type i64 in_tokens out_tokens))
         ;; Intermediate lets break direct if-let nesting so
         ;; clippy::collapsible_if stays quiet.
         (if-let
          ((Some candidates) (ref (dot body candidates)))
          (let
           ((first (dot candidates (first))))
           (if-let
            ((Some first) first)
            (let
             ((content (ref (dot first content))))
             (if-let
              ((Some content) content)
              (let
               ((parts (ref (dot content parts))))
               (if-let
                ((Some parts) parts)
                (for
                 (p parts)
                 (if-let
                  ((Some text) (ref (dot p text)))
                  (do0
                   (stmt (dot summary (push_str text)))))))))))))
         (if-let
          ((Some usage) (ref (dot body usage_metadata)))
          (do0
           (if-let
            ((Some v) (ref (dot usage prompt_token_count)))
            (do0
             (setf in_tokens (coerce (deref v) i64))))
           (if-let
            ((Some v) (ref (dot usage candidates_token_count)))
            (do0
             (setf out_tokens (coerce (deref v) i64))))))
         ;; Tail.  make-instance has no shorthand form, so the struct
         ;; literal is a (space Name (curly ...)) escape: GenOutput
         ;; {summary, in_tokens, out_tokens}.
         (Ok (space GenOutput (curly "summary" "in_tokens" "out_tokens"))))))))
     ,(pub_ `(defun-async embed_text (client api_key text)
      (declare
       (type "reqwest::Client" &client)
       (type &str api_key text)
       (values "Result<Vec<f32>, String>"))
      (let
       ((url (string "https://generativelanguage.googleapis.com/v1beta/models/gemini-embedding-001:embedContent")) (request (make-instance ReqEmbed :content (make-instance ReqContent :parts (vec! (make-instance ReqPart :text (dot text (to_string))))))) (response ,(fallible `(dot client (post url) (query (ref (bracket (tuple (string "key") api_key)))) (json (ref request)) (send)))))
       (unless
        (dot response (status) (is_success))
        (return (Err (format! (string "gemini embed http error: {}") (dot response (status))))))
       (let
        ((body ,(fallible `(dot response (json)))))
        (declare (type RespEmbed body))
        (if-let
         ((Some embedding) (ref (dot body embedding)))
         (if-let
          ((Some values) (ref (dot embedding values)))
          ;; Arm tails: bare expressions, no (return ...).
          (Ok (dot values (clone)))
          (Err (dot (string "no values in embedding") (to_string))))
         (Err (dot (string "no embedding in response") (to_string))))))))
     (attr "cfg(test)"
        (space "mod tests"
          (block
            (use (super *))
            (attr "test"
              (defun cosine_identical ()
                (let ((result (cosine_similarity (ref (bracket (comma 1.0 2.0 3.0))) (ref (bracket (comma 1.0 2.0 3.0))))))
                  (stmt (assert! (> result 0.99999))))))
            (attr "test"
              (defun cosine_orthogonal ()
                (let ((result (cosine_similarity (ref (bracket (comma 1.0 0.0))) (ref (bracket (comma 0.0 1.0))))))
                  (stmt (assert! (== result 0.0))))))
            (attr "test"
              (defun cosine_zero_vector ()
                (let ((result (cosine_similarity (ref (bracket (comma 0.0 0.0))) (ref (bracket (comma 1.0 2.0))))))
                  (stmt (assert! (== result 0.0))))))
            (attr "test"
              (defun cosine_empty ()
                (let ((result (cosine_similarity (ref (bracket (comma))) (ref (bracket (comma 1.0))))))
                  (stmt (assert! (== result 0.0))))))
            (attr "test"
              (defun cosine_unequal_lengths ()
                (let ((result (cosine_similarity (ref (bracket (comma 1.0 0.0 0.0))) (ref (bracket (comma 1.0 0.0))))))
                  (stmt (assert! (== result 1.0))))))
            (attr "test"
              (defun bytes_roundtrip ()
                (let ((original (vec! 1.5 -2.25 0.0))
                      (back (bytes_to_embedding (ref (embedding_to_bytes (ref (bracket (comma 1.5 -2.25 0.0))))))))
                  (stmt (assert! (== back original)))))))))
))