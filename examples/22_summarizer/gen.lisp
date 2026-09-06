(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(load "examples/22_summarizer/toolkit.lisp")

(in-package :cl-rust-generator)

;;;; gen.lisp --- generate the summarizer-mvp crate (Phase 3).
;;;; Run from the repo root:
;;;;   sbcl --eval '(ql:register-local-projects)' --load examples/22_summarizer/gen.lisp --quit
;;;; Public items use (space "pub" ...) and ("pub name" type) slots: the
;;;; generator has no pub qualifier, and strings pass through verbatim.

(defparameter *source-dir* #P"examples/22_summarizer/summarizer-mvp/")

(defun mvp-path (name)
  (asdf:system-relative-pathname 'cl-rust-generator
                                 (merge-pathnames name *source-dir*)))

(defun pub_ (form)
  "Wrap a defun/defstruct0/defenum item as a pub item."
  `(space "pub" ,form))

(defparameter *cargo-toml*
  "[package]
name = \"summarizer-mvp\"
version = \"0.1.0\"
edition = \"2021\"

[dependencies]
tokio = { version = \"1\", features = [\"full\"] }
axum = \"0.8\"
sqlx = { version = \"0.8\", features = [\"sqlite\", \"runtime-tokio\", \"macros\"] }
serde = { version = \"1\", features = [\"derive\"] }
serde_json = \"1\"
reqwest = { version = \"0.12\", features = [\"json\"] }
chrono = \"0.4\"
tracing = \"0.1\"
tracing-subscriber = \"0.3\"
")

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
  (loop for src in (directory (asdf:system-relative-pathname
                               'cl-rust-generator
                               #P"../rs-summarizer/migrations/*.sql"))
        for dst = (mvp-path (format nil "migrations/~a" (file-namestring src)))
        do (ensure-directories-exist dst)
           (uiop:copy-file src dst)))

(defun models-rs ()
  `(do0
     (use (serde (curly Deserialize Serialize)))
     (attr "derive(Debug, Clone, Copy, PartialEq, Eq)"
       ,(pub_ '(defenum GenerationStatus Queued Running Succeeded Failed)))
     (space "impl GenerationStatus"
       (progn
         ,(pub_ '(defun as_str ("&self")
                   (declare (values &str))
                   (case self
                     (Queued (string "queued"))
                     (Running (string "running"))
                     (Succeeded (string "succeeded"))
                     (Failed (string "failed")))))))
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

(defun db-rs ()
  `(do0
     (use (sqlx (curly SqlitePool))
          (sqlx sqlite (curly SqliteConnectOptions SqliteJournalMode SqlitePoolOptions))
          (std str (curly FromStr))
          (crate models (curly SummaryRow)))
     ,(pub_ `(defun-async init_db (database_url)
               (declare (type &str database_url)
                        (values "Result<SqlitePool, sqlx::Error>"))
               (let ((options (dot (? (SqliteConnectOptions--from_str database_url))
                                     (create_if_missing true)
                                     (journal_mode SqliteJournalMode--Wal)))
                      (pool (? (await (dot "SqlitePoolOptions::new()"
                                            (max_connections 5)
                                            (connect_with options))))))
                 (? (await (dot (sqlx--migrate! (string "./migrations"))
                                (run (ref pool)))))
                 (return (Ok pool)))))
     ,(pub_ `(defun-async insert_submit (db model transcript)
               (declare (type SqlitePool &db)
                        (type &str model transcript)
                        (values "Result<i64, sqlx::Error>"))
               (let ((result (? ,(db-exec "INSERT INTO summaries (model, transcript, original_source_link, generation_status) VALUES (?, ?, '', 'queued')"
                                           'model 'transcript))))
                 (return (Ok (dot result (last_insert_rowid)))))))
     ,(pub_ `(defun-async fetch_row (db identifier)
               (declare (type SqlitePool &db)
                        (type i64 identifier)
                        (values "Result<Option<SummaryRow>, sqlx::Error>"))
               (return (Ok (? ,(db-fetch-option
                                "SummaryRow"
                                "SELECT identifier, model, transcript, summary, summary_done, generation_status, generation_error_message FROM summaries WHERE identifier = ?"
                                'identifier))))))
     ,(pub_ `(defun-async claim_running (db identifier now)
               (declare (type SqlitePool &db)
                        (type i64 identifier)
                        (type &str now)
                        (values "Result<bool, sqlx::Error>"))
               (let ((result (? ,(db-exec "UPDATE summaries SET generation_status = 'running', generation_started_at = ?, generation_updated_at = ? WHERE identifier = ? AND generation_status = 'queued'"
                                           'now 'now 'identifier))))
                 (return (Ok (!= 0 (dot result (rows_affected))))))))
     ,(pub_ `(defun-async finish_success (db identifier summary in_tokens out_tokens now)
               (declare (type SqlitePool &db)
                        (type i64 identifier in_tokens out_tokens)
                        (type String summary)
                        (type &str now)
                        (values "Result<(), sqlx::Error>"))
               (? ,(db-exec "UPDATE summaries SET summary = ?, summary_done = 1, generation_status = 'succeeded', summary_input_tokens = ?, summary_output_tokens = ?, summary_timestamp_end = ? WHERE identifier = ?"
                            'summary 'in_tokens 'out_tokens 'now 'identifier))
               (return (Ok (tuple)))))
     ,(pub_ `(defun-async finish_failed (db identifier message now)
               (declare (type SqlitePool &db)
                        (type i64 identifier)
                        (type String message)
                        (type &str now)
                        (values "Result<(), sqlx::Error>"))
               (? ,(db-exec "UPDATE summaries SET generation_status = 'failed', generation_error_code = 'mvp_error', generation_error_message = ?, generation_updated_at = ? WHERE identifier = ?"
                            'message 'now 'identifier))
               (return (Ok (tuple)))))
     ,(pub_ `(defun-async store_embedding (db identifier bytes)
               (declare (type SqlitePool &db)
                        (type i64 identifier)
                        (type "Vec<u8>" bytes)
                        (values "Result<(), sqlx::Error>"))
               (? ,(db-exec "UPDATE summaries SET embedding = ?, embedding_model = 'text-embedding-004' WHERE identifier = ?"
                            'bytes 'identifier))
               (return (Ok (tuple)))))
     ,(pub_ `(defun-async fetch_all_embeddings (db)
               (declare (type SqlitePool &db)
                        (values "Result<Vec<(i64, Vec<u8>)>, sqlx::Error>"))
               (let ((rows (? (await (dot ("sqlx::query_as::<_, (i64, Vec<u8>)>"
                                            (string "SELECT identifier, embedding FROM summaries WHERE embedding IS NOT NULL"))
                                           (fetch_all db))))))
                 (return (Ok rows)))))))

(defun fallible (expr)
  "Wrap a future-returning EXPR as future.await.map_err(|e| e.to_string())?."
  `(? (dot (await ,expr)
            (map_err (lambda (e)
                       (dot e (to_string)))))))

(defun ai-rs ()
  `(do0
     (use (serde (curly Deserialize Serialize)))
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
     (attr "derive(Debug, Clone, serde::Deserialize)"
       ,(pub_ `(defstruct0 RespUsage
                 ("pub promptTokenCount" "Option<u64>")
                 ("pub candidatesTokenCount" "Option<u64>"))))
     (attr "derive(Debug, Clone, serde::Deserialize)"
       ,(pub_ `(defstruct0 RespGen
                 ("pub candidates" "Option<Vec<RespCandidate>>")
                 ("pub usageMetadata" "Option<RespUsage>"))))
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
                     (setf acc (+ acc (* (aref a i) (aref b i)))
                           na (+ na (* (aref a i) (aref a i)))
                           nb (+ nb (* (aref b i) (aref b i)))))
                   (if (or (== na 0.0)
                           (== nb 0.0))
                       (return 0.0))
                   (return (/ acc (* (dot na (sqrt))
                                     (dot nb (sqrt)))))))))
     ,(pub_ `(defun embedding_to_bytes (embedding)
               (declare (type "&[f32]" embedding)
                        (values "Vec<u8>"))
               (return (dot embedding
                            (iter)
                            (flat_map (lambda (x)
                                        (dot x (to_le_bytes))))
                            (collect)))))
     ,(pub_ `(defun bytes_to_embedding (bytes)
               (declare (type "&[u8]" bytes)
                        (values "Vec<f32>"))
               (return (dot bytes
                            (chunks_exact 4)
                            (map (lambda (c)
                                   (f32--from_le_bytes (dot c (try_into) (unwrap)))))
                            (collect)))))
     ,(pub_ `(defun build_prompt (transcript)
               (declare (type &str transcript)
                        (values String))
               (return (format! (string "Summarize the following transcript. Reply with facts, decisions and numbers first, then a short abstract: {}")
                                transcript))))
     ,(pub_ `(defun-async generate_summary (client api_key model transcript)
               (declare (type "reqwest::Client" &client)
                        (type &str api_key model transcript)
                        (values "Result<GenOutput, String>"))
               (let ((url (format! (string "https://generativelanguage.googleapis.com/v1beta/models/{}:generateContent")
                                   model)))
                 (let ((request (make-instance ReqGen
                                               :contents (vec! (make-instance ReqContent
                                                                              :parts (vec! (make-instance ReqPart
                                                                                                          :text (dot transcript (to_string))))))))))
                   (let ((response ,(fallible `(dot client
                                                     (post (ref url))
                                                     (query (ref (bracket (tuple (string "key") api_key))))
                                                     (json (ref request))
                                                     (send))))))
                     (unless (dot response (status) (is_success))
                       (return (Err (format! (string "gemini http error: {}")
                                             (dot response (status))))))
                     (let ((body ,(fallible `(dot response (json)))))
                       (declare (type RespGen body))
                       (let* ((summary (string "")))
                         (declare (type String summary)
                                  (mutable summary))
                         (if-let ((Some candidates) (ref (dot body candidates)))
                           (if-let ((Some first) (dot candidates (first)))
                             (if-let ((Some content) (ref (dot first content)))
                               (if-let ((Some parts) (ref (dot content parts)))
                                 (for (p parts)
                                   (if-let ((Some text) (ref (dot p text)))
                                     (do0 (stmt (dot summary (push_str text))))))))))))
                         (let ((in_tokens 0))
                           (declare (type i64 in_tokens))
                           (let ((out_tokens 0))
                             (declare (type i64 out_tokens))
                             (if-let ((Some usage) (ref (dot body usageMetadata)))
                               (do0
                                 (if-let ((Some v) (dot usage promptTokenCount (copied)))
                                   (do0 (setf in_tokens (coerce v i64))))
                                 (if-let ((Some v) (dot usage candidatesTokenCount (copied)))
                                   (do0 (setf out_tokens (coerce v i64))))))
                             (return (Ok (make-instance GenOutput
                                                        :summary summary
                                                        :in_tokens in_tokens
                                                        :out_tokens out_tokens))))))))
     ,(pub_ `(defun-async embed_text (client api_key text)
               (declare (type "reqwest::Client" &client)
                        (type &str api_key text)
                        (values "Result<Vec<f32>, String>"))
               (let ((url (string "https://generativelanguage.googleapis.com/v1beta/models/text-embedding-004:embedContent")))
                 (let ((request (make-instance ReqEmbed
                                               :content (make-instance ReqContent
                                                                       :parts (vec! (make-instance ReqPart
                                                                                                   :text (dot text (to_string)))))))))
                   (let ((response ,(fallible `(dot client
                                                     (post (ref url))
                                                     (query (ref (bracket (tuple (string "key") api_key))))
                                                     (json (ref request))
                                                     (send))))))
                     (unless (dot response (status) (is_success))
                       (return (Err (format! (string "gemini embed http error: {}")
                                             (dot response (status))))))
                     (let ((body ,(fallible `(dot response (json)))))
                       (declare (type RespEmbed body))
                       (if-let ((Some embedding) (ref (dot body embedding)))
                         (if-let ((Some values) (ref (dot embedding values)))
                           (return (Ok (dot values (clone)))))
                         (return (Err (string "no embedding in response"))))))))))

(defparameter *index-html*
  "<!doctype html><html><body><h1>summarizer-mvp</h1><form method='post' action='/submit'><input name='model' value='gemini-3.5-flash-lite' size='40'/><br/><textarea name='transcript' rows='20' cols='80'></textarea><br/><button type='submit'>summarize</button></form><h2>search</h2><form method='post' action='/search'><input name='query' size='60'/><button type='submit'>search</button></form></body></html>")

(defun routes-rs ()
  `(do0
     (use (axum (curly Router))
          (axum response (curly Html IntoResponse))
          (axum extract (curly State Form Path))
          (axum routing (curly get post))
          (sqlx (curly SqlitePool))
          (crate AppState)
          (crate models (curly SubmitForm SearchForm))
          (crate (curly db ai)))
     ,(index-html-item)
     ,(handler 'index '()
        '(declare (values "impl IntoResponse"))
        '(return (Html INDEX_HTML)))
     ,(handler 'submit '("Form(input): Form<SubmitForm>")
        '(declare (values "impl IntoResponse"))
        '(let ((transcript (dot input (transcript) (trim))))
           (if (dot transcript (is_empty))
               (return (Html (string "<p>empty transcript</p>"))))
           (let ((identifier (case (await (db--insert_submit (ref (dot app db))
                                                             (ref (dot input model))
                                                             transcript))
                               ((Ok id) id)
                               ((Err e) (return (Html (format! (string "<p>db error: {}</p>")
                                                               e))))))))
             (let ((app_clone (dot app (clone)))))
             (let ((db_clone (dot app db (clone)))))
             (let ((key_clone (dot app api_key (clone)))))
             (let ((model_clone (dot input model (clone)))))
             (let ((text_owned (dot transcript (to_string)))))
             (tokio--spawn (space "async move"
                                  (progn (await (run_generation db_clone
                                                                identifier
                                                                key_clone
                                                                model_clone
                                                                text_owned)))))
             (return (Html (format! (string "<p>queued #{}</p><p><a href='/status/{}'>poll status</a></p>")
                                            identifier
                                            identifier)))))
     ,(handler 'status '("Path(identifier): Path<i64>")
        '(declare (values "impl IntoResponse"))
        '(case (await (db--fetch_row (ref (dot app db))
                                     identifier))
           ((Ok (Some row)) (Html (format! (string "<h1>summary #{}</h1><p>status: {}</p><pre>{}</pre>")
                                                   (dot row identifier)
                                                   (dot row generation_status)
                                                   (dot row summary))))
           ((Ok None) (Html (string "<p>not found</p>")))
           ((Err e) (Html (format! (string "<p>db error: {}</p>")
                                   e)))))
     ,(handler 'search '("Form(input): Form<SearchForm>")
        '(declare (values "impl IntoResponse"))
        '(let ((client (reqwest--Client--new))))
        '(let ((query_vec (case (await (ai--embed_text (ref client)
                                                      (ref (dot app api_key))
                                                      (ref (dot input query))))
                            ((Ok v) v)
                            ((Err e) (return (Html (format! (string "<p>embed error: {}</p>")
                                                            e))))))))
        '(let ((rows (case (await (db--fetch_all_embeddings (ref (dot app db))))
                       ((Ok r) r)
                       ((Err e) (return (Html (format! (string "<p>db error: {}</p>")
                                                       e))))))))
        '(let* ((scored (vec!))))
        '(declare (type "Vec<(i64, f32)>" scored))
        '(for ((tuple id bytes) rows)
           (let ((vec (ai--bytes_to_embedding (ref bytes)))))
           (stmt (dot scored (push (tuple id (ai--cosine_similarity (ref query_vec)
                                                                   (ref vec)))))))
        '(stmt (dot scored (sort_by (lambda (a b)
                                      (dot (dot b 1 (partial_cmp (ref (dot a 1))))
                                           (unwrap_or std--cmp--Ordering--Equal))))))
        '(stmt (dot scored (truncate 5)))
        '(let ((items (dot scored
                           (iter)
                           (map (lambda ((tuple id score))
                                  (format! (string "<li>summary #{}: {:.3}</li>")
                                           id
                                           score)))
                           (collect)))))
        '(declare (type "Vec<String>" items))
        '(return (Html (format! (string "<h1>similar summaries</h1><ul>{}</ul>")
                                        (dot items (join (string " ")))))))
     '(defun-async run_generation (db identifier api_key model transcript)
        (declare (type SqlitePool db)
                 (type i64 identifier)
                 (type String api_key model transcript))
        (let ((now (dot chrono--Utc (now) (to_rfc3339)))))
        (if-let ((Ok claimed) (await (db--claim_running (ref db)
                                                       identifier
                                                       (ref now))))
          (if claimed
              (do0
                (let ((client (reqwest--Client--new))))
                (case (await (ai--generate_summary (ref client)
                                                   (ref api_key)
                                                   (ref model)
                                                   (ref transcript)))
                  ((Err message)
                   (do0 (case (await (db--finish_failed (ref db)
                                                       identifier
                                                       message
                                                       (ref now)))
                          ((Ok _) (tuple))
                          ((Err _) (tuple)))))
                  ((Ok gen)
                   (do0
                     (let ((end (dot chrono--Utc (now) (to_rfc3339)))))
                     (case (await (db--finish_success (ref db)
                                                      identifier
                                                      (dot gen summary (clone))
                                                      (dot gen in_tokens)
                                                      (dot gen out_tokens)
                                                      (ref end))))
                       ((Ok _) (tuple))
                       ((Err _) (tuple)))
                     (case (await (ai--embed_text (ref client)
                                                  (ref api_key)
                                                  (ref (dot gen summary))))
                       ((Ok vec)
                        (do0 (case (await (db--store_embedding (ref db)
                                                              identifier
                                                              (ai--embedding_to_bytes (ref vec)))))
                               ((Ok _) (tuple))
                               ((Err _) (tuple)))))
                       ((Err _) (tuple)))))))
              (do0))
          (do0)))
     ,(routes '(("/" get index)
                ("/submit" post submit)
                ("/status/{identifier}" get status)
                ("/search" post search))))

(defun main-rs ()
  `(do0
     (mod ai db models routes)
     (use (sqlx (curly SqlitePool)))
     (attr "derive(Debug, Clone)"
       ,(pub_ `(defstruct0 AppState
                 ("pub db" SqlitePool)
                 ("pub api_key" String))))
     (attr "tokio::main"
       (defun-async main ()
         (tracing_subscriber--fmt--init)
         (let ((api_key (dot (std--env--var (string "GEMINI_API_KEY"))
                             (unwrap_or_else (lambda (_)
                                               (String--new)))))))
         (let ((db (dot (await (db--init_db (string "sqlite:data/summarizer-mvp.db")))
                        (expect (string "db init failed"))))))
         (let ((state (make-instance AppState
                                     :db (dot db (clone))
                                     :api_key api_key))))
         (let ((app (routes--build_router state))))
         (let ((listener (dot (await (tokio--net--TcpListener--bind (string "127.0.0.1:5001")))
                              (expect (string "bind failed"))))))
         (stmt (dot (await (axum--serve listener
                                        (dot app (into_make_service))))
                    (unwrap)))))))

(defun generate-mvp ()
  (let ((*omit-redundant-parens* t))
    (write-text-file "Cargo.toml" *cargo-toml*)
    (copy-migrations)
    (write-source (mvp-path "src/models.rs") (models-rs))
    (write-source (mvp-path "src/db.rs") (db-rs))
    (write-source (mvp-path "src/ai.rs") (ai-rs))
    (write-source (mvp-path "src/routes.rs") (routes-rs))
    (write-source (mvp-path "src/main.rs") (main-rs))))

(defun index-html-item ()
  `(space "const INDEX_HTML: &str =" ,(format nil "~s;" *index-html*)))

(generate-mvp)
