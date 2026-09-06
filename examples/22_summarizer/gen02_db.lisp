(load (merge-pathnames "gen00_utils.lisp" *load-pathname*))

(in-package :cl-rust-generator)

;;;; gen02_db.lisp --- db.rs: pool init, inserts, CAS transitions, embeddings.
;;;; Every let carries its users in its BODY (sibling forms cannot see
;;;; bindings); sequential immutable bindings merge into one let.

(defun db-rs ()
  `(do0
     (use (sqlx (curly SqlitePool))
          (sqlx sqlite (curly SqliteConnectOptions SqliteJournalMode SqlitePoolOptions))
          (std str (curly FromStr))
          (crate models (curly SummaryRow GenerationStatus)))
     ,(pub_ `(defun-async init_db (database_url)
               (declare (type &str database_url)
                        (values "Result<SqlitePool, sqlx::Error>"))
               ;; sqlite will not create parent dirs: make them first.
               (if-let ((Some parent) (dot (std--path--Path--new (dot database_url (trim_start_matches (string "sqlite:")))) (parent)))
                 (stmt (? (dot (std--fs--create_dir_all parent) (map_err sqlx--Error--Io)))))
               (let ((options (dot (? (SqliteConnectOptions--from_str database_url))
                                    (create_if_missing true)
                                    (journal_mode SqliteJournalMode--Wal)))
                     (pool (? (await (dot "SqlitePoolOptions::new()"
                                           (max_connections 5)
                                           (connect_with options))))))
                 (? (await (dot (sqlx--migrate! (string "./migrations"))
                                (run (ref pool)))))
                 (Ok pool))))
     ,(pub_ `(defun-async insert_submit (db model transcript)
               (declare (type SqlitePool &db)
                        (type &str model transcript)
                        (values "Result<i64, sqlx::Error>"))
               (let ((status (dot (scope GenerationStatus Queued) (as_str)))
                     (result (? ,(db-exec "INSERT INTO summaries (model, transcript, original_source_link, generation_status) VALUES (?, ?, '', ?)"
                                          'model 'transcript 'status))))
                 (Ok (dot result (last_insert_rowid))))))
     ,(pub_ `(defun-async fetch_row (db identifier)
               (declare (type SqlitePool &db)
                        (type i64 identifier)
                        (values "Result<Option<SummaryRow>, sqlx::Error>"))
               ;; Tail: the query already returns Result<Option<SummaryRow>,
               ;; sqlx::Error>, so no Ok(...?) wrapper (clippy::needless_question_mark).
               ,(db-fetch-option
                 "SummaryRow"
                 "SELECT identifier, model, transcript, summary, summary_done, generation_status, generation_error_message FROM summaries WHERE identifier = ?"
                 'identifier)))
     ,(pub_ `(defun-async claim_running (db identifier now)
               (declare (type SqlitePool &db)
                        (type i64 identifier)
                        (type &str now)
                        (values "Result<bool, sqlx::Error>"))
               (let ((new_status (dot (scope GenerationStatus Running) (as_str)))
                     (old_status (dot (scope GenerationStatus Queued) (as_str)))
                     (result (? ,(db-exec "UPDATE summaries SET generation_status = ?, generation_started_at = ?, generation_updated_at = ? WHERE identifier = ? AND generation_status = ?"
                                          'new_status 'now 'now 'identifier 'old_status))))
                 (Ok (!= 0 (dot result (rows_affected)))))))
     ,(pub_ `(defun-async finish_success (db identifier summary in_tokens out_tokens now)
               (declare (type SqlitePool &db)
                        (type i64 identifier in_tokens out_tokens)
                        (type String summary)
                        (type &str now)
                        (values "Result<(), sqlx::Error>"))
               (let ((status (dot (scope GenerationStatus Succeeded) (as_str)))
                     (_result (? ,(db-exec "UPDATE summaries SET summary = ?, summary_done = 1, generation_status = ?, summary_input_tokens = ?, summary_output_tokens = ?, summary_timestamp_end = ? WHERE identifier = ?"
                                           'summary 'status 'in_tokens 'out_tokens 'now 'identifier)))))
               (Ok (tuple))))
     ,(pub_ `(defun-async finish_failed (db identifier message now)
               (declare (type SqlitePool &db)
                        (type i64 identifier)
                        (type String message)
                        (type &str now)
                        (values "Result<(), sqlx::Error>"))
               (let ((status (dot (scope GenerationStatus Failed) (as_str)))
                     (_result (? ,(db-exec "UPDATE summaries SET generation_status = ?, generation_error_code = 'mvp_error', generation_error_message = ?, generation_updated_at = ? WHERE identifier = ?"
                                           'status 'message 'now 'identifier)))))
               (Ok (tuple))))
     ,(pub_ `(defun-async store_embedding (db identifier bytes)
               (declare (type SqlitePool &db)
                        (type i64 identifier)
                        (type "Vec<u8>" bytes)
                        (values "Result<(), sqlx::Error>"))
               (? ,(db-exec "UPDATE summaries SET embedding = ?, embedding_model = 'text-embedding-004' WHERE identifier = ?"
                            'bytes 'identifier))
               (Ok (tuple))))
     ,(pub_ `(defun-async fetch_all_embeddings (db)
               (declare (type SqlitePool &db)
                        (values "Result<Vec<(i64, Vec<u8>)>, sqlx::Error>"))
               (let ((rows (? (await (dot ("sqlx::query_as::<_, (i64, Vec<u8>)>"
                                           (string "SELECT identifier, embedding FROM summaries WHERE embedding IS NOT NULL"))
                                          (fetch_all db))))))
                 (Ok rows))))))
