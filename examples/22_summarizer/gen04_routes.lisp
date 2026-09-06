(load (merge-pathnames "gen00_utils.lisp" *load-pathname*))

(in-package :cl-rust-generator)

;;;; gen04_routes.lisp --- routes.rs: index/submit/status/search handlers,
;;;; the background run_generation task and the route table.  Handler
;;;; bodies merge sequential bindings into one let (see gen00 header).

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
     (defun-async index ("State(_app): State<AppState>")
       (declare (values "impl IntoResponse"))
       (Html INDEX_HTML))
     ,(handler 'submit '("Form(input): Form<SubmitForm>")
        '(declare (values "impl IntoResponse"))
        '(let
         ((transcript (dot input transcript (trim))))
         (if
          (dot transcript (is_empty))
          (return (Html (dot (string "<p>empty transcript</p>") (to_string)))))
         (let
          ((identifier (case (await (db--insert_submit (ref (dot app db)) (ref (dot input model)) transcript)) ((Ok id) id) ((Err e) (return (Html (format! (string "<p>db error: {}") e)))))) (db_clone (dot app db (clone))) (key_clone (dot app api_key (clone))) (model_clone (dot input model (clone))) (text_owned (dot transcript (to_string))))
          (tokio--spawn
           (space
            "async move"
            (progn
             (await (run_generation db_clone identifier key_clone model_clone text_owned)))))
          (Html (format! (string "<p>queued #{}</p><p><a href='/status/{}'>poll status</a></p>") identifier identifier)))))
     ,(handler 'status '("Path(identifier): Path<i64>")
        '(declare (values "impl IntoResponse"))
        '(case (await (db--fetch_row (ref (dot app db))
                                     identifier))
           ((Ok (Some row)) (Html (format! (string "<h1>summary #{}</h1><p>status: {}</p><pre>{}</pre>")
                                                   (dot row identifier)
                                                   (dot row generation_status)
                                                   (dot row summary))))
           ((Ok None) (Html (dot (string "<p>not found</p>") (to_string))))
           ((Err e) (Html (format! (string "<p>db error: {}</p>")
                                   e)))))
     ,(handler 'search '("Form(input): Form<SearchForm>")
        '(declare (values "impl IntoResponse"))
        '(let
         ((client (reqwest--Client--new)) (query_vec (case (await (ai--embed_text (ref client) (ref (dot app api_key)) (ref (dot input query)))) ((Ok v) v) ((Err e) (return (Html (format! (string "<p>embed error: {}") e)))))) (rows (case (await (db--fetch_all_embeddings (ref (dot app db)))) ((Ok r) r) ((Err e) (return (Html (format! (string "<p>db error: {}") e)))))) (scored (vec!)))
         (declare (type "Vec<(i64, f32)>" scored) (mutable scored))
         (for
          ((tuple id bytes) rows)
          (let
           ((vec (ai--bytes_to_embedding (ref bytes))))
           (stmt (dot scored (push (tuple id (ai--cosine_similarity (ref query_vec) (ref vec))))))))
         (stmt
          (dot scored (sort_by (lambda (a b) (dot (dot b 1 (partial_cmp (ref (dot a 1)))) (unwrap_or std--cmp--Ordering--Equal))))))
         (stmt (dot scored (truncate 5)))
         (let
          ((items (dot scored (iter) (map (lambda ((tuple id score)) (format! (string "<li>summary #{}: {:.3}</li>") id score))) (collect))))
          (declare (type "Vec<String>" items))
          (Html (format! (string "<h1>similar summaries</h1><ul>{}</ul>") (dot items (join (string " "))))))))
     ,@(list '(defun-async run_generation (db identifier api_key model transcript)
      (declare (type SqlitePool db) (type i64 identifier) (type String api_key model transcript))
      (let
       ((now (dot (chrono--Utc--now) (to_rfc3339))))
       (if-let
        ((Ok true) (await (db--claim_running (ref db) identifier (ref now))))
        ;; (Ok true) folds the claimed check into the if-let: no
        ;; nested if for clippy::collapsible_if.
        (do0
         (let
           ((client (reqwest--Client--new)))
           (case
            (await (ai--generate_summary (ref client) (ref api_key) (ref model) (ref transcript)))
            ((Err message)
             (do0
              (stmt (drop (await (db--finish_failed (ref db) identifier message (ref now)))))))
            ((Ok generation)
             (do0
              (let
               ((end (dot (chrono--Utc--now) (to_rfc3339))))
               (stmt (drop (await (db--finish_success (ref db) identifier (dot generation summary (clone)) (dot generation in_tokens) (dot generation out_tokens) (ref end)))))
               (if-let
                ((Ok vec) (await (ai--embed_text (ref client) (ref api_key) (ref (dot generation summary)))))
                (do0
                 (stmt (drop (await (db--store_embedding (ref db) identifier (ai--embedding_to_bytes (ref vec))))))))))))
          (do0)))))))
     ,(routes '(("/" get index)
                ("/submit" post submit)
                ("/status/{identifier}" get status)
                ("/search" post search)))))
