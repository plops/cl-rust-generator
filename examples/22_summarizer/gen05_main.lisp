(load (merge-pathnames "gen00_utils.lisp" *load-pathname*))

(in-package :cl-rust-generator)

;;;; gen05_main.lisp --- main.rs: AppState, wiring, serve.  All
;;;; sequential bindings merge into one let with the serve call last.

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
         (let
          ((api_key (dot (std--env--var (string "GEMINI_API_KEY")) (unwrap_or_else (lambda (_) (String--new))))) (db (dot (await (db--init_db (string "sqlite:data/summarizer-mvp.db"))) (expect (string "db init failed")))) (state (space AppState (curly "db: db.clone()" "api_key"))) (app (routes--build_router state)) (listener (dot (await (tokio--net--TcpListener--bind (string "127.0.0.1:5001"))) (expect (string "bind failed")))))
          (stmt (dot (await (axum--serve listener (dot app (into_make_service)))) (unwrap)))))
)))
