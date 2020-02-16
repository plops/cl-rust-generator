(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)

(declaim (optimize (speed 0)
		   (safety 3)
		   (debug 3)))

;; cargo test
;; cargo run
;; cargo clean
;; cargo fix

;; https://github.com/ProgrammingRust/fingertips
;; example from the channels chapter

(progn
  (defparameter *source-dir* #P"examples/06_parallel_text/code/src/")

  (with-open-file (s (asdf:system-relative-pathname 'cl-rust-generator
				   (merge-pathnames #P"../Cargo.toml"
						    *source-dir*))
		     
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (format s "~a"
	    "[package]
name = \"code\"
version = \"0.1.0\"
authors = [\"Martin Kielhorn <kielhorn.martin@gmail.com>\"]
edition = \"2018\"

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

[dependencies]
argparse = \"*\"
byteorder = \"*\"
"


	    ))
  
  (defparameter *code-file*
    (asdf:system-relative-pathname 'cl-rust-generator
				   (merge-pathnames #P"main.rs"
						    *source-dir*)))

  (let ((code
	 `(do0
	   (use (std fs File)
		(std io prelude *)
		(std thread spawn)
		(std sync mpsc channel))
	   (defun start_file_reader_thread ("documents: Vec<PathBuf>")
	     (declare (values "Receiver<String>"
			      "JoinHandle<io::Result<()>>"))
	     (let (((paren sender receiver) (channel))
		   (handle (spawn
			    ;; transfer ownership of sender
			    (space
			     move
			     (lambda ()
			       (for (filename documents)
				    (let* ((f (? ("File::open" filename)))
					   (text ("String::new")))

				      ;; use ? so that errors don't pass silently
				      (? (f.read_to_string "&mut text"))
				      ;; after successful read send to channel (only 3 words go through channel)
				      (when (dot sender
						 (send text)
						 (is_err))
					;; send only fails if the receiver has been dropped
					;; in that case the receiver just exits early with Ok
					break)))
			       (return (Ok "()"))
			       ))))
		   
		   )
	       (do0
		;; result of closure is in threads JoinHandle
		(return (paren receiver handle)))))


	   (defun start_file_indexing_thread ("texts: Receiver<String>")
	     (declare (values "Receiver<InMemoryIndex>"
			      "JoinHandle<()>")))

	   (do0
	    ;; receiver will block
	    ;; loop exits when channel is empty and sender has been dropped
	    (for (text receiver)
		
		 )))))

    
    
    (write-source *code-file*
		  code)))

