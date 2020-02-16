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
	   (do0
	    (use (std fs File)
		 (std io prelude *)
		 (std thread spawn)
		 (std sync mpsc channel))
	    )

	   (let (((paren sender receiver) (channel))
		 (handle (spawn
			  (space
			   move
			   (lambda ()
			     (for (filename documents)
				  (let* ((f (? ("File::open" filename))))))
			     )))))))))

    
    
    (write-source *code-file*
		  code)))

