(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator")
  ;(ql:quickload "cl-ppcre")
  )

(in-package :cl-rust-generator)

;; switches
;; :safety .. enable extra asserts in the code
;; :nolog  .. suppress all logging output (also makes code more readable)
;; :log-consume .. show consumption of padding bits
;; :log-brc .. show decompression

(setf *features* (union *features* '(:safety
					;:nolog
					;:log-brc
				     ;:log-consume
				     )))
(setf *features* (set-difference *features* '(;:safety
					      :nolog
					      :log-brc
					      :log-consume
					      )))

(progn
  (defparameter *source-dir* #P"examples/01_gcd/source/")
  (defparameter *code-file* (asdf:system-relative-pathname 'cl-rust-generator (merge-pathnames #P"run_01_base.rs"
											       *source-dir*)))

  (let ((code
	 `(do0

	   "use std::io::Write;"
	   
	   "use std::str::FromStr;"
	   
	   (defun main ()
	     (let ((numbers ("Vec::new")))
	       (for (arg (dot ("std::env::args")
			      (skip 1)))
		    (numbers.push
		     (dot ("u64::from_str" &arg)
			  (expect (string "error parsing argument")))))
	       (when (== 0 (numbers.len))
		 (dot (writeln! ("std::io::stderr")
				(string "Usage: gcd NUMBER ..."))
		      (unwrap))
		 ("std::process::exit" 1))
	       (let ((d (aref numbers 0)))
		 (for (m (ref (aref numbers "1..")))
		      (setf d (gcd d *m))))
	       (println! (string "The greatest common divisor of {:?} is {}"
				 numbers d)))))))
    
    
    (write-source *code-file*
		  code)))

