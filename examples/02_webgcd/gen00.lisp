(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator")
  ;(ql:quickload "cl-ppcre")
  )

(in-package :cl-rust-generator)

(declaim (optimize (speed 0)
	  (debug 3)
	  (safety 3)))

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


;; cargo new --bin rs02_webgcd
;; cargo test
;; cargo run
;; cargo clean

(progn
  (defparameter *source-dir* #P"examples/02_webgcd/rs02_webgcd/src/")
  (defparameter *code-file* (asdf:system-relative-pathname 'cl-rust-generator (merge-pathnames #P"main.rs"
											       *source-dir*)))

  (let ((code
	 `(do0

	   "extern crate iron;"

	   ; "#[macro_use] extern crate mime;"

	   "use iron::prelude::*;"
	   "use iron::status;"

	   (defun get_form (_)
	    (declare (type "&Request" _)
		      (immutable _)
		      (values "IronResult<Response>"))
	     (let ((response ("Response::new")))
	       (response.set_mut "status::Ok")
					;(response.set_mut (mime! "Text/Html; Charset=Utf8"))
	       (response.set_mut (string-r "<title>GCD Calculator</title>
<form action='/gcd' method='post'>
<input type='text' name='n'/>
<input type='text' name='m'/>
<button type='submit'>Compute GCD</button>
</form>"))
	       (return (Ok response))))
	   
	   (defun main ()
	     (println! (string "Serving on http://localhost:3000..."))
	     (dot ("Iron::new" get_form)
		  (http (string "localhost:3000"))
		  (unwrap))))))
    
    
    (write-source *code-file*
		  code)))

