(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)

(declaim (optimize (speed 0)
	  (debug 3)
	  (safety 3)))
#+nil
(progn
  (setf *features* (union *features* '()))
  (setf *features* (set-difference *features* '())))


;; cargo test
;; cargo run
;; cargo clean
;; cargo fix

;; https://github.com/maps4print/azul/wiki/Getting-Started
(progn
  (defparameter *source-dir* #P"examples/04_azul/rs04_azul/src/")
  (defparameter *code-file* (asdf:system-relative-pathname 'cl-rust-generator (merge-pathnames #P"main.rs"
											       *source-dir*)))

  (let ((code
	 `(do0

	   "extern crate azul;"
	   
	   (defun main ()
	     ))))
    
    
    (write-source *code-file*
		  code)))

