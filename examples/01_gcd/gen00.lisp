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
	    (defun main ()
	      (declare (values int))))))
    
    
    (write-source *code-file*
		  code)))

