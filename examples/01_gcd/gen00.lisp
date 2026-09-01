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


;; cargo new --bin rs01_gcd
;; cargo test
;; cargo run
;; cargo clean
;; cargo clippy
;; cargo clippy --fix --bin "rs01_gcd" -p rs01_gcd --tests
(progn
  (defparameter *source-dir* #P"examples/01_gcd/rs01_gcd/src/")
  (defparameter *code-file* (asdf:system-relative-pathname 'cl-rust-generator (merge-pathnames #P"main.rs"
											       *source-dir*)))
  (write-source
   *code-file*
   `(do0
     "use std::str::FromStr;"

     (defun gcd (n m)
       (declare (type u64 n m)
		(mutable n m)
		(values u64))
       (assert! (and (!= 0 n)
		     (!= 0 m)))
       (while (!= 0 m)
	      (when (< m n)
		(std--mem--swap "&mut m" "&mut n"))
	      (%= m n))
       n)

     "#[test]"
     (defun test_gcd ()
       (assert_eq! (gcd 14 15) 1)
       (assert_eq! (gcd (* 2 3 5 11 17)
			(* 3 7 11 13 19))
		   (* 3 11)))
      
     (defun main ()
       (let* ((numbers ("Vec::new")))
	 (for (arg (dot ("std::env::args")
			(skip 1)))
	      (numbers.push
	       (dot ("u64::from_str" &arg)
		    (expect (string "error parsing argument")))))
	 (when (numbers.is_empty)
	   (eprintln!
	    (string "Usage: gcd NUMBER ..."))
	   
	   ("std::process::exit" 1))
	 (let* ((d (aref numbers 0)))
	   (for (m (ref (aref numbers "1..")))
		(setf d (gcd d *m)))
	   (println! (string "The greatest common divisor of {:?} is {}")
		     numbers d)))))))

