(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)

;; cargo new mandelbrot
;; cargo test
;; cargo run
;; cargo clean
;; cargo clippy
;; cargo clippy --fix --bin "rs01_gcd" -p rs01_gcd --tests
(progn
  (defparameter *source-dir* #P"examples/21_mandelbrot/mandelbrot/src/")
  (defparameter *code-file* (asdf:system-relative-pathname 'cl-rust-generator (merge-pathnames #P"main.rs"
											       *source-dir*)))
  (write-source
   *code-file*
   `(do0
     "use num::Complex;"

     (defun escape_time (c limit)
       (declare (type Complex<f64> c)
		(type usize limit)
		(mutable n m)
		(values Option<usize>))
       (let* ((z (make-instance Complex :re .0 :im .0)))
	 (for (i (slice 0 limit))
	      (if (< 4.0 (z.norm_sqr))
		  (return (Some i))
		  (setf z (+ (* z z) c)))))
       None)

     "use std::str::FromStr;"
     "/// parse string s of the form <left><sep><right> like 400x600 or 1,5"
     (defun "parse_pair<T: FromStr>" (s separator)
       (declare (type &str s)
		(type char separator)
		(values "Option<(T,T)>"))
       (case (s.find separator)
	 (None None)
	 ((Some index)
	  (case (tuple (T--from_str (ref (aref s "..index")))
		       (T--from_str (ref (aref s (+ index "1..")))))
	    ((tuple (Ok l)
		    (Ok r))
	     (Some (tuple l r)))
	    (t None)))))

     "#[test]"
     (defun test_parse_pair ()
       ,@(loop for (type s sep result) in `((i32 "" "," None)
					    (i32 "10," "," None)
					    (i32 ",10" "," None)
					    (i32 "10,20" "," (Some (tuple 10 20)))
					    (i32 "10,20xy" "," None)
					    (f64 "0.5x" "x" None)
					    (f64 "0.5x1.5" "x" (Some (tuple .5 1.5)))
				    )
	       collect
	       `(assert_eq! ((scope parse_pair (angle ,type)) (string ,s)
					       (char ,sep))
			    ,result)))
     
     #+nil(defun main ()
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

