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

  #+nil (eprintln!
      (string ,(format nil "~a~{~a~^ ~}"
		       (if (string= msg "")
			   ""
			   (format nil "~a " msg))
		       (loop for e in vars
			     collect		     (format nil "~a={~a}" (emit-rs :code e) (emit-rs :code e)))))
      )
  (defun lprint (&key (msg "")
		   (vars nil))
  
  
    
    `(eprintln!
      (string ,(format nil "~a~{~a~^ ~}"
		       (if (string= msg "")
			   ""
			   (format nil "~a " msg))
		       (loop for e in vars
			     collect
			     (format nil "~a={}" (emit-rs :code e)))))
       ,@(loop for e in vars
	    collect
	    (emit-rs :code e))
      ))
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

     (defun parse_complex (s)
       (declare (type &str s)
		(values (space Option (angle Complex (angle f64)))))
       (dot (parse_pair s (char ","))
	    (map (lambda ((tuple  re im))
		   (make-instance Complex re im)))))

     "#[test]"
     (defun test_parse_complex ()
       (assert_eq! (parse_complex (string "1.25,-.0625"))
		   (Some (make-instance Complex :re 1.25
						:im -.0625)))
       (assert_eq! (parse_complex (string ",-.06"))
		   None))

     "/// Given row and column of a pixel in image grid return corresponding point on the complex plane"
     (defun pixel_to_point (bounds pixel upper_left lower_right)
       (declare (values Complex<f64>)
		(type (tuple usize usize) bounds pixel)
		(type Complex<f64> upper_left lower_right))
       (let (((tuple width height)
	       (tuple (- lower_right.re
			 upper_left.re)
		      (- upper_left.im
			 lower_right.im))))
	 ;; pixel.1 increases as we go down, imaginary component increases as we go up
	 (make-instance Complex
			:re (+ upper_left.re (/
					      (* (coerce pixel.0 f64)
						 width)
					      (coerce bounds.0 f64)))
			:im (- upper_left.im (/
					      (* (coerce pixel.1 f64)
						 height)
					      (coerce bounds.1 f64))))))

     "#[test]"
     (defun test_pixel_to_point ()
       (assert_eq! (pixel_to_point (tuple 100 200)
				   (tuple 25 175)
				   (make-instance Complex :re -1.0 :im 1.0)
				   (make-instance Complex :re 1.0 :im -1.0))
		   (make-instance Complex :re -.5 :im -.75)))

     (defun render (pixels bounds upper_left lower_right)
       (declare (type "&mut [u8]" pixels)
		(type (tuple usize usize) bounds)
		(type Complex<f64> upper_left lower_right)
		)
       (assert! (== (pixels.len)
		    (* bounds.0 bounds.1)))
       (for (row 0..bounds.1)
	    (for (column 0..bounds.0)
		 (let ((point (pixel_to_point bounds
					      (tuple column row)
					      upper_left
					      lower_right)))
		   (setf (aref pixels (+ (* row bounds.0)
					 column))
			 ,(let ((max-value 4095)
				)
			   `(case (escape_time point ,max-value)
			     (None 0) ;; point belongs to mandelbrot set
			     ((Some count)
			      (coerce #+nil (/ ;; linear
					     (coerce count f32)
					 ,(/ ,max-value 256f0))
				      #+nil ,(let (((alpha-value 1.2f0)))
					      `(/ ;; exponential 
					       (- (dot (* ,(/ alpha-value max-value)
							  (coerce count f32))
						       (exp)) 1f0)
					       ,(/ (- (exp alpha-value) 1)
						   255)))
				      
				      ,(let ((gamma-value .3f0))
					 ;; gamma
					 `(dot (/ (coerce count f32)
						  ,(coerce (/ 4095d0
							      (expt 255d0 (/ 1d0 gamma-value)))
							   'single-float))
					       (powf ,gamma-value)))
				      u8)))))))))

     
     (use (image (curly ExtendedColorType ImageEncoder ImageError))
	  (image codecs webp WebPEncoder)
	  (std fs File))

     (defun write_image (filename pixels bounds)
       (declare (type &str filename)
		(type "&[u8]" pixels)
		(type (tuple usize usize) bounds)
		(values (space Result (angle (comma "()" ImageError)))))
       (let ((output (? (File--create filename)))
	     (encoder (WebPEncoder--new_lossless output)))
	 (? (encoder.write_image pixels
				 (coerce bounds.0 u32)
				 (coerce bounds.1 u32)
				 ExtendedColorType--L8))
	 (Ok "()")))

     (use (std env))
     
     (defun main ()
       (let ((args (dot (env--args) (collect))))
	 (declare (type Vec<String> args))
	 (when (!= 5 (args.len))
	   (let ((program (aref &args 0)))
	     (eprintln! (string "Usage:   {program} FILE        PIXELS   LEFT,TOP RIGHT,BOTTOM"))
	     (eprintln! (string "Example: {program} mandel.webp 1000x750 -1.2,.35 -1,.2"))
	     (std--process--exit 1)))
	 
	 (let ((bounds (dot (parse_pair (aref &args 2)
					(char "x"))
			    (expect (string "error parsing image dimensions"))))
	       (upper_left (dot (parse_complex (aref &args 3))
				(expect (string "error parsing upper left corner point"))))
	       (lower_right (dot (parse_complex (aref &args 4))
				 (expect (string "error parsing lower right corner point")))))
	   (declare (type		;(tuple usize usize)
		     "(usize, usize)"
		     bounds))
	   
	   
	   (let* ((pixels (aref vec! (semicolon 0 (* bounds.0 bounds.1)))))
	     #+nil (render (ref-mut pixels)
			   bounds upper_left lower_right)
	     (let ((threads (dot (std--thread--available_parallelism)
				 (expect (string "error querying CPU count"))
				 (get)))
		   (rows_per_band (bounds.1.div_ceil threads))
		   (bands (pixels.chunks_mut (* rows_per_band bounds.0))))
	       ,(lprint :vars `(bounds.0 bounds.1
					 upper_left lower_right
					 threads
					 rows_per_band))
	       (std--thread--scope (lambda (spawner)
				     (for ((tuple i band)
					   (bands.enumerate))
					  (let ((top (* rows_per_band i))
						(height (/ (band.len)
							   bounds.0))
						(band_bounds (tuple bounds.0 height))
						(band_upper_left (pixel_to_point bounds
										 (tuple 0 top)
										 upper_left
										 lower_right))
						(band_lower_right (pixel_to_point bounds
										  (tuple bounds.0 (+ top height))
										  upper_left lower_right)))
					    ,(lprint :vars `(i top height band_bounds.0 band_bounds.1 band_upper_left band_lower_right))
					    (do0 ;; hack to force semicolon
					     (spawner.spawn (space move (lambda ()
									  (do0 (render band band_bounds
										       band_upper_left band_lower_right)))))))))))
	     (dot (write_image (aref &args 1)
			       &pixels bounds)
		  (expect (string "error writing PNG file"))))
	   ))))))

