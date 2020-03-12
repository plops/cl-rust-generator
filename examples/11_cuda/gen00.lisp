(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)
;; https://github.com/bheisler/rustacuda
(progn
  (defparameter *source-dir* #P"examples/11_cuda/code/src/")
  
  (defun logprint (msg &optional (rest nil))
    `(progn
       (println! (string ,(format nil "{}:{} ~a ~{~a~^ ~}"
				  msg
				  (loop for e in rest collect
				       (format nil " ~a={}" (emit-rs :code e)))))

		 ;;(Utc--now)
		 (file!)
		 (line!)
		 ,@(loop for e in rest collect
		      e			;`(dot ,e (display))
			))))
  
  (defparameter *module* nil)
  (defun define-module (args)
    (destructuring-bind (module-name module-code) args
      (push `(:name ,module-name :code ,module-code)
	    *module*)))



  (with-open-file (s (asdf:system-relative-pathname 'cl-rust-generator
						    (merge-pathnames #P"../Cargo.toml"
								     *source-dir*))
		     
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (format s 
	    "[package]
name = \"code\"
version = \"0.1.0\"
authors = [\"Martin Kielhorn <kielhorn.martin@gmail.com>\"]
edition = \"2018\"

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

[dependencies]
rustacuda = \"*\"
rustacuda_core = \"*\"
rustacuda_derive = \"*\"
"))
  
  (define-module
      `(main
	(do0
	 "#[macro_use]"
	 "extern crate rustacuda;"
	 "#[macro_use]"
	 "extern crate rustacuda_derive;"
	 "extern crate rustacuda_core;"
	 (use (rustacuda prelude *)
	      (std error Error)
	      (std ffi CString))
	 
	 (defun main ()
	   (declare (values "Result<(),Box<dyn Error>>"))
	   ((rustacuda--init (CudaFlags--empty)) ?)
	   (let ((device (? (Device--get_device 0)))
		 (context (? (Context--create_and_push
			      (logior
			       ContextFlags--MAP_HOST
			       ContextFlags--SCHED_AUTO)
			      device)))
		 (module_data (CString--new
			       (include_str! (string "add.ptx"))))
		 (module (? (Module--load_from_string &module_data)))
		 (stream (? (Stream--new StreamFlags--NON_BLOCKING None)))
		 )
	     (let* ((x (? (DeviceBox--new &10.0f32)))
		    (y (? (DeviceBox--new &20.0f32)))
		    (result (? (DeviceBox--new &0.0f32))))))))))
 

  (loop for e in (reverse *module*) and i from 0 do
       (destructuring-bind (&key name code) e
	 (write-source (asdf:system-relative-pathname 'cl-rust-generator
						      (merge-pathnames (format nil "~a.rs" name)
								       *source-dir*))
		       `(do0
			 "#![allow(unused_parens)]"
					;(use (chrono (curly DateTime Utc)))
			 ,code)))))
