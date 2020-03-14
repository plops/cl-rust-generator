(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)
;; https://github.com/bheisler/rustacuda
(progn
  (defparameter *source-dir* #P"examples/11_cuda/code/src/")
  
  (defun logprint (msg &optional (rest nil))
    `(progn
       (println! (string ,(format nil "{} {}:{} ~a ~{~a~^ ~}"
				  msg
				  (loop for e in rest collect
				       (format nil " ~a={}" (emit-rs :code e)))))

		 (Utc--now)
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
chrono = \"*\"
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
	      (std ffi CString)
	      (chrono Utc))
	 
	 (defun main ()
	   (declare (values "Result<(),Box<dyn Error>>"))
	   (? (rustacuda--init (CudaFlags--empty)))
	   ,(logprint "get device")
	   (let ((device (? (Device--get_device 0)))
		 (_ctx (? (Context--create_and_push
			      (logior
			       ContextFlags--MAP_HOST
			       ContextFlags--SCHED_AUTO)
			      device)))
		 (ptx (? (CString--new
			(include_str! (string "add.ptx")))))
		 (module (? (Module--load_from_string &ptx)))
		 (stream (? (Stream--new StreamFlags--NON_BLOCKING None)))
		 )
	     
	     (progn
	       ,(logprint "allocate buffers")
	       ,(let ((l `((in_x "1.0f32")
			  (in_y "2.0f32")
			  (out_1 "0.0f32")
			  (out_2 "0.0f32"))))
		 `(let* (,@(loop for (var val) in l
			      collect
				`(,var (? (DeviceBuffer--from_slice
					   ,(format nil "&[~a; 10]" val))))))
		    #+nil ,@(loop for (var val) in l
			 collect
			   (logprint "" `((dot ,var capacity))))
		    ,(logprint "launch")
		    (space unsafe
			   (progn
			     (let ((result (launch!
					    ("module.sum<<<1,1,0,stream>>>"
					     ,@(loop for (e f) in l collect
						    `(dot ,e (as_device_ptr)))))))
			       (? result))))
		    ,(logprint "sync")
		    (? (stream.synchronize))
		    (let* ((out_host "[0.0f32; 20]"))
		      (? (dot out_1 (copy_to "&mut out_host[0..10]")))
		      (? (dot out_2 (copy_to "&mut out_host[10..20]"))))
		    (for (x (out_host.iter))
			 ,(logprint "" `(x))
			 #+nil
			 (assert_eq!
			  (coerce 3.0 u32)
			  (coerce *x u32)))
		    ))))
	   (return (Ok "()"))))))
 

  (loop for e in (reverse *module*) and i from 0 do
       (destructuring-bind (&key name code) e
	 (write-source (asdf:system-relative-pathname 'cl-rust-generator
						      (merge-pathnames (format nil "~a.rs" name)
								       *source-dir*))
		       `(do0
			 "#![allow(unused_parens)]"
					;(use (chrono (curly DateTime Utc)))
			 ,code)))))
