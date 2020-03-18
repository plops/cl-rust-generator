(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)

(progn
  (defparameter *source-dir* #P"examples/13_vulkano/code/src/")
  
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
[dependencies]
vulkano-win = \"*\"
winit = \"*\"
chrono = \"*\"
"))
  
  (define-module
      `(main
	(do0

	 
	 (use (vulkano_win VkSurfaceBuild)
	      (winit (curly EventsLoop WindowBuilder))
	      (chrono Utc))
	 
	 (defun main ()
	   (declare (values "Result<(),Box<dyn Error>>"))
	   (let* ((events_loop (EventsLoop--new))
		  )
	     (let ((extensions (vulkano_win--required_extensions))
		   (instance (dot (Instance--new None &extensions None)
				  (expect (string "failed to create Vulkan instance"))))
		   (surface (dot (WindowBuilder--new)
				 (build_vk_surface
				  &events_loop
				  (instance.clone))
				 (unwrap))))))
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
