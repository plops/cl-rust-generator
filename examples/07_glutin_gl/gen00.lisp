(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)

(declaim (optimize (speed 0)
		   (safety 3)
		   (debug 3)))

;; https://gist.github.com/matthewjberger/9da00592b472b50ec1e6b3238719264b#file-main-rs-L3
;; this downloads 97 crates
(progn
  (defparameter *source-dir* #P"examples/07_glutin_gl/code/src/")

  

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
			e ;`(dot ,e (display))
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
    (format s "~a"
	    "[package]
name = \"code\"
version = \"0.1.0\"
authors = [\"Martin Kielhorn <kielhorn.martin@gmail.com>\"]
# edition = \"2018\"

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

[dependencies]
glutin = \"*\"
gl = \"*\"
"


	    ))
  (let ((screen-width 512)
	(screen-height 512))
   (define-module
       `(main
	 (do0
	  "extern crate gl;"
	  "extern crate glutin;"
	  (use (glutin GLContext)
	       (gl types *)
	       (std (curly mem ptr str))
	       (std os raw c_void)
	       (std ffi CString))
	  (defun main ()
	    (let* ((events_loop (glutin--event_loop--EventLoop--new)))
	      (let ((window (dot (glutin--WindowBuilder--new)
				 (with_title (string "glutin thing"))
				 (with_dimensions ,screen-width
						  ,screen-height)))
		    (context (dot (glutin--ContextBuilder--new)
				  (with_vsync true)))
		    (gl_window (dot (glutin--GLWindow--new
				     window
				     context
				     &events_loop)
				    (unwrap))))
		(space unsafe
		       (progn
			 (dot gl_window
			      (make_current)
			      (unwrap))
			 (gl--load_with
			  (lambda (symbol)
			    (coerce (dot gl_window
					 (get_proc_address symbol))
				    *const)
			    (return _)))
		      )))))))))


  (loop for e in (reverse *module*) and i from 0 do
	   (destructuring-bind (&key name code) e
	     (write-source (asdf:system-relative-pathname 'cl-rust-generator
							  (merge-pathnames (format nil "~a.rs" name)
									   *source-dir*))
			   `(do0
			     "#[allow(unused_parens)]"
			     (use (chrono (curly DateTime Utc)))
			     ,code)))))
