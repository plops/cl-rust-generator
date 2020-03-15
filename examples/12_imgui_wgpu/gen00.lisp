(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)
;; https://github.com/unconed/imgui-wgpu-rs
;; Building_WebGPU_with_Rust_-_Dzmitry_Malyshau_-_FOSDEM_20-Z1l91Y3_oyw
(progn
  (defparameter *source-dir* #P"examples/12_imgui_wgpu/code/src/")
  
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
wgpu = \"*\"
#cgmath = \"*\"
#imgui = \"*\"
##imgui-winit-support = \"*\"
#imgui-wgpu = \"*\"

[dev-dependencies]
winit = \"*\"
"
))
  
  (define-module
      `(main
	(do0
	 (use
	  (wgpu)
	  ;(imgui *)
					;(imgui_wgpu Renderer)
					;(imgui_winit_support)
	  )
	 #+nil
	 
	 (use (winit (curly WindowBuilder
			    Event
			    WindowEvent
			    EventsLoop
			    KeywordInput
			    VirtualKeyCode
			    ElementState
			    dpi--LogicalSize))
	      (imgui *)
	      (imgui_wgpu--Renderer)
	      (imgui_winit_support)
	      (std time Instant))
	 (defun main ()
	   (let ((adapter (dot (wgpu--Adapter--request
			    (ref
			     (make-instance
			      wgpu--RequestAdapterOptions
			      :power_preference wgpu--PowerPreference--Default
			       :backends wgpu--BackendBit--PRIMARY))
			   
			    )
			       (unwrap)))
		 #+nil (instance (wgpu--Instance--new))
		 #+nil (adapter (instance.get_adapter
			   (ref
			    (make-instance
			     wgpu--AdapterDescriptor
			     :power_preference wgpu--PowerPreference--HighPerformance)))))
	     )
	   ))))
 

  (loop for e in (reverse *module*) and i from 0 do
       (destructuring-bind (&key name code) e
	 (write-source (asdf:system-relative-pathname 'cl-rust-generator
						      (merge-pathnames (format nil "~a.rs" name)
								       *source-dir*))
		       `(do0
			 "#![allow(unused_parens)]"
					;(use (chrono (curly DateTime Utc)))
			 ,code)))))
