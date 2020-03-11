(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)
;; https://rustwasm.github.io/wasm-bindgen/examples/webgl.html
;; cd code; wasm-pack build
;; cd code; npm init wasm-app www
;; cd code/www; npm install
;; cd code/www; npm run start
(progn
  (defparameter *source-dir* #P"examples/10_wasm_webgl/code/src/")
  
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

[lib]
crate-lib = [\"cdylib\"]
crate-type = [\"cdylib\",\"rlib\"]

[dependencies]
wasm-bindgen = \"*\" #.2
js-sys = \"*\"

[dependencies.web-sys]
version = \"*\" #.3
features = [~{'~a'~^,~}]
"
	    `(Document Document	Element HtmlCanvasElement
		       WebGlBuffer
		       WebGlRenderingContext
		       WebGlProgram
		       WebGlShader
		       Window)))
  
  (define-module
      `(lib
	(do0
	 (use (wasm_bindgen prelude *)
	      (wasm_bindgen JsCast)
	      (web_sys (curly WebGlProgram
			      WebGlRenderingContext
			      WebGlShader)))
	 
	 "type Result<T> = std::result::Result<T,JsValue>;"
	 (do0
	  (space pub
		 (defun start ()
		   (declare (values "Result<()>"))
		   (let ((document (dot (web_sys--window)
					(unwrap)
					(document)
					(unwrap)))
			 (canvas_el (dot document
				      (get_element_by_id (string "canvas"))
				      (unwrap)))
			 (canvas (? (dot canvas_el
					 ("dyn_into::<web_sys::HtmlCanvasElement>"))))
			 (context (dot canvas
				       (? (get_context (string "webgl")))
				       (unwrap)
				       (?
					("dyn_into::<WebGlRenderingContext>")))))
		     (return (Ok "()"))))))


	 )))
 

  (loop for e in (reverse *module*) and i from 0 do
       (destructuring-bind (&key name code) e
	 (write-source (asdf:system-relative-pathname 'cl-rust-generator
						      (merge-pathnames (format nil "~a.rs" name)
								       *source-dir*))
		       `(do0
			 "#[allow(unused_parens)]"
					;(use (chrono (curly DateTime Utc)))
			 ,code)))))
