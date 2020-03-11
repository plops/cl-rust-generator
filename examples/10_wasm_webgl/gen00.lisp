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
	 
	 ;"type Result<T> = std::result::Result<T,JsValue>;"

	 (do0
	  (space pub
		 (defun compile_shader ("context: &WebGlRenderingContext"
					"shader_type: u32"
					"source: &str")
		   (declare (values "Result<WebGlShader,String>"))
		   (let ((shader (dot context
				      (create_shader shader_type)
				      (? (ok_or_else (lambda ()
						       (return (String--from
							 (string "unable to create shader object")))))))))
		     (context.shader_source &shader source)
		     (context.compile_shader &shader)
		     (if (dot context
			      (get_shader_parameter &shader
						    WebGlRenderingContext--COMPILE_STATUS)
			      (as_bool)
			      (unwrap_or false))
			 (do0
			  (return (Ok shader)))
			 (do0
			  (return 
			   (Err (dot context
				     (get_shader_info_log &shader)
				     (unwrap_or_else (lambda ()
						       (return (String--from
								(string "unknown error creating shader"))))))))))))))
	 (do0
	  (space pub
		 (defun link_program ("context: &WebGlRenderingContext"
				      "vert_shader: &WebGlShader"
				      "frag_shader: &WebGlShader"
				      )
		   (declare (values "Result<WebGlProgram,String>"))
		   (let ((program (dot context
				       (create_program)
				       (? (ok_or_else (lambda ()
							(return (String--from
								 (string "unable to create shader object")))))))))

		     (context.attach_shader &program vert_shader)
		     (context.attach_shader &program frag_shader)
		     (context.link_program &program)
		     (if (dot context
			      (get_program_parameter &program
						     WebGlRenderingContext--LINK_STATUS)
			      (as_bool)
			      (unwrap_or false))
			 (do0
			  (return (Ok program)))
			 (do0
			  (return 
			   (Err (dot context
				     (get_program_info_log &program)
				     (unwrap_or_else (lambda ()
						       (return (String--from
								(string "unknown error creating program object"))))))))))))))
	 
	 (do0
	  "#[wasm_bindgen]"
	  (space pub
		 (defun start ()
		   (declare (values "Result<(),JsValue>"))
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
					("dyn_into::<WebGlRenderingContext>"))))
			 (vert_shader (? (compile_shader &context
						       WebGlRenderingContext--VERTEX_SHADER
						       (string# "attribute vec4 position;
void main(){
  gl_Position = position;
}
"))))
			 (frag_shader (? (compile_shader &context
						       WebGlRenderingContext--VERTEX_SHADER
						       (string# "void main(){
  gl_FragColor = vec4(1.0,1.0,1.0,1.0);
}
"))))
			 (program (? (link_program &context &vert_shader &frag_shader))))
		     (declare (type "web_sys::HtmlCanvasElement"
				    canvas))
		     (context.use_program (Some &program))
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
