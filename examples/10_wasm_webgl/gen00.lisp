(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator")
  (ql:quickload "cl-cpp-generator2"))


(in-package :cl-cpp-generator2)

(let ((name "trace.frag"))
  (defparameter *source-dir* #P"examples/10_wasm_webgl/code/src/")
 (write-source (asdf:system-relative-pathname 'cl-rust-generator
					      (merge-pathnames (format nil "~a" name)
							       *source-dir*))
	       `(do0
		 (defun main ()
		   #+nil (let ((uv (/ gl_FragCoord.xy
				(vec2 512s0 512s0))))
		     (declare (type vec2 uv)))
		   (setf gl_FragColor (vec4 1s0 1s0 (+ .5 (* .5 (sin 1s0))) 1s0))))))

(in-package :cl-rust-generator)


;; https://rustwasm.github.io/wasm-bindgen/examples/webgl.html
;; cd code; wasm-pack build --dev
;; cd code; npm init wasm-app www
;; cd code/www; npm install
;; cd code/www; npm run start

;; https://rustwasm.github.io/2018/06/25/vision-for-rust-and-wasm.html
;; cargo install twiggy
;; twiggy top target/wasm32-unknown-unknown/release/code.wasm

;; interactivity
;; https://rustwasm.github.io/book/game-of-life/hello-world.html

;; shader
;; https://youtu.be/Cfe5UQ-1L9Q?t=1364

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
console_error_panic_hook = \"*\"

[dependencies.web-sys]
version = \"*\" #.3
features = [~{'~a'~^,~}]

[package.metadata.wasm-pack.profile.dev]
wasm-opt = ['-O']

[package.metadata.wasm-pack.profile.dev.wasm-bindgen]
debug-js-glue = true
demangle-name-section = true
dwarf-debug-info = true
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
	 "extern crate console_error_panic_hook;"
	 (use (wasm_bindgen prelude *)
	      (wasm_bindgen JsCast)
	      (web_sys (curly WebGlProgram
			      WebGlRenderingContext
			      WebGlShader))
	      (std panic))
	 
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
		 (defun run ()
		   (declare (values "Result<(),JsValue>"))
		   (std--panic--set_hook
		    (Box--new
		     console_error_panic_hook--hook))
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
						       WebGlRenderingContext--FRAGMENT_SHADER
						       (include_str! (string "trace.frag"))
						       #+nil (string# "void main(){
  gl_FragColor = vec4(1.0,1.0,1.0,1.0);
}
"))))
			 (program (? (link_program &context &vert_shader &frag_shader))))
		     (declare (type "web_sys::HtmlCanvasElement"
				    canvas))
		     (context.use_program (Some &program))
		     ,(let ((l `(-.7 -.7
				     0 .7
				     -.7 0
				     0 .7
				     0)))
			`(let ((vertices (list ,@(mapcar (lambda (x) (* 1s0 x))
							  l)))
			       (buffer (dot context
					    (create_buffer)
					    (? (ok_or (string "failed to create buffer"))))))
			   (declare (type ,(format nil "[f32;~a]" (length l))
					  vertices))
			   (context.bind_buffer WebGlRenderingContext--ARRAY_BUFFER (Some &buffer))
			   "// don't do memory allocations until view is dropped"
			   (space unsafe
				  (progn
				    (let ((vert_array (js_sys--Float32Array--view &vertices)))
				      (context.buffer_data_with_array_buffer_view
				       WebGlRenderingContext--ARRAY_BUFFER
				       &vert_array
				       WebGlRenderingContext--STATIC_DRAW))))
			   (context.vertex_attrib_pointer_with_i32
			    0 3 WebGlRenderingContext--FLOAT false 0 0)
			   (context.enable_vertex_attrib_array 0)
			   (context.clear_color 0s0 0s0 0s0 1s0)
			   (context.clear WebGlRenderingContext--COLOR_BUFFER_BIT)
			   (context.draw_arrays
			    WebGlRenderingContext--TRIANGLES
			    0
			    (coerce (/ (vertices.len)
				       3)
				    i32))))
		     (return (Ok "()")))))))))
 

  (loop for e in (reverse *module*) and i from 0 do
       (destructuring-bind (&key name code) e
	 (write-source (asdf:system-relative-pathname 'cl-rust-generator
						      (merge-pathnames (format nil "~a.rs" name)
								       *source-dir*))
		       `(do0
			 "#![allow(unused_parens)]"
					;(use (chrono (curly DateTime Utc)))
			 ,code)))))
