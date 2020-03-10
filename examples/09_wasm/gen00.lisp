(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)
;; https://dev.to/deciduously/reactive-canvas-with-rust-webassembly-and-web-sys-2hg2
;; cd code; wasm-pack build
;; cd code; npm init wasm-app www
;; cd code/www; npm install
;; cd code/www; npm run start
(progn
  (defparameter *source-dir* #P"examples/09_wasm/code/src/")
  
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
    (format s "~a"
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

[dependencies.web-sys]
version = \"*\" #.3
features = [\"Attr\",
\"CanvasRenderingContext2d\",
\"Document\",
\"Element\",
\"Event\",
\"EventTarget\",
\"HtmlCanvasElement\",
\"HtmlElement\",
\"HtmlInputElement\",
\"Node\",
\"Text\",
\"Window\"]

"))
  (defun append-attrs (doc el &rest attribs)
    "append-attrs document label (for size)"
    `(do0
      ,@(loop for (key val) in attribs collect
	   `(progn
	      (let ((attr (? (dot ,doc
				  (create_attribute (string ,key))))))
		(dot attr
		     (set_value (string ,val)))
		(? (dot ,el (set_attribute_node &attr))))))))
  (defun append-text-child (doc el text)
    `(progn
	(let ((text (dot ,doc
			 (create_text_node (string ,text)))))
	  
	  (? (dot ,el (append_child &text))))))
  (defun create-element-attrs (doc type &rest attribs)
    `(progn
	(let ((el (? (dot ,doc
			(create_element (string ,type))))))
	  ,(append-attrs doc 'el attribs)
	  (return el))))
  (defun append-element-attrs (doc parent type &rest attribs)
    `(progn
	(let ((el ,(create-element-attrs doc type attribs)))
	  (? (dot ,parent ,(append-child '&el))))))
  (defun append-text-element-attrs (doc parent type text &rest attribs)
    `(progn
       (let ((el ,(create-element-attrs doc type attribs)))
	 ,(append-text-child doc 'el text)
	  (? (dot ,parent ,(append-child '&el))))))
  
  (define-module
      `(lib
	(do0
	 (use (wasm_bindgen prelude *)
	      (web_sys Document))
	 
	 "type Result<T> = std::result::Result<T,JsValue>;"
	 #+nil (do0
	  "#[wasm_bindgen]"
	  (space extern
		 (progn
		   "pub fn alert (s: &str);")))
	 #+nil (do0
	  "#[wasm_bindgen]"
	  (space pub
		 (defun say_hi ()
		   (alert (string "hi from rust!")))))

	 (defun get_document ()
	   (declare (values "Result<Document>"))
	   (let ((window (dot (web_sys--window)
			      (unwrap))))
	     (return (Ok
		      (dot window
			   (document)
			   (unwrap))))))
	 
	 (do0
	  "#[wasm_bindgen]" 
	  (space pub
		 (defun run ()
		   (declare (values "Result<()>"))
		   (let (
			 (document (? (get_document)))
			 (body (dot document
				    (body)
				    (unwrap))))
		     (? (mount_app &document &body))
		     (return (Ok "()")))))
	  
	  )
	 (defun mount_app ("document: &web_sys::Document"
			   "body: &web_sys::HtmlElement")
	   (declare (values "Result<()>"))
	   (? (mount_title &document &body))
	   (return (Ok "()")))
	 (defun mount_title ("document: &web_sys::Document"
			     "body: &web_sys::HtmlElement")
	   (declare (values "Result<()>"))
	   (let ((title (dot document
			     (create_element (string "h1"))
			     (expect (string "title"))))
		 (title_text
		  (dot document
		       (create_text_node (string "DOT")))))
	     (dot title
		  (append_child &title_text)
		  (expect (string "append child to title")))
	     (dot
	      body
	      (append_child &title)
	      (expect (string "append title to body")))
	     (return (Ok "()"))))
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
