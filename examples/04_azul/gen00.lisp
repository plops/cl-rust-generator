(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)

(declaim (optimize (speed 0)
	  (debug 3)
	  (safety 3)))
#+nil
(progn
  (setf *features* (union *features* '()))
  (setf *features* (set-difference *features* '())))


;; cargo test
;; cargo run
;; cargo clean
;; cargo fix

;; https://github.com/maps4print/azul/wiki/Getting-Started
(progn
  (defparameter *source-dir* #P"examples/04_azul/rs04_azul/src/")
  (defparameter *code-file* (asdf:system-relative-pathname 'cl-rust-generator (merge-pathnames #P"main.rs"
											       *source-dir*)))

  (let ((code
	 `(do0

	   "extern crate azul;"
	   "use azul::{prelude::*,widgets::{label::Label,button::Button}};"
	   ;"struct MyDataModel {}"

	   (space
	    struct
	    MyDataModel
	    (curly
	     (comma "counter: usize")))

	   (defun update_counter ("event:CallbackInfo<MyDataModel>")
	     (declare (values UpdateScreen))
	     (incf event.state.data.counter 1)
	     (return Redraw))
	   
	   (space
	    "impl Layout for MyDataModel"
	    (curly
	     (defun layout ("&self, _info : LayoutInfo<Self>"
			    
			    )
	       (declare (values "Dom<Self>"))
	       (let ((label (dot ("Label::new"
				  (format! (string "{}")
					   self.counter))
				 (dom)))
		     (button (dot ("Button::with_label"
				  (string "counter"))
				  (dom)
				  (with_callback
				   "On::MouseUp"
				   update_counter))))
		 (declare (immutable label button))
		 (let ((dom (dot ("Dom::div")
				 (with_child label)
				 (with_child button))))
		   (return dom))))))
	   
	   
	   (defun main ()
	     (let ((app (dot ("App::new"
			      (space MyDataModel
				     (curly "counter: 0"))
			      ("AppConfig::default"))
			     (unwrap)))
		   (window (dot (app.create_window
				 ("WindowCreateOptions::default")
				 ("css::native"))
				(unwrap))))
	       (declare (immutable window))
	       (dot app
		    (run window)
		    (unwrap)))))))
    
    
    (write-source *code-file*
		  code)))

