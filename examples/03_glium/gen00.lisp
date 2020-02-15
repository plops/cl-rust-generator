(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)

;; cargo test
;; cargo run
;; cargo clean
;; cargo fix

;; https://github.com/glium/glium/blob/master/book/tuto-01-getting-started.md
;; https://docs.rs/glium/0.26.0/glium/
;; https://github.com/glium/glium/blob/master/examples/tutorial-01.rs
(progn
  (defparameter *source-dir* #P"examples/03_glium/rs03_glium/src/")
  (defparameter *code-file* (asdf:system-relative-pathname 'cl-rust-generator (merge-pathnames #P"main.rs"
											       *source-dir*)))

  (let ((code
	 `(do0

	   "extern crate glium;"
	   "use glium::Surface;"
	   (defun main ()
	     (let ((event_loop ("glium::glutin::event_loop::EventLoop::new"))
		   (wb (dot ("glium::glutin::window::WindowBuilder::new")
			    (with_inner_size ("glium::glutin::dpi::LogicalSize::new" 512 512))
			    (with_title (string "vis"))))
		   (cb ("glium::glutin::ContextBuilder::new"))
		   (display (dot ("glium::Display::new" wb cb &event_loop)
				 (unwrap))))
	       (declare (immutable wb cb display))
	       (event_loop.run
		(space move
		       (lambda (event _ control_flow)
			 (let ((next_frame_time (+ ("std::time::Instant::now")
						   ("std::time::Duration::from_nanos" "16_666_667"))))
			   (setf *control_flow
				 ("glium::glutin::event_loop::ControlFlow::WaitUntil"
				  next_frame_time)))
					;(match event)
			 (let ((target (display.draw)))
			   (target.clear_color 0.0 0.0 1.0 1.0)
			   (dot target
				(finish)
				(unwrap)))))))))))
    
    
    (write-source *code-file*
		  code)))

