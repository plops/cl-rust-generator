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
	   "#[macro_use]"
	   "extern crate glium;"

	   
	   (defun main ()
	     "#[allow(unused_imports)]"
	     "use glium::{glutin,Surface};"
	     (let ((event_loop ("glium::glutin::event_loop::EventLoop::new"))
		   (wb (dot ("glium::glutin::window::WindowBuilder::new")
			    (with_inner_size ("glium::glutin::dpi::LogicalSize::new" 512 512))
			    (with_title (string "vis"))))
		   (cb ("glium::glutin::ContextBuilder::new"))
		   (display (dot ("glium::Display::new" wb cb &event_loop)
				 (unwrap))))
	       ;(declare (mutable event_loop))


	       (space
		"#[derive(Copy,Clone)]"
		(defstruct0 Vertex
		    (position "[f32;2]")))
	       (implement_vertex! Vertex position)

	       (let ((shape (vec!
			     ,@(loop for (e f) in `((-.5 -.5)
						    (.0 .5)
						    (.5 -.25))
				  collect
				    `(make-instance Vertex
						    :position (list ,e ,f)))))
		     (vertex_buffer
		      (dot ("glium::VertexBuffer::new"
			    &display
			    &shape)
			   (unwrap)))
		     (indices
		      ("glium::index::NoIndices"
		       "glium::index::PrimitiveType::TrianglesList"))
		     (vertex_shader_source
		      (string# "#version 140
in vec2 position;
void main() {
  gl_Position = vec4(position,0.0,1.0);
}
"))
		     (fragment_shader_source
		      (string# "#version 140
out vec4 color;
void main() {
  color = vec4(1.0,0.0,0.0,1.0);
}
"))
		     (program (dot ("glium::Program::from_source"
				    &display
				    vertex_shader_source
				    fragment_shader_source
				    None)
				   (unwrap))))
		 )
	       
	       (event_loop.run
		(space move
		       (lambda (event _ control_flow)
			 (let ((next_frame_time (+ ("std::time::Instant::now")
						   ("std::time::Duration::from_nanos" "16_666_667"))))
			   (setf *control_flow
				 ("glium::glutin::event_loop::ControlFlow::WaitUntil"
				  next_frame_time))
			   (case event
			     ((space "glutin::event::Event::WindowEvent"
				     (curly event ".."))
			      (case event
				("glutin::event::WindowEvent::CloseRequested"
				 (setf *control_flow
				       "glutin::event_loop::ControlFlow::Exit")
				 (return))
				(t (return)))
			      )
			     (("glutin::event::Event::NewEvents" cause)
			       (case cause
				 ((space "glutin::event::StartCause::ResumeTimeReached"
					 (curly ".."))
				  "()")
				 ("glutin::event::StartCause::Init"
				  "()")
				 (t (return))))
			     (_ (return))))
					;(match event)
			 
			 (let ((target (display.draw)))
			   (declare (mutable target))
			   (target.clear_color 0.0 0.0 1.0 1.0)
			   (dot (target.draw &vertex_buffer
					 &indices
					 &program
					 "&glium::uniforms::EmptyUniforms"
					 ("&Default::default"))
				(unwrap))
			   (dot target
				(finish)
				(unwrap)))))))))))
    
    
    (write-source *code-file*
		  code)))

