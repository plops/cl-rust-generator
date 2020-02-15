(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)

;; cargo test
;; cargo run
;; cargo clean
;; cargo fix
;; https://www.youtube.com/watch?v=3dGnS9Dp5kc imgui-rs: Immediate mode GUIs with Rust
;; https://github.com/Gekkio/imgui-rs/blob/master/imgui-examples/examples/hello_world.rs
(progn
  (defparameter *source-dir* #P"examples/05_imgui/rs05_imgui/src/")
  (defparameter *code-file*
    (asdf:system-relative-pathname 'cl-rust-generator
				   (merge-pathnames #P"main.rs"
						    *source-dir*)))

  (let ((code
	 `(do0
	   (do0
	    "use imgui::*;"
	    ,@(loop for e in `("glutin"
			       "glutin::event::{Event,WindowEvent}"
			       "glutin::event_loop::{ControlFlow,EventLoop}"
			       "glutin::window::WindowBuilder"
			       "{Display,Surface}")
		 collect
		   (format nil "use glium::~a;" e))
	    "use imgui::{Context,FontConfig,FontGlyphRanges,FontSource,Ui};"
	    "use imgui_glium_renderer::Renderer;"
	    "use imgui_winit_support::{HiDpiMode,WinitPlatform};"
	    "use std::time::Instant;")
	   

	   (defstruct0 System
	       (event_loop "EventLoop<()>")
	     (display "glium::Display")
	     (imgui Context)
	     (platform WinitPlatform)
	     (renderer Renderer)
	     (font_size f32))


	   (defun init ("title: &str")
	     (declare (values System))
	     (let ((title (case (title.rfind (string "/"))
			    ((Some idx) (dot title (split_at (+ idx 1)) 1))
			    (None title)))
		   (event_loop ("EventLoop::new"))
		   (context (dot ("glutin::ContextBuilder::new")
				 (with_vsync true)))
		   (builder (dot ("WindowBuilder::new")
				 (with_title (title.to_owned))
				 (with_inner_size
				  ("glutin::dpi::LogicalSize::new"
				   "512f64"
				   "512f64"))))
		   (display (dot ("Display::new"
				  builder
				  context
				  &event_loop)
				 (expect (string "failed to initialize display"))))
		   (imgui ("Context::create")))
	       (declare (mutable imgui))
	       (imgui.set_ini_filename None)
	       (let* ((platform ("WinitPlatform::init"
				 "&mut imgui")))
		 (progn
		   (let ((gl_window (display.gl_window))
			 (window (gl_window.window)))
		     (platform.attach_window (imgui.io_mut)
					     &window
					     "HiDpiMode::Rounded"))))
	      #+Nil(let ((hidpi_facotr (platform.hidpi_factor))
		     (font_size (* 13s0 hidpi_factor)))
		 (declare (type f32 font_size))
		 (dot imgui
		      (fonts)
		      (add_font
		       (ref (list
			     (make-instance "FontSource::DefaultFontData"
					    :config (Some
						     (make-instance
						      FontConfig
						      :size_pixels font_size
						      "" ("..FontConfig::default"))))
			     (make-instance "FontSource::TtfData"
					    ))))))
	      (let ((renderer (dot ("Renderer::init"
				    "&mut imgui"
				    &display)
				   (expect (string "failed to initialize renderer")))))
		(return (make-instance System
				       event_loop
				       display
				       imgui
				       platform
				       renderer
				       :font_size 12s0)))))
	   
	   (defun main ()
	     (let ((system (init (file!))))
	       (system.main_loop
		(space move
		       (lambda (_ ui)
			 (dot ("Window::new" (im_str! (string "Hello world")))
			      (size (list 300.0 100.0)
				    "Condition::FirstUseEver")
			      (build ui
				     (lambda ()
				       (ui.text (im_str! (string "Hello World")))
				       (let ((mouse_pos (dot ui
							     (io)
							     mouse_pos)))
					 (ui.text (format
						   (string "mouse: ({:.1},{:.1})"
							   )
						   (aref mouse_pos 0)
						   (aref mouse_pos 1)))))))))))))))

    
    
    (write-source *code-file*
		  code)))

