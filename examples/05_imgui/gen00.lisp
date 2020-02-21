(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)

(declaim (optimize (speed 0)
		   (safety 3)
		   (debug 3)))

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
  (defun logprint (msg &optional (rest nil))
    `(progn
       (println! (string ,(format nil "{} {}:{} ~a ~{~a~^ ~}"
				  msg
				  (loop for e in rest collect
				       (format nil " ~a={}" (emit-rs :code e)))))
		 #+nil (dot (SystemTime--now)
		      (duration_since UNIX_EPOCH)
		      (expect (string "time went backwards"))
		      (as_micros))
		 (Utc--now)
		 (file!)
		 (line!)
		 ,@(loop for e in rest collect
			e ;`(dot ,e (display))
			))))


  (with-open-file (s (asdf:system-relative-pathname 'cl-rust-generator
						    (merge-pathnames #P"../Cargo.toml"
								     *source-dir*))
		     
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (format s "~a"
	    "[package]
name = \"rs03_glium\"
version = \"0.1.0\"
authors = [\"Martin Kielhorn <kielhorn.martin@gmail.com>\"]
edition = \"2018\"

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

[dependencies]
glium = \"*\"
imgui = \"*\"
imgui-glium-renderer = \"*\"
imgui-winit-support = \"*\"
chrono = \"*\"
reqwest = \"*\"
crossbeam-channel = \"*\"
"
	    


	    ))

  
  (let ((code
	 `(do0
	   (do0

	    (use (reqwest)
		 (chrono (curly DateTime Utc)))
	    
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


	   (impl System
		 (defun "main_loop<F: FnMut(&mut bool,&mut Ui)+'static>"
		     (self "mut run_ui: F")
		   (let (((make-instance System
					 event_loop
					 display
					 "mut imgui"
					 "mut platform"
					 "mut renderer"
					 "..") self)
			 (last_frame ("Instant::now")))
		     (declare (mutable last_frame))
		     (dot event_loop
			  (run
			   (space
			    move
			    (lambda (event _ control_flow)
			      (case event
				(("Event::NewEvents" _)
				 (setf last_frame (dot imgui
						       (io_mut)
						       (update_delta_time last_frame)))
				 )
				("Event::MainEventsCleared"
				 (let ((gl_window (display.gl_window)))
				   (dot platform
					(prepare_frame (imgui.io_mut)
						       (ref (gl_window.window)))
					(expect (string "failed to prepare frame")))
				   (dot gl_window
					(window)
					(request_redraw))))
				(("Event::RedrawRequested" _)
				 (let* ((ui (imgui.frame))
					(run true))
				   (run_ui "&mut run"
					   "&mut ui")
				   (unless run
				     (setf *control_flow
					   "ControlFlow::Exit"))
				   (let ((gl_window (display.gl_window)))
				     (let* ((target (display.draw)))
				       (target.clear_color_srgb 1s0 1s0 1s0 1s0)
				       (platform.prepare_render &ui
								(gl_window.window))
				       (let ((draw_data (ui.render)))
					 (dot renderer
					      (render "&mut target"
						      draw_data)
					      (expect (string "rendering failed")))
					 (dot target
					      (finish)
					      (expect (string "swap buffer failed"))))))))
				((make-instance "Event::WindowEvent"
						:event "WindowEvent::CloseRequested"
						"..")
				 (setf *control_flow "ControlFlow::Exit"))
				(event
				 (let ((gl_window (display.gl_window)))
				   (platform.handle_event (imgui.io_mut)
							  (gl_window.window)
							  &event)))))))))))
	   
	   (defun main ()
	     #+nil (let* ((client (request--Client--new))
		    (body (dot client
			       (get (string "https://query1.finance.yahoo.com/v7/finance/quote?lang=en-US&region=US&corsDomain=finance.yahoo.com&symbols=DBX,LITE,AMD,INTC&fields=regularMarketPrice"))
			       (? await)
			       (text)
			       (? await))))
	       
	       ,(logprint "stock" `(body)))
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
					 (ui.text (format!
						   (string "mouse: ({:.1},{:.1})"
							   )
						   (aref mouse_pos 0)
						   (aref mouse_pos 1)))))))
			 (dot ("Window::new" (im_str! (string "recv")))
			      (size (list 200.0 100.0)
				    "Condition::FirstUseEver")
			      (build ui
				     (lambda ()
				       (ui.text (im_str! (string "recv")))
				       )))))))))))

    
    
    (write-source *code-file*
		  code)))

