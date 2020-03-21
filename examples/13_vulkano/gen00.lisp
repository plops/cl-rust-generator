(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)
;; https://vulkano.rs/guide/example-operation
(progn
  (defparameter *source-dir* #P"examples/13_vulkano/code/src/")
  
  (defun logprint (msg &optional (rest nil))
    `(progn
       (println! (string ,(format nil "{} {}:{} ~a ~{~a~^ ~}"
				  msg
				  (loop for e in rest collect
				       (format nil " ~a={}" (emit-rs :code e)))))

		 (Utc--now)
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
[dependencies]
vulkano = \"*\"
vulkano-win = \"*\"
winit = \"*\"
chrono = \"*\"
"))
  
  (define-module
      `(main
	(do0

	 
	 (use
	  ;(vulkano instance (curly Instance InstanceExtensions))
	  ;(vulkano_win VkSurfaceBuild)
					;(winit event_loop (curly EventsLoop WindowBuilder))
	  (vulkano command_buffer CommandBuffer
		   )
	  (vulkano sync GpuFuture
		   )
	  (chrono Utc))
	 
	 (defun main ()
	   ,(logprint "start" `())
	   (let* ((event_loops (winit--event_loop--EventLoop--new))
		  )
	     (let (
		   (extensions (vulkano_win--required_extensions))
		   (instance (dot (vulkano--instance--Instance--new None &extensions None)
				  (expect (string "failed to create Vulkan instance"))))
		   (physical (dot (vulkano--instance--PhysicalDevice--enumerate &instance)
				  (next)
				  (expect (string "no device available"))))
		   (queue_family (dot physical
				      (queue_families)
				      (find (lambda (&q)
					      (return (q.supports_graphics))))
				      (expect (string "couldnt find graphical queue family"))))
		   ((values device "mut queues")
		    (dot (vulkano--device--Device--new physical
						       (ref (vulkano--device--Features--none))
						       (ref (vulkano--device--DeviceExtensions--none))
						       (dot (list (values queue_family 0.5))
							    (iter)
							    (cloned)))
			 (expect (string "failed to create device"))))
		   (queue (dot queues
			       (next)
			       (unwrap))))
	       (let ((data 12)
		     (buffer_src (dot (vulkano--buffer--CpuAccessibleBuffer--from_data
				   (device.clone)
				   (vulkano--buffer--BufferUsage--all)
				   false
				   data)
				      (expect (string "failed to create buffer"))))
		     (buffer_dst (dot (vulkano--buffer--CpuAccessibleBuffer--from_data
				   (device.clone)
				   (vulkano--buffer--BufferUsage--all)
				   false
				   data)
				      (expect (string "failed to create buffer")))))
		 (progn
		   (let* ((content (dot buffer_src
				      (write)
				      (unwrap))))
		     (setf *content 2)))
		 
		 (let ((command_buffer (dot
					(vulkano--command_buffer--AutoCommandBufferBuilder--new
					 (device.clone)
					 (queue.family))
					(unwrap)
					;; this command buffer should copy something
					(copy_buffer (buffer_src.clone)
						     (buffer_dst.clone))
					(unwrap)
					(build)
					(unwrap)))
		       (finished (dot command_buffer
				      (execute (queue.clone))
				      (unwrap)
				      )))
		   (do0 ,(logprint "copy .." `())
			(dot finished
			     (then_signal_fence_and_flush)
			     (unwrap)
			     (wait None)
			     (unwrap))
			,(logprint "after copy" `())))
		 
		 )
	       
	       #+nil (let ((surface (dot (winit-window--WindowBuilder--new)
				   (build_vk_surface
				    &event_loop
				    (instance.clone))
				   (unwrap))))
		,(logprint "queue" `()))))
	   ,(logprint "end" `())
	   ;(return (Ok "()"))
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
