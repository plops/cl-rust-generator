(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator")
  (ql:quickload "cl-cpp-generator2"))

(in-package :cl-cpp-generator2)

(let (;(name "trace.frag")
      (w 512s0)
      (h 512s0))
  (defparameter *source-dir* #P"examples/13_vulkano/code/src/")
  (write-source (asdf:system-relative-pathname 'cl-rust-generator
					       (merge-pathnames "trace.comp"
								*source-dir*))
               #+nil `(do0
                 "#version 450"

		 "layout(local_size_x=64,local_size_y=1,local_size_z=1) in;"
		 "layout(set=0,binding=0) buffer Data { uint data[]; } buf;"
		 
                 (defun main ()
		   (let ((idx gl_GlobalInvocationID.x))
		     (declare (type uint idx))
		     (setf (aref buf.data idx)
			   (* (aref buf.data idx) 12))))
		 
		 )
	       `(do0
                 "#version 450"

		 "layout(local_size_x=8,local_size_y=8,local_size_z=1) in;"
		 "layout(set=0,binding=0,rgba8) uniform writeonly image2D img;"


		 (defun main ()
		   (let ((norm_coordinates (/ (+ (vec2 0.5) gl_GlobalInvocationID.xy)
					      (vec2 (imageSize img))))
			 (c (- (* 2.0 (- norm_coordinates (vec2 0.5)))
			       (vec2 1.0 0.0)))
			 (z (vec2 0.0 0.0))
			 (i 0.0))
		     (declare (type vec2 norm_coordinates c z)
			      (type float i))
		     (for ((= i 0.0)
			   (< i 1.0)
			   (incf i .005))
			  (setf z (vec2 (+ (* z.x z.x)
					   (* -1 z.y z.y)
					   c.x
					   )
					(+ (* z.x z.y)
					   (*  z.x z.y)
					   c.y)))
			  (when (< 4.0 (length z))
			    break))
		     (let ((to_write (vec4 (vec3 i) 1.0)))
		       (declare (type vec4 to_write))
		       (imageStore img (ivec2 gl_GlobalInvocationID.xy)
				   to_write))))
		 
		 ))
  (write-source (asdf:system-relative-pathname 'cl-rust-generator
					       (merge-pathnames "trace.vert"
								*source-dir*))
		`(do0
                 "#version 450"

		 "layout(location=0) in vec2 position;"
		 
		 
                 (defun main ()
		   (setf gl_Position (vec4 position 0.0 1.0)))))
  (write-source (asdf:system-relative-pathname 'cl-rust-generator
					       (merge-pathnames "trace.frag"
								*source-dir*))
		`(do0
                 "#version 450"

		 "layout(location=0) out vec4 f_color;"
		 
		 
                 (defun main ()
		   (setf f_color (vec4 1.0 0.0 0.0 1.0))))))





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
vulkano = \"0.13\"
vulkano-win = \"*\"
winit = \"*\"
chrono = \"*\"
vulkano-shaders= \"0.13\"
image = \"*\"
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
	  (vulkano descriptor descriptor_set PersistentDescriptorSet)
	  (std sync Arc)
	  (chrono Utc))

	 (do0
	  "#[derive(Default,Copy,Clone)]"
	  (defstruct0 Vertex
	      (position "[f32;2]"))
	  (vulkano--impl_vertex! Vertex position))
	 
	 (defun main ()
	   ,(logprint "start" `())
	   (let* ((event_loops (winit--event_loop--EventLoop--new))
		  )
	     (let (
		   ; (extensions (vulkano_win--required_extensions))
		   (instance (dot (vulkano--instance--Instance--new None
					; &extensions
								    (&vulkano--instance--InstanceExtensions--none)
								    None)
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


	       #+nil
	       (progn
		 "// command buffer copy example"
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
			 (progn
			   (let ((src_content (dot buffer_src (read) (unwrap)))
				 (dst_content (dot buffer_dst (read) (unwrap))))
			     ,(logprint "after copy" `(*src_content *dst_content))))))
		 
		  ))
	       #+nil
	       (progn
		 "// compute shader"
		 (let ((data_iter "0 .. 65535")
		       (data_buffer (dot (vulkano--buffer--CpuAccessibleBuffer--from_iter
					  (device.clone)
					  (vulkano--buffer--BufferUsage--all)
					  ;false
					  data_iter)
					 (expect (string "failed to create buffer")))))

		   (space "mod cs"
			  (progn
			    (macroexpand
			     vulkano_shaders--shader!
			     :ty (string "compute")
			     :src
			     (string#
			      ,(read-file-into-string (asdf:system-relative-pathname 'cl-rust-generator
										     (merge-pathnames "trace.comp"
												      *source-dir*)))))))
		   (let ((shader (dot (cs--Shader--load (device.clone))
				      (expect (string "failed to create shader"))))
			 (compute_pipeline (std--sync--Arc--new
					    (dot (vulkano--pipeline--ComputePipeline--new
						  (device.clone)
						  (&shader.main_entry_point)
						  "&()")
						 (expect (string "failed to create compute pipeline")))))
			 
			 (set 
			   (std--sync--Arc--new
			    (dot
			  (vulkano--descriptor--descriptor_set--PersistentDescriptorSet--start
			   (compute_pipeline.clone)
			   0
			   )
			   (add_buffer (data_buffer.clone))
			   (unwrap)
			   (build)
			   (unwrap))))
			 (command_buffer (dot (vulkano--command_buffer--AutoCommandBufferBuilder--new
					       (device.clone)
					       (queue.family))
					      (unwrap)
					      (dispatch (list 1024 1 1)
							(compute_pipeline.clone)
							(set.clone)
							"()")
					      (unwrap)
					      (build)
					      (unwrap)))
			 (finished (dot command_buffer
					(execute (queue.clone))
					(unwrap))))
		     (dot finished
			  (then_signal_fence_and_flush)
			  (unwrap)
			  (wait None)
			  (unwrap))
		     (let ((content (dot data_buffer
					 (read)
					 (unwrap))))
		       ,(logprint "result" `((aref content 0)
					     (aref content 1)))))))
	       
	       

	       #+nil
	       (progn
		 "// store a blue image"
		 (let ((image (dot (vulkano--image--StorageImage--new
				    (device.clone)
				    (make-instance vulkano--image--Dimensions--Dim2d
						   :width 1024
						   :height 1024)
				    vulkano--format--Format--R8G8B8A8Unorm
				    (Some (queue.family)))
				   (unwrap)))
		       
		       (buf (dot (vulkano--buffer--CpuAccessibleBuffer--from_iter
				 (device.clone)
				 (vulkano--buffer--BufferUsage--all)
				 (dot "(0.. 1024*1024*4)"
				      (map (lambda (_) "0u8"))))
				 (expect (string "failed to create buffer"))))
		       (command_buffer (dot (vulkano--command_buffer--AutoCommandBufferBuilder--new
					     (device.clone)
					     (queue.family))
					    (unwrap)
					    (clear_color_image
					     (image.clone)
					     (vulkano--format--ClearValue--Float (list 0s0 0s0 1s0 1s0)))
					    (unwrap)
					    (copy_image_to_buffer (image.clone) (buf.clone))
					    (unwrap)
					    (build)
					    (unwrap)))
		       (finished (dot command_buffer
					(execute (queue.clone))
					(unwrap))))
		   (dot finished
			  (then_signal_fence_and_flush)
			  (unwrap)
			  (wait None)
			  (unwrap))
		   (progn
		     "// save image"
		     (let ((buffer_content (dot buf
						(read)
						(unwrap)))
			   (image (dot ("image::ImageBuffer::<image::Rgba<u8>,_>::from_raw"
					1024 1024
					"&buffer_content[..]")
				       (unwrap))))
		       (dot image
			    (save (string "image.png"))
			    (unwrap))))))

	       #+nil (progn
		 "// store a blue image"
		 (let ((image (dot (vulkano--image--StorageImage--new
				    (device.clone)
				    (make-instance vulkano--image--Dimensions--Dim2d
						   :width 1024
						   :height 1024)
				    vulkano--format--Format--R8G8B8A8Unorm
				    (Some (queue.family)))
				   (unwrap)))
		       
		       (buf (dot (vulkano--buffer--CpuAccessibleBuffer--from_iter
				 (device.clone)
				 (vulkano--buffer--BufferUsage--all)
				 (dot "(0.. 1024*1024*4)"
				      (map (lambda (_) "0u8"))))
				 (expect (string "failed to create buffer"))))
		       (command_buffer (dot (vulkano--command_buffer--AutoCommandBufferBuilder--new
					     (device.clone)
					     (queue.family))
					    (unwrap)
					    (clear_color_image
					     (image.clone)
					     (vulkano--format--ClearValue--Float (list 0s0 0s0 1s0 1s0)))
					    (unwrap)
					    (copy_image_to_buffer (image.clone) (buf.clone))
					    (unwrap)
					    (build)
					    (unwrap)))
		       (finished (dot command_buffer
					(execute (queue.clone))
					(unwrap))))
		   (dot finished
			  (then_signal_fence_and_flush)
			  (unwrap)
			  (wait None)
			  (unwrap))
		   (progn
		     "// save image"
		     (let ((buffer_content (dot buf
						(read)
						(unwrap)))
			   (image (dot ("image::ImageBuffer::<image::Rgba<u8>,_>::from_raw"
					1024 1024
					"&buffer_content[..]")
				       (unwrap))))
		       (dot image
			    (save (string "image.png"))
			    (unwrap))))))


	       #+nil
	       (progn
		 "// store mandelbrot image"
		 (let ((image (dot (vulkano--image--StorageImage--new
				    (device.clone)
				    (make-instance vulkano--image--Dimensions--Dim2d
						   :width 1024
						   :height 1024)
				    vulkano--format--Format--R8G8B8A8Unorm
				    (Some (queue.family)))
				   (unwrap)))
		       #+nil (data_iter "0 .. 65535")
			     )

		   (space "mod cs"
			  (progn
			    (macroexpand
			     vulkano_shaders--shader!
			     :ty (string "compute")
			     :src
			     (string#
			      ,(read-file-into-string (asdf:system-relative-pathname 'cl-rust-generator
										     (merge-pathnames "trace.comp"
												      *source-dir*)))))))
		   (let ((shader (dot (cs--Shader--load (device.clone))
				      (expect (string "failed to create shader"))))
			 (compute_pipeline (std--sync--Arc--new
					    (dot (vulkano--pipeline--ComputePipeline--new
						  (device.clone)
						  (&shader.main_entry_point)
						  "&()")
						 (expect (string "failed to create compute pipeline")))))
			 
			 (set 
			  (std--sync--Arc--new
			   (dot
			    (vulkano--descriptor--descriptor_set--PersistentDescriptorSet--start
			     (compute_pipeline.clone)
			     0
			     )
			    (add_image (image.clone))
			    (unwrap)
			    (build)
			    (unwrap))))
			  (buf (dot (vulkano--buffer--CpuAccessibleBuffer--from_iter
					  (device.clone)
					  (vulkano--buffer--BufferUsage--all)
					  (dot (slice 0 (* 1024 1024 4))
					       (map (lambda (_) "0u8"))))
					 (expect (string "failed to create buffer"))))
			 (command_buffer (dot (vulkano--command_buffer--AutoCommandBufferBuilder--new
					       (device.clone)
					       (queue.family))
					      (unwrap)
					      (dispatch (list (/ 1024 8)
							      (/ 1024 8) 1)
							(compute_pipeline.clone)
							(set.clone)
							"()")
					      (unwrap)
					      (copy_image_to_buffer (image.clone)
								    (buf.clone))
					      (unwrap)
					      (build)
					      (unwrap)))
			 (finished (dot command_buffer
					(execute (queue.clone))
					(unwrap))))
		     (dot finished
			  (then_signal_fence_and_flush)
			  (unwrap)
			  (wait None)
			  (unwrap))
		     (progn
		       "// save image"
		       (let ((buffer_content (dot buf
						(read)
						(unwrap)))
			   (image (dot ("image::ImageBuffer::<image::Rgba<u8>,_>::from_raw"
					1024 1024
					"&buffer_content[..]")
				       (unwrap))))
		       (dot image
			    (save (string "image.png"))
			    (unwrap)))))))

	       (progn
		 "// render example"
		 (let ((vertex_buffer
			(dot (vulkano--buffer--CpuAccessibleBuffer--from_iter
					  (device.clone)
					  (vulkano--buffer--BufferUsage--all)
					  (dot (space vec! (list ,@(loop for (x y) in `((-1 -1)
										    (0 1)
										    (1 -.5)) collect
								    `(make-instance Vertex
										    :position
										    (list ,(* .5 x)
											  ,(* .5 y))))))
					       (into_iter)))
			     (expect (string "failed to create buffer"))))
		       (render_pass (std--sync--Arc--new
				     (dot (vulkano--single_pass_renderpass!
				       (device.clone)
				       (space
					"attachments:"
					(progn
					  (space "color:"
						 (progn
						   "load: Clear,"
						   "store: Store,"
						   "format: vulkano::format::Format::R8G8B8A8Unorm,"
						   "samples: 1,"))))
				       (space "pass:"
					      (progn
						"color: [color],"
						"depth_stencil: {}")))
					  (unwrap)))
			 )
		       (image (dot (vulkano--image--StorageImage--new
				    (device.clone)
				    (make-instance vulkano--image--Dimensions--Dim2d
						   :width 1024
						   :height 1024)
				    vulkano--format--Format--R8G8B8A8Unorm
				    (Some (queue.family)))
				   (unwrap)))
		       (framebuffer (std--sync--Arc--new
				     (dot (vulkano--framebuffer--Framebuffer--start
					   (render_pass.clone))
					  (add (image.clone))
					  (unwrap)
					  (build)
					  (unwrap)))))
		   (do0
		    (space "mod vs"
			   (progn
			     (macroexpand
			      vulkano_shaders--shader!
			      :ty (string "vertex")
			      :src
			      (string#
			       ,(read-file-into-string (asdf:system-relative-pathname 'cl-rust-generator
										      (merge-pathnames "trace.vert"
												       *source-dir*)))))))

		    (space "mod fs"
			   (progn
			     (macroexpand
			      vulkano_shaders--shader!
			      :ty (string "fragment")
			      :src
			      (string#
			       ,(read-file-into-string (asdf:system-relative-pathname 'cl-rust-generator
										      (merge-pathnames "trace.frag"
												       *source-dir*)))))))
		    (let ((vs (dot (vs--Shader--load (device.clone))
				   (expect (string "failed to create shader"))))
			  (fs (dot (fs--Shader--load (device.clone))
				   (expect (string "failed to create shader"))))
			  (pipeline (std--sync--Arc--new
				     (dot
				      (vulkano--pipeline--GraphicsPipeline--start)
				      (vertex_input_single_buffer--<Vertex>)
				      (vertex_shader (vs.main_entry_point)
						     "()")
				      (viewports_dynamic_scissors_irrelevant 1)
				      (fragment_shader (fs.main_entry_point) "()")
				      (render_pass (dot (vulkano--framebuffer--Subpass--from
							 (render_pass.clone)
							 0)
							(unwrap)))
				      (build (device.clone))
				      (unwrap)))))))
		   
		   (dot
		    (vulkano--command_buffer--AutoCommandBufferBuilder--new
		     (device.clone)
		     (queue.family))
		    (unwrap)
		    (begin_render_pass (framebuffer.clone)
				       false
				       (space vec!
					      (list
					       (dot
						(list 0s0 0s0 1s0 1s0)
						(into)))))
		    (unwrap)
		    (end_render_pass)
		    (unwrap))))

	       

	       
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
