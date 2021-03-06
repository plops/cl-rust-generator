(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator")
  (ql:quickload "cl-cpp-generator2"))


(declaim (optimize (speed 0)
		   (safety 3)
		   (debug 3)))

(setf *features* (union *features* '(:debug
				     :runtime-shader)))
(setf *features* (set-difference *features* '(:debug
					       :runtime-shader
					      )))

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

  (defun compile-shader (shader-name shader-type code)
    (let ((shader-name "trace")
	  (shader-type "vert")
	  (filename (asdf:system-relative-pathname 'cl-rust-generator
						(merge-pathnames (format nil "~a.~a" shader-name shader-type)
								 *source-dir*)))
	  (filename-out (asdf:system-relative-pathname 'cl-rust-generator
						(merge-pathnames (format nil "~a_~a.spv" shader-name shader-type)
								 *source-dir*))))
      
   (write-source filename
		 code)

   (sb-ext:run-program "/usr/bin/glslangValidator"
		       (list (namestring filename)
			     "-V"
			     "-S" shader-type "-o"
			     (namestring filename-out) ))))
  (compile-shader "trace" "vert"
		  `(do0
                   "#version 450"
 
		   "layout(location=0) in vec3 position;"
		 
		 
                   (defun main ()
		     (setf gl_Position (vec4 position 1.0)))))

  (compile-shader "trace" "frag"
		  `(do0
                 "#version 450"

		 "layout(location=0) out vec4 f_color;"
		 
		 "//  https://www.youtube.com/watch?v=Cfe5UQ-1L9Q&t=1365s"
		 "layout(push_constant) uniform PushConstantData { uint timestamp; uint window_w; uint window_h; uint mouse_x; uint mouse_y;} pc;"


		 (defun map (pos)
		   (declare (type "in vec3" pos)
			    (values float))
		   (let ((d1 (- (length pos) .25 ;; size of sphere
			       ))
			 (d2 (+ pos.y .25)))
		     (declare (type float d1 d2))
		     (return (min d1 d2))))

		 (defun calcNormal (pos)
		   (declare (type "in vec3" pos)
			    (values vec3))
		   (let ((e (vec2 .0001 .0)))
		     (declare (type vec2 e))
		     (return (normalize (vec3 (- (map (+ pos e.xyy))
						 (map (- pos e.xyy)))
					      (- (map (+ pos e.yxy))
						 (map (- pos e.yxy)))
					      (- (map (+ pos e.yyx))
						 (map (- pos e.yyx))))))))

		;; 1:09

		 ,@(let ((far-away 20.0))
		    `(
		       (defun castRay (ro rd)
			 (declare (type "in vec3" ro)
				  (type vec3 rd)
				  (values float))
			 (let ((tau 0.0))
			   (declare (type float tau))
			  (for  ((= "int i" 0)
				 (< i 100)
				 (incf i))
				(let ((pos (+ ro (* tau rd)))
				      (h (map pos)))
				  (declare (type vec3 pos)
					   (type float h))
				  (when (< h .001)
				    break)
				  (incf tau h)
				  (when (< ,far-away tau)
				    break))))
			(when (< ,far-away tau)
			  (setf tau -1.0))
			(return tau))
		      (defun main ()
			(let ((iResolution (ivec2 pc.window_w pc.window_h))
			      (p0 (/ (- (* 2.0 gl_FragCoord.xy
					   )
					;(vec2 pc.window_w pc.window_h)
				       iResolution.xy
				       )
					; pc.window_h
				    iResolution.y
				    ))
			      (p (vec2 p0.x (* -1 p0.y)))
					;(f (smoothstep .25 .26 (length p)))
			      (an (/ (* 10.0 pc.mouse_x)
				     pc.window_w)
			       
					;(* .001 pc.timestamp)
			       )
			      (ro (vec3 (sin an)
					(* .1 (sin (* .01 pc.timestamp)))
					(cos an))) ;; camera
			      (ta (vec3 .0 .0 .0))
			      ;; direction
			      (ww (normalize (- ta ro)))
			      (uu (normalize (cross ww (vec3 0 1 0))))
			      (vv (normalize (cross uu ww)))
			      (rd (normalize ; (vec3 p -1.5)
				   (+ (* p.x uu)
				      (* p.y vv)
				      (* 1.5 ww))))
			      (col (- (vec3 .3 .5 .9)
				      (* .5 rd.y))) ;; sky
			      (tau (castRay ro rd)))
			  (declare (type vec2 p p0)
				   (type float an)
				   (type ivec2 iResolution)
				   (type float f tau)
				   (type vec3 col ro rd ta ww uu vv))

			  (setf col (mix col ;; sky gradient horizon
					 (vec3 .7 .75 .8)
					 (exp (* -10.0 rd.y))))
			  
			  (when (< 0 tau)
			    (let ((pos (+ ro (* tau rd)))
				  (nor (calcNormal pos))
				  (mate (vec3 .2 .2 .2)) ;; base color
				  (sun_dir (normalize (vec3 .8 .4 .2)))
				  (sun_dif (clamp ("dot" nor sun_dir)
						  0.0 1.0))
				  (sun_sha (step (castRay (+ pos (* .001 nor)) sun_dir)
						 0.0))
				  (sky_dif (clamp (+ .5 ("dot" nor (vec3 0.0 1.0 0.0)))
						  0.0 1.0))
				  (bou_dif (clamp (+ .5 ("dot" nor (vec3 0.0 -1.0 0.0)))
						  0.0 1.0)))
			      (declare (type vec3 pos nor sun_dir mate)
				       (type float sun_dif sky_dif sun_sha bou_dif))
			   
			      (setf col (* mate
					   (vec3 7.0 5. 3.)
					   sun_dif
					   sun_sha))
			      (incf col (* mate
					   (vec3 0.5 .8 .9)
					   sky_dif))
			      (incf col (* mate 
					   (vec3 0.7 .3 .2)
					   bou_dif))))
			  (setf col (pow col (vec3 .4545)))
			  (setf f_color (vec4 col .4))))))))

  (compile-shader "trace2" "frag"
		  `(do0
                 "#version 450"

		 "layout(location=0) out vec4 f_color;"
		 
		 
		 "layout(push_constant) uniform PushConstantData { uint timestamp; uint window_w; uint window_h; uint mouse_x; uint mouse_y;} pc;"


		 (defun main ()
		   (setf f_color (vec4 1.0 .2 .2 .7))))))





(in-package :cl-rust-generator)
;(trace emit-rs)
;; https://vulkano.rs/guide/example-operation
(progn
  (defparameter *source-dir* #P"examples/13_vulkano/code/src/")
  
  (defun logprint (msg &optional (rest nil))
    `(progn
       (println! (string ,(format nil "{} {}:{} ~a ~{~a~^ ~}"
				  msg
				  (loop for e in rest collect
				       (format nil " ~a={:?}" (emit-rs :code e)))))

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
vulkano-win = \"0.13\"
winit = \"0.19\" #0.19
chrono = \"*\"
vulkano-shaders= \"0.13\"
image = \"*\"
"))
  
  (define-module
      `(main
	(do0
	 
	 
	 (use
	  ;(vulkano instance (curly Instance InstanceExtensions))
	  (vulkano_win VkSurfaceBuild)
	  (winit EventsLoop)
	  (winit WindowBuilder)
	  (vulkano command_buffer CommandBuffer
		   )
	  (vulkano sync GpuFuture
		   )
	  (vulkano descriptor descriptor_set PersistentDescriptorSet)
	  
	  (std sync Arc)
	  
	  (chrono Utc)
	  (std fs File)
	  (std io prelude * )
	  )
	 

	 (do0
	  "#[derive(Default,Copy,Clone)]"
	  (defstruct0 Vertex
	      (position "[f32;3]"))
	  (vulkano--impl_vertex! Vertex position))
	 
	 (defun main ()
	   ,(logprint "start" `())
	   "// https://github.com/vulkano-rs/vulkano-examples/blob/020c546562203f948111508cc7cb67ffd258ab87/src/bin/debug.rs"
	   (let* ((event_loops (winit--EventsLoop--new))
		  )
	     
	     (let* ((extensions (vulkano_win--required_extensions)))
	       #+debug (setf extensions.ext_debug_report true)
	      (let (
		    (instance (dot (vulkano--instance--Instance--new None
								     &extensions
								     ;; (&vulkano--instance--InstanceExtensions--none)
								     #-debug None
								     #+debug (space vec! (list (string "VK_LAYER_LUNARG_standard_validation")))
								     )
				   (expect (string "failed to create Vulkan instance"))))
		    #+debug (_callback (dot (vulkano--instance--debug--DebugCallback--errors_and_warnings
				     &instance
				     (lambda (msg)
				       ,(logprint "debug:" `(msg.description))))
				    (ok))))
	       
		(let (
		    
		    
		      (physical (dot (vulkano--instance--PhysicalDevice--enumerate &instance)
				     (next)
				     (expect (string "no device available"))))
		      (queue_family (dot physical
					 (queue_families)
					 (find (lambda (&q)
						 (return (q.supports_graphics))))
					 (expect (string "couldnt find graphical queue family"))))
		      (device_ext (make-instance vulkano--device--DeviceExtensions
						 :khr_swapchain true
						 ".. vulkano::device::DeviceExtensions::none()"))
		      ((values device "mut queues")
		       (dot (vulkano--device--Device--new physical
							  (physical.supported_features)
							  &device_ext
					;(ref (vulkano--device--Features--none))
					;(ref (vulkano--device--DeviceExtensions--none))
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
			    )
			(do0
			 (let ((finished (dot command_buffer
					      (execute (queue.clone))
					      (unwrap))))
			   (dot finished
				(then_signal_fence_and_flush)
				(unwrap)
				(wait None)
				(unwrap)))
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
				  (unwrap))))))))

		  #+nil
		  (progn
		    "// render triangle to png"
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
			  (buf (dot (vulkano--buffer--CpuAccessibleBuffer--from_iter
				     (device.clone)
				     (vulkano--buffer--BufferUsage--all)
				     (dot "(0.. 1024*1024*4)"
					  (map (lambda (_) "0u8"))))
				    (expect (string "failed to create buffer"))))
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
					 (unwrap))))
			     (dynamic_state (make-instance vulkano--command_buffer--DynamicState
							   :viewports (Some
								       (space vec! (list (make-instance vulkano--pipeline--viewport--Viewport
													:origin (list 0s0 0s0)
													:dimensions (list 1024s0 1024s0)
													:depth_range (slice 0s0 1s0)))))
							   ".. vulkano::command_buffer::DynamicState::none()")))))
		   
		      (let ((command_buffer
			     (dot
			      (vulkano--command_buffer--AutoCommandBufferBuilder--primary_one_time_submit
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
			      (draw (pipeline.clone)
				    &dynamic_state
				    (vertex_buffer.clone)
				    "()"
				    "()")
			      (unwrap)
			      (end_render_pass)
			      (unwrap)
			      (copy_image_to_buffer (image.clone)
						    (buf.clone))
			      (unwrap)
			      (build)
			      (unwrap))))
			(do0
			 (let ((finished (dot command_buffer
					      (execute (queue.clone))
					      (unwrap))))
			   (dot finished
				(then_signal_fence_and_flush)
				(unwrap)
				(wait None)
				(unwrap)))
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
				  (unwrap))))))))

	       
		  (progn
		    "// render to window"
		    "//  https://github.com/vulkano-rs/vulkano-examples/blob/master/src/bin/triangle.rs "
		    "// vulkano 0.13 example:"
		    "// https://github.com/vulkano-rs/vulkano-examples/blob/1cf9c37073a79a3a0cee60e83c8db8d967218e3e/src/bin/triangle.rs"
		    (let ((surface (dot (winit--WindowBuilder--new)
					(build_vk_surface
					 &event_loops
					 (instance.clone))
					(unwrap)))
			  (window (surface.window))
			  (caps (dot (surface.capabilities physical)
				     (expect (string "failed to get surface capabilities"))))
			
			  )
		    
		      (let (
			    (dimensions ;(caps.current_extent.unwrap_or (list 1280 1024))
			     ((lambda ()
				(let ((window_size (window.get_inner_size)))
				  (case window_size
				    ((Some dimensions)
				     (let ((ds (dot dimensions
						    (to_physical (window.get_hidpi_factor))
						    (into))))
				       (declare (type "(u32, u32)" ds))
				       ,(logprint "window size" `(ds))
				       (return (list ds.0 ds.1))))
				    (t ,(logprint "window size fail" `())
				       (panic! (string "win size")))))))
			      )
			    (alpha (dot caps
					supported_composite_alpha
					(iter)
					(next)
					(unwrap)))
			    (format (dot caps
					 (aref supported_formats 0)
					 0))
			    ((values "mut swapchain" images)
			     (dot (vulkano--swapchain--Swapchain--new
				   (device.clone)
				   (surface.clone)
				   caps.min_image_count
				   format
				   dimensions
				   1
				   caps.supported_usage_flags
				   &queue
				   vulkano--swapchain--SurfaceTransform--Identity
				   alpha
				   vulkano--swapchain--PresentMode--Immediate ; Fifo
				   true
				   None)
				  (expect (string "failed to create swapchain"))))
			    #+nil ((values image_num acquire_future)
				   (dot (vulkano--swapchain--acquire_next_image (swapchain.clone)
										None)
					(unwrap))))
		     
			,(logprint "swapchain" `(dimensions alpha format caps.min_image_count caps.supported_usage_flags
							    ))
		      

			(do0
			 "// render triangle to swapchain"
			 (let ((vertex_buffer
				(dot (vulkano--buffer--CpuAccessibleBuffer--from_iter
				      (device.clone)
				      (vulkano--buffer--BufferUsage--all)
				      (dot (space vec! (list ,@(loop for (x y) in `((-1 -1)
										    (1 -1)
										    (-1 1)
										    (1 1)) collect
								    `(make-instance Vertex
										    :position
										    (list ,(* 1.0 x)
											  ,(* -1.0 y)
											  0.0)))))
					   (into_iter)))
				     (expect (string "failed to create buffer"))))
			       (vertex_buffer2
				(dot (vulkano--buffer--CpuAccessibleBuffer--from_iter
				      (device.clone)
				      (vulkano--buffer--BufferUsage--all)
				      (dot (space vec!						  
						  (list  ,@(let ((phi-n 13)
								(theta-n 5))
							     (loop for phi-i below phi-n append
								  (loop for theta-i below theta-n collect
								       (let* ((r .25)
									      (phi (* 2 pi (/ phi-i (* 1.0 phi-n))))
									      (theta (* pi (/ theta-i (* 1.0 theta-n))))
									      (x (coerce (* r (sin theta) (cos phi)) 'single-float)
										)
									      
									      (y (coerce
										  (* r (sin theta)
										     (sin phi))
										  'single-float)
										
										)
									      (z (coerce (* r (cos theta))
											 'single-float)
										))
									 `(make-instance Vertex
											 :position
											 (list ,x
											       ,y
											       ,z))))))))
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
							       "format: swapchain.format(),"
							       "samples: 1,"))))
						   (space "pass:"
							  (progn
							    "color: [color],"
							    "depth_stencil: {}")))
						  (unwrap)))))
			   
			   (do0
			    (do0
			     ,@(loop for (mod type fn) in `((vs vertex "trace.vert")
							    (fs fragment "trace.frag")
							    (fs2 fragment "trace2.frag"))
				  collect
				    (let ((full-fn (asdf:system-relative-pathname
						    'cl-rust-generator
						    (merge-pathnames fn
								     *source-dir*)))
					  (input (format nil "ShaderInput_~a" mod))
					  (output (format nil "ShaderOutput_~a" mod))
					  (layout (format nil "ShaderLayout_~a" mod))
					  (input-iter (format nil "ShaderInputIter_~a" mod))
					  (output-iter (format nil "ShaderOutputIter_~a" mod)))
				      `(do0
					(do0
					 #+runtime-shader
					 (let ((,mod
						(progn
						  (let* ((f (dot (std--fs--File--open (string ,full-fn))
								 (expect (string ,(format nil "can't find ~a" full-fn))))))
						    (let* ((v (space vec! (list))))
						      (dot f
							   (read_to_end "&mut v")
							   (unwrap))
						      (space unsafe
							     (dot
							      (progn
								(vulkano--pipeline--shader--ShaderModule--new
								 (device.clone)
								 &v
								 ))
							      (unwrap))))))))
					   (do0
					    "#[derive(Debug,Copy,Clone,PartialEq,Eq,Hash)]"
					    ,(format nil "struct ~a;" input)
					    (space ,(format
						     nil
						     "unsafe impl vulkano::pipeline::shader::ShaderInterfaceDef for ~a"
						     input)
						   (progn
						     (setf "type Iter" ,input-iter)
						     (defun elements (&self)
						       (declare (values ,input-iter))
						       (return (,input-iter 0))))))
					   (do0
					    "#[derive(Debug,Copy,Clone)]"
					    ,(format nil "struct ~a(u16);" input-iter)
					    (space ,(format
						     nil
						     "impl vulkano::pipeline::shader::Iterator for ~a"
						     input-iter)
						   (progn
						     (setf "type Item" "vulkano::pipeline::shader::ShaderInterfaceDefEntry")

						     (do0
						      "#[inline]"
						      (defun next ("&mut self")
							(declare (values "Option<Self::Item>"))
							"// - on entry per location, non-overlapping"
							"// - each element must not be longer than 128 bytes"

							,@(loop for e in `(("1..2" vulkano--format--Format--R32G32B32Sfloat color)
									   ("0..1" vulkano--format--Format--R32G32B32Sfloat color))
							       and i from 0
							     collect
							       (destructuring-bind (location format name) e
								 `(when (== self.0 ,i)
								   (incf self.0)
								   (return
								     (Some
								      (make-instance
								       "vulkano::pipeline::shader::ShaderInterfaceDefEntry"
								       :location ,location
								       :format ,format
								       :name (Some (std--borrow--Cow--Borrowed (string ,name)))))))))
							(return None)))
						     (do0
						      "#[inline]"
						      (defun size_hint ("&mut self")
							(declare (values "(usize,Option<usize>)"))
							"// - exact number of entries left in iterator"
							(let ((len (coerce (- 2 self.0) usize)))
							  (return (values len (Some len)))))))))
					   (do0
					    (space ,(format
						     nil
						     "impl vulkano::pipeline::shader::ExactSizeIterator for ~a"
						     input-iter)
						   (progn
						     )))
					   (do0
					    "#[derive(Debug,Copy,Clone,PartialEq,Eq,Hash)]"
					    ,(format nil "struct ~a;" output)
					    (space ,(format
						     nil
						     "unsafe impl vulkano::pipeline::shader::ShaderInterfaceDef for ~a"
						     input)
						   (progn
						     (setf "type Iter" ,input-iter)
						     (defun elements (&self)
						       (declare (values ,input-iter))
						       (return (,input-iter 0))))))
					   (do0
					    "#[derive(Debug,Copy,Clone)]"
					    ,(format nil "struct ~a(u16);" output-iter)
					    (space ,(format
						     nil
						     "impl vulkano::pipeline::shader::Iterator for ~a"
						     output-iter)
						   (progn
						     (setf "type Item" "vulkano::pipeline::shader::ShaderInterfaceDefEntry")

						     (do0
						      "#[inline]"
						      (defun next ("&mut self")
							(declare (values "Option<Self::Item>"))
							,@(loop for e in `(
									   ("0..1" vulkano--format--Format--R32G32B32Sfloat v_color))
							       and i from 0
							     collect
							       (destructuring-bind (location format name) e
								 `(when (== self.0 ,i)
								   (incf self.0)
								   (return
								     (Some
								      (make-instance
								       "vulkano::pipeline::shader::ShaderInterfaceDefEntry"
								       :location ,location
								       :format ,format
								       :name (Some (std--borrow--Cow--Borrowed (string ,name)))))))))
							(return None)))
						     (do0
						      "#[inline]"
						      (defun size_hint ("&mut self")
							(declare (values "(usize,Option<usize>)"))
							"// - exact number of entries left in iterator"
							(let ((len (coerce (- 2 self.0) usize)))
							  (return (values len (Some len)))))))))
					   (do0
					    (space ,(format
						     nil
						     "impl vulkano::pipeline::shader::ExactSizeIterator for ~a"
						     output-iter)
						   (progn
						     )))
					   (do0
					    "#[derive(Debug,Copy,Clone)]"
					    ,(format nil "struct ~a(ShaderStages);" layout)
					    (space ,(format
						     nil
						     "unsafe impl vulkano::pipeline::shader::PipelineLayoutDesc for ~a"
						     layout)
						   (progn
						     (defun num_sets ("&self")
						       (declare (values usize))
						       (return 0))
						     (defun num_bindings_in_set ("&self" "_set: usize")
						       (declare (values Option<usize>))
						       (return None))
						     (defun descriptor ("&self" "_set: usize" "_binding: usize")
						       (declare (values Option<DescriptorDesc>))
						       (return None))
						     (defun num_push_constants_ranges ("&self")
						       (declare (values usize))
						       (return 0))
						     (defun push_constants_range ("&self" "_num: usize")
						       (declare (values Option<PipelineLayoutDescPcRange>))
						       (return None)))))
					   
					   )
					 )
					#-runtime-shader
					(space
					 ,(format nil "mod ~a" mod)
					 (progn
					   (macroexpand
					    vulkano_shaders--shader!
					    :ty (string ,type)
					    :src
					    (string#
					     ,(read-file-into-string
					       (asdf:system-relative-pathname
						'cl-rust-generator
						(merge-pathnames fn
								 *source-dir*)))))))))))
			    (let (#-runtime-shader (vs (dot (vs--Shader--load (device.clone))
							    (expect (string "failed to create shader"))))
						   #-runtime-shader (fs (dot (fs--Shader--load (device.clone))
									     (expect (string "failed to create shader"))))
						   #-runtime-shader (fs2 (dot (fs--Shader--load (device.clone))
									      (expect (string "failed to create shader"))))
						   (pipeline (std--sync--Arc--new
							      (dot
							       (vulkano--pipeline--GraphicsPipeline--start)
							       (vertex_input_single_buffer--<Vertex>)
							       
							       (vertex_shader
								#-runtime-shader  (vs.main_entry_point)
								#+runtime-shader
								(space
								 unsafe
								 (progn
								   (dot vs
									(graphics_entry_point
									 (std--ffi--CStr--from_bytes_with_nul_unchecked
									  (string-b "main\\0"))))))
								"()")
							       (triangle_strip)
							       (viewports_dynamic_scissors_irrelevant 1)
							       #-runtime-shader  (fragment_shader (fs.main_entry_point) "()")
							       (blend_alpha_blending)
							       (render_pass (dot (vulkano--framebuffer--Subpass--from
										  (render_pass.clone)
										  0)
										 (unwrap)))
							       (build (device.clone))
							       (unwrap))))
						   (pipeline2 (std--sync--Arc--new
							       (dot
								(vulkano--pipeline--GraphicsPipeline--start)
								(vertex_input_single_buffer--<Vertex>)
								#-runtime-shader  (vertex_shader (vs.main_entry_point)
												 "()")
								(line_strip)
								(viewports_dynamic_scissors_irrelevant 1)
								#-runtime-shader (fragment_shader (fs2.main_entry_point) "()")
								(blend_alpha_blending)
								(render_pass (dot (vulkano--framebuffer--Subpass--from
										   (render_pass.clone)
										   0)
										  (unwrap)))
								(build (device.clone))
								(unwrap))))
						   
						   )))
			   #+runtime-shader (let ((ep (fs2.main_entry_point)))
					      ,(logprint "fs2" `(ep.input ep.output)))
			   ;; https://github.com/vulkano-rs/vulkano-examples/blob/1cf9c37073a79a3a0cee60e83c8db8d967218e3e/src/bin/push-constants.rs
			   (let* ((push_constants (make-instance fs--ty--PushConstantData
								 :timestamp 0
								 :window_w (aref dimensions 0)
								 :window_h (aref dimensions 1)
								 :mouse_x 0
								 :mouse_y 0)))
			 
			     (let* ((dynamic_state (make-instance vulkano--command_buffer--DynamicState
					; :line_width None
					; :viewports None
					; :scissors None
					; :compare_mask None
					; :write_mask None
					; :reference None
								  :viewports
								  (Some
								   (space vec! (list (make-instance vulkano--pipeline--viewport--Viewport
												    :origin (list 0s0 0s0)
												    :dimensions (list (coerce (aref dimensions 0) f32)
														      (coerce (aref dimensions 1) f32))
					;(list 1024s0 1024s0)
												    :depth_range (slice 0s0 1s0)))))
								  ".. vulkano::command_buffer::DynamicState::none()"
								  ))
				    (framebuffers
				     (dot images
					  (iter)
					  (map (lambda (image)
						 (return (coerce
							  (std--sync--Arc--new
							   (dot (vulkano--framebuffer--Framebuffer--start
								 (render_pass.clone))
								(add (image.clone))
								(unwrap)
								(build)
								(unwrap)))
							  "Arc<dyn vulkano::framebuffer::FramebufferAbstract + Send +Sync>"))))
					  (collect--<Vec<_>>))))
			       (let* ((recreate_swapchain false
					)
				      (previous_frame_end (Some (coerce
								 (Box--new (vulkano--sync--now (device.clone)
											       ))
								 "Box<dyn vulkano::sync::GpuFuture>"))))
				 )))))
		     
			(event_loops.run_forever
			 (lambda (event)
			   (do0
			    (dot previous_frame_end
				 (as_mut)
				 (unwrap)
				 (cleanup_finished))
			    (incf push_constants.timestamp)
			    (when recreate_swapchain
			    
			      (let ((dimensions ;(caps.current_extent.unwrap_or (list 1280 1024))
				     ((lambda ()
					(let ((window_size (window.get_inner_size)))
					  (case window_size
					    ((Some dimensions)
					     (let ((ds (dot dimensions
							    (to_physical (window.get_hidpi_factor))
							    (into))))
					       (declare (type "(u32, u32)" ds))
					       ,(logprint "window size" `(ds))
					       (return (list ds.0 ds.1))))
					    (t ,(logprint "window size fail" `())
					       (panic! (string "win size")))))))
				      )
				    ((values new_swapchain new_images)
				     (case (swapchain.recreate_with_dimension dimensions)
				       ((Ok r) r)
				       ((Err vulkano--swapchain--SwapchainCreationError--UnsupportedDimensions)
					(return winit--ControlFlow--Continue))
				       ((Err e) (panic!  (string "{:?}") e)))))
				,(logprint "swapchain needs recreation" `(dimensions))
				(setf swapchain new_swapchain)
				(setf dynamic_state
				      (make-instance vulkano--command_buffer--DynamicState
						     :viewports
						     (Some
						      (space vec! (list (make-instance vulkano--pipeline--viewport--Viewport
										       :origin (list 0s0 0s0)
										       :dimensions (list (coerce (aref dimensions 0) f32)
													 (coerce (aref dimensions 1) f32))
										       :depth_range (slice 0s0 1s0)))))
						     ".. vulkano::command_buffer::DynamicState::none()"
						     ))
				(setf framebuffers
				      (dot new_images
					   (iter)
					   (map (lambda (image)
						  (return (coerce
							   (std--sync--Arc--new
							    (dot (vulkano--framebuffer--Framebuffer--start
								  (render_pass.clone))
								 (add (image.clone))
								 (unwrap)
								 (build)
								 (unwrap)))
							   "Arc<dyn vulkano::framebuffer::FramebufferAbstract + Send +Sync>"))))
					   (collect--<Vec<_>>)))
				(setf recreate_swapchain false))
			      )
			    ,(logprint "next image" `())
			    (let (((values image_num
					   acquire_future)
				   (case (vulkano--swapchain--acquire_next_image
					  (swapchain.clone)
					  None)
				     ((Ok r) r)
				     ((Err vulkano--swapchain--AcquireError--OutOfDate)
				      (do0
				       ,(logprint "acquire error" `())
				       (setf recreate_swapchain true)
				       (return winit--ControlFlow--Continue)))
				     ((Err e)
				      ,(logprint "failed to acquire next image" `(e))
				      (panic! (string "fail"))))))
			    
					;,(logprint "cmd" `())
			      (let ((command_buffer
				     (dot
				      (vulkano--command_buffer--AutoCommandBufferBuilder--primary_simultaneous_use ;one_time_submit
				       (device.clone)
				       (queue.family))
				      (unwrap)
				      (begin_render_pass (dot (aref framebuffers image_num)
							      (clone)) 
							 false
							 (space vec!
								(list
								 (dot
								  (list 0s0 0s0 1s0 1s0)
								  (into)))))
				      (unwrap)
				      (draw (pipeline.clone)
					    &dynamic_state
					    (vertex_buffer.clone)
					    "()"
					    (push_constants.clone))
				      (unwrap)
				      (draw (pipeline2.clone)
					    &dynamic_state
					    (vertex_buffer2.clone)
					    "()"
					    (push_constants.clone))
				      (unwrap)
				      (end_render_pass)
				      (unwrap)
				      (build)
				      (unwrap)))
				    (future (dot previous_frame_end
						 (take)
						 (unwrap)
						 (join acquire_future)
						 (then_execute (queue.clone)
							       command_buffer)
						 (unwrap)
						 (then_swapchain_present (queue.clone)
									 (swapchain.clone)
									 image_num)
						 (then_signal_fence_and_flush))))
				(case future 
				  ((Ok future)
				   (setf previous_frame_end (Some (coerce
								   (Box--new future)
								   "Box<_>"))))
				  ((Err vulkano--sync--FlushError--OutOfDate)
				   ,(logprint "out of date" `())
				   (setf recreate_swapchain true)
				   (setf previous_frame_end (Some (coerce
								   (Box--new (vulkano--sync--now
									      (device.clone)))
								   "Box<_>"))))
				  ((Err e)
				   ,(logprint "failed to flush future" `(e))
				   (setf previous_frame_end (Some (coerce
								   (Box--new (vulkano--sync--now
									      (device.clone)))
								   "Box<_>"))))))


			      ))
		       
			   (case event
			     ((make-instance winit--Event--WindowEvent
					     :event winit--WindowEvent--CloseRequested
					     "..")
			      (return winit--ControlFlow--Break))
			     ((make-instance winit--Event--WindowEvent
					     :event (winit--WindowEvent--Resized _) 
					     "..")
			      (setf recreate_swapchain true)
			      (return winit--ControlFlow--Continue))
			     ((make-instance winit--Event--WindowEvent
					     :event (make-instance winit--WindowEvent--CursorMoved
								   device_id
								   position
								   "..") 
					     "..")
					;,(logprint "cursor-moved" `(position))
			      (setf push_constants.mouse_x (coerce position.x u32)
				    push_constants.mouse_y (coerce position.y u32))
			      (return winit--ControlFlow--Continue))
			     (_
			      (return winit--ControlFlow--Continue))
			 
			     ))))
		      ,(logprint "queue" `())))))))
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


 
