;;;; gen00.lisp --- Generate src/main.rs via cl-rust-generator

(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)

(defparameter *source-dir* #P"/home/kiel/stage/cl-rust-generator/examples/26_onnx/source2/src/")
(defparameter *code-file* (merge-pathnames #P"main.rs" *source-dir*))


(let ((*omit-redundant-parens* t))

;;; ----------------------------------------------------------------------
;;; Configuration & Constants
;;; ----------------------------------------------------------------------

  (defparameter *model-name* "yolo26n")
  (defparameter *model-size* 640)
  (defparameter *model-path* "../yolo26n.onnx")

;;; ----------------------------------------------------------------------
;;; Lisp Helper Functions to Reduce Code Repetition
;;; ----------------------------------------------------------------------

  (defun emit-pixel-planes (index &rest planes)
    "Generate normalized channel assignments: plane[index] = ch as f32 / 255.0"
    (loop for (plane ch) in planes
          collect `(= (aref ,plane ,index)
                      (/ (coerce ,ch f32) 255.0))))

  (defun emit-draw-detection (det &key (min-conf 0.1) (thickness 2.0) (color 'RED))
    "Filter detection by confidence and draw bounding box: [x1, y1, x2, y2, conf, cls]"
    `(when (>= (aref ,det 4) ,min-conf)
       (draw_rectangle_lines (aref ,det 0)
                             (aref ,det 1)
                             (- (aref ,det 2) (aref ,det 0))
                             (- (aref ,det 3) (aref ,det 1))
                             ,thickness
                             ,color)))



  (write-source
   *code-file*
   `(do0
     ;; 1. Imports
     (use (macroquad prelude "*")
          (ort (curly inputs session--Session value--TensorRef))
          (x11rb connection Connection)
          (x11rb protocol xproto (curly self ImageFormat)))

     ;; 2. Constants
     (stmt (space const "SIZE: usize" "=" ,*model-size*))
     (stmt (space const "PLANE: usize" "=" (* SIZE SIZE)))

     "// Compiles the ONNX weights directly into the binary's .rodata section"
     (stmt (space const "MODEL_BYTES: &[u8]" "=" (include_bytes! (string ,*model-path*))))

     ;; 3. Window configuration
     (defun window_conf ()
       (declare (values Conf))
       (return
         (space Conf
                (curly
                 (space "window_title:" (dot (string ,*model-name*) (into)))
                 ,@(loop for dim in '("window_width:" "window_height:")
                         collect `(space ,dim (coerce SIZE i32)))
                 "..Default::default()"))))

     ;; 4. Main application entry point
     (attr "macroquad::main(window_conf)"
	   (defun-async main ()
             (let (((paren conn screen) (dot (x11rb--connect None) (unwrap)))
		   (root (dot (aref (dot conn (setup) roots) screen) root))
		   "// Load the model directly from embedded memory"
		   (session (dot (Session--builder)
				 (unwrap)
				 (commit_from_memory MODEL_BYTES)
				 (unwrap)))
		   (input_name (dot (aref (dot session (inputs)) 0) (name) (to_string)))
		   (img (Image--gen_image_color (coerce SIZE u16) (coerce SIZE u16) BLACK))
		   (tex (Texture2D--from_image (ref img)))
		   (input (vec! (semicolon "0.0f32" (* 3 PLANE))))))
             (declare (mutable session img input))

             (while (not (is_key_down KeyCode--Escape))
		    (let ((reply (dot (xproto--get_image (ref conn)
							 ImageFormat--Z_PIXMAP
							 root
							 0
							 0
							 (coerce SIZE u16)
							 (coerce SIZE u16)
							 u32--MAX)
				      (unwrap)
				      (reply)
				      (unwrap)))
			  ((paren r_plane rest) (dot input (split_at_mut PLANE)))
			  ((paren g_plane b_plane) (dot rest (split_at_mut PLANE))))

		      ;; Copy BGRA window frame to RGB texture and planar model input buffer
		      (for ((paren i px) (dot reply data (chunks_exact 4) (take PLANE) (enumerate)))
			   (let (((paren b g r) (paren (aref px 0) (aref px 1) (aref px 2)))
				 (off (* i 4)))
			     (dot (aref (dot img bytes) (range off (+ off 4)))
				  (copy_from_slice (ref (list r g b 255))))
			     ,@(emit-pixel-planes 'i '(r_plane r) '(g_plane g) '(b_plane b))))

		      ;; Run inference on embedded ONNX session
		      (let ((outputs (dot session
					  (run (aref inputs!
						     (space (dot input_name (as_str))
							    "=>"
							    (dot (TensorRef--from_array_view
								  (paren (list 1 3 SIZE SIZE)
									 (ref (aref input (range-full)))))
								 (unwrap)))))
					  (unwrap)))
			    ((paren _ dets) (dot (aref outputs 0)
						 ("try_extract_tensor::<f32>")
						 (unwrap))))

			(dot tex (update (ref img)))
			(clear_background BLACK)
			(draw_texture (ref tex) 0.0 0.0 WHITE)

			(for (det (dot dets (chunks_exact 6)))
			     ,(emit-draw-detection 'det))

			(await (next_frame))))))))))
