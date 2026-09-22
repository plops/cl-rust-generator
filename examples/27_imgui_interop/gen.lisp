(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)

(progn
  (defparameter *source-dir*
    #P"examples/27_imgui_interop/rust_logic/src/")
  (defparameter *code-file*
    (asdf:system-relative-pathname 'cl-rust-generator
      (merge-pathnames #P"lib.rs"
        *source-dir*)))
  )

(defun app-struct-code ()
  "Shared AppState shape. Mirrors src/main.cpp struct AppState."
  `(attr "repr(C)"
     (space "pub"
       (defstruct0 AppState
         ("pub click_count" i32)
         ("pub slider_value" f32)
         ("pub checkbox_status" bool))))
  )

(defun init-fn-code ()
  "init_app_state returns default state by value."
  `(attr "unsafe(no_mangle)"
     (space "pub extern \"C\""
       (defun init_app_state ()
         (declare (values AppState))
         (make-instance AppState
           :click_count 0
           :slider_value 0.5s0
           :checkbox_status false))))
  )

(defun process-fn-code ()
  "process_logic advances slider when checked. Null-safe. Unsafe fn
because the caller lends us a raw pointer we cannot verify."
  `(do0
     "/// Advances the slider while the box is checked, wraps past 1.0."
     "///"
     "/// # Safety"
     "///"
     "/// `state` must point to a live mutable `AppState`. Null is a no-op,"
     "/// any other dangling pointer is undefined behavior."
     (attr "unsafe(no_mangle)"
       (space "pub unsafe extern \"C\""
       (defun process_logic ("state: *mut AppState")
         (if (dot state (is_null))
           (return))
         (space unsafe
           (let ((s (ref-mut (deref state))))
             (if (dot s checkbox_status)
               (progn
                 (incf (dot s slider_value) 0.001s0)
                 (if (< 1.0s0 (dot s slider_value))
                   (setf (dot s slider_value) 0.0s0))))))))))
  )

(defun test-init-code ()
  "Defaults are 0, 0.5 and false."
  `(attr "test"
     (defun t_init_defaults ()
       (let ((s (init_app_state)))
         (stmt (assert! (== (dot s click_count) 0)))
         (stmt (assert! (== (dot s slider_value) 0.5s0)))
         (stmt (assert! (not (dot s checkbox_status)))))))
  )

(defun test-null-code ()
  "Null state pointer is a silent no-op."
  `(attr "test"
     (defun t_null_noop ()
       (space unsafe
         (progn
           (process_logic "std::ptr::null_mut()")))))
  )

(defun test-inactive-code ()
  "Unchecked box leaves the slider alone."
  `(attr "test"
     (defun t_inactive_keeps_slider ()
       (let* ((s (init_app_state)))
         (space unsafe
           (progn
             (process_logic (ref-mut s))))
         (stmt (assert! (== (dot s slider_value) 0.5s0))))))
  )

(defun test-active-code ()
  "Checked box advances the slider by one step."
  `(attr "test"
     (defun t_active_steps_slider ()
       (let* ((s (init_app_state)))
         (setf (dot s checkbox_status) true)
         (space unsafe
           (progn
             (process_logic (ref-mut s))))
         (stmt (assert! (< 0.5s0 (dot s slider_value))))
         (stmt (assert! (< (dot s slider_value) 0.6s0))))))
  )

(defun test-wrap-code ()
  "Slider wraps to zero past 1.0 while checked."
  `(attr "test"
     (defun t_wrap_at_one ()
       (let* ((s (init_app_state)))
         (setf (dot s checkbox_status) true)
         (setf (dot s slider_value) 1.0s0)
         (space unsafe
           (progn
             (process_logic (ref-mut s))))
         (stmt (assert! (== (dot s slider_value) 0.0s0))))))
  )

(defun tests-code ()
  "cfg(test) module. Calls the fns above."
  `(attr "cfg(test)"
     (space "mod tests"
       (block
         (use (super *))
         ,(test-init-code)
         ,(test-null-code)
         ,(test-inactive-code)
         ,(test-active-code)
         ,(test-wrap-code))))
  )

(let ((*omit-redundant-parens* t))
  (write-source *code-file*
    `(do0
       ,(app-struct-code)
       ,(init-fn-code)
       ,(process-fn-code)
       ,(tests-code)))
  )
