;; tests.lisp
(load "~/quicklisp/setup.lisp")
(ql:quickload "cl-rust-generator")
(in-package :cl-rust-generator)

(defparameter *test-ast-1*
  '(defun calculate_max (a b)
     (declare (type i32 a b) (values i32))
     (if (> a b)
         (return a)
         (return b))))

(defparameter *test-ast-2*
  '(let ((x (+ 1 (* 2 3)))
         (y (cast f32 x)))
     (declare (type i32 x) (type f32 y))
     (return y)))

(defun run-baseline-tests ()
  (format t "--- TEST 1 ---~%~a~%" (emit-rs :code *test-ast-1*))
  (format t "--- TEST 2 ---~%~a~%" (emit-rs :code *test-ast-2*)))

(run-baseline-tests)
