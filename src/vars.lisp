(in-package :cl-rust-generator)

(defparameter *file-hashes* (make-hash-table))

(defparameter *keywords-without-semicolon* `(defun if for include
					     dotimes while case do0 do0-no-final-semicolon progn case
					     space defstruct0 impl use mod
					     extern unsafe macroexpand space let let*))