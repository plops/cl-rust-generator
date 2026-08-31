(defpackage :cl-rust-generator
  (:use :cl
	:alexandria)
  (:export
   #:write-source
   #:emit-rs
   #:*rustfmt-program*
   #:*rustfmt-arguments*))
