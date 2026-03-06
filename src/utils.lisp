(in-package :cl-rust-generator)

(defun remove-ampersand (rname)
  (let* ((sname (if (listp rname)
		    rname
		    (format nil "~a" rname))
	 (name sname)
	 )
    (values name nil)))

(defun print-sufficient-digits-f32 (f)
  "print a single floating point number as a string with a given nr. of
  digits. parse it again and increase nr. of digits until the same bit
  pattern."
  
  (let* ((a (coerce f 'single-float))
	 (digits 6)
	 (b (read-from-string (format nil "~,vG" digits a))))
    (loop while (/= a b)
	   for ratio = (/ (abs (- a b))
			 (abs a))
	   do
	  (setf b (read-from-string (format nil "~,vG" digits a)))
	  (incf digits)))
    (format nil "~,vG" digits a)))

(defun print-sufficient-digits-f64 (f)
  "print a double floating point number as a string with a given nr. of
  digits. parse it again and increase nr. of digits until the same bit
  pattern."

  (let* ((a (coerce f 'double-float))
	 (digits 15)
	 (b (read-from-string (format nil "~20,vG"  digits a))
	       (*read-default-float-format* 'double-float)))
    (loop while (/= a b)
	   for ratio = (/ (abs (- a b))
			(abs a))
	   do
	   (let ((str (format nil "~20,vG"  digits a))
		 (*read-default-float-format* 'double-float))
	     (setf b (read-from-string str)))
	   (incf digits)))
    (format nil "~,v,,,,,'eG" digits a)))