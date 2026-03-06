(in-package :cl-rust-generator)

(defstruct type-definition
  (declaration)
  (mutable)
  (reference))

(defun type-definition-supersede-declaration (rname
					      hashtable decl
					      &optional (mutable t))
  "mutable by default"
  (multiple-value-bind (name ref) (remove-ampersand rname)
    (multiple-value-bind (el exists) (gethash name hashtable)
     (if exists
	 (let ((m (type-definition-mutable el)))
	   (remhash name hashtable)
	   (setf (gethash name hashtable)
		 (make-type-definition :declaration  decl
				       :mutable m
				       :reference ref)))
	 (progn
	   (setf (gethash name hashtable)
		 (make-type-definition :declaration decl :mutable mutable
				       :reference ref)))))))

(defun type-definition-supersede-mutable (rname hashtable mutable)
  (multiple-value-bind (name ref) (remove-ampersand rname)
    (multiple-value-bind (el exists) (gethash name hashtable)
     (if exists
	 (let ((decl (type-definition-declaration el)))
	   (remhash name hashtable)
	   (setf (gethash name hashtable)
		 (make-type-definition :declaration decl
				       :mutable mutable
				       :reference ref)))
	 (progn
	   (setf (gethash name hashtable)
		 (make-type-definition :declaration nil
				       :mutable mutable
				       :reference ref)))))))

(defun consume-declare (body)
  "go through the body until no declare anymore"
  (let ((env (make-hash-table :test 'equal))
	(new-body nil))
    (loop while body
	  for e = (pop body)
	  do
	  (if (and (listp e)
		   (eq (first e) 'declare))
	      (loop for declaration in (rest e) do
		   (cond
		     ((eq (first declaration) 'type)
		      (destructuring-bind (symb type &rest vars) declaration
			(declare (ignorable symb))
			(loop for var in vars do
			     (type-definition-supersede-declaration
			      var env type))))
		     ((eq (first declaration) 'immutable)
		      (destructuring-bind (symb &rest vars) declaration
			(declare (ignorable symb))
			(loop for var in vars do
			     (type-definition-supersede-mutable
			      var env nil))))
		     ((eq (first declaration) 'mutable)
		      (destructuring-bind (symb &rest vars) declaration
			(declare (ignorable symb))
			(loop for var in vars do
			     (type-definition-supersede-mutable
			      var env t))))
		     ((eq (first declaration) 'values)
		      #+nil (progn
			(format t "values not supported yet~%")
			(quit))
		      )
		     (t
		      (warn "consume-declare: dont know how to handle ~a" declaration))))
	      (push e new-body)))
    (values (reverse new-body) env)))

(defun lookup-type (rname &key env)
  "get the type of a variable from an environment"
  (let* ((name (remove-ampersand rname))
	 (el (gethash name env)))
    el))

(defun variable-declaration (&key name env emit mutable-default)
  (let* ((name (remove-ampersand name))
	 (decl-m (lookup-type name :env env))
	 (type (when decl-m
		   (type-definition-declaration decl-m)))
	 (m (if decl-m
		(type-definition-mutable decl-m)
		mutable-default)))
    (with-output-to-string (s)
      (format s "~a: " name)
      (when (type-definition-reference decl-m)
	(format s "&" ))
      (when m
	(format s "mut "))
      (if (listp type)
	  (if (null type)
	      (format s "~a" (funcall emit name))
	      (progn
	       ;; array
	       (destructuring-bind (array_ element-type &rest dims) type
		 (declare (ignorable array_))
		 (format s "[~{~a~^,~}]" (mapcar #'emit dims))
		 (format s ";~a" element-type))))
	  (progn
	    (if type
		(format s "~a" type)
		(format s "auto")))))))