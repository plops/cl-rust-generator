(in-package :cl-rust-generator)

(defun parse-let (code emit &key (mutable-default nil))
  "let ({var | (var [init-form])}*) declaration* form*"
  (destructuring-bind (decls &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body mutable-default)
      (with-output-to-string (s)
	(format s "~a"
		(funcall emit
			`(do0
			  ,@(loop for decl in decls collect
				  (destructuring-bind (name &optional value) decl
				    (format nil "let ~a ~@[ = ~a~];"
					    (let ((l (variable-declaration :name name :env env :emit emit
									   :mutable-default mutable-default)))
					      (if (listp l)
						  (funcall #'emit l)
						  l))
					       (when value
						   (funcall emit value))))
			  ,@body)))))))

(defun parse-defun (code emit &key header-only)
  ;; defun function-name lambda-list [declaration*] form*
  (destructuring-bind (name lambda-list &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (let ((req-param lambda-list))
	(declare (ignorable req-param))
	(with-output-to-string (s)
	  (format s "fn ~a ~a~@[ -> ~a~]"
		  (funcall emit name)
		  (funcall emit `(paren
				  ,@(loop for rp in req-param collect
					 (let* ((p (remove-ampersand rp))
						(decl-m (lookup-type p :env env))
						(declaration (when decl-m (type-definition-declaration decl-m)))
						(m (when decl-m (type-definition-mutable decl-m)))
						(ref (when decl-m (type-definition-reference decl-m))))
					   (if decl-m
					       (with-output-to-string (s)
						 (format s "~a: " p)
						 (when ref
						   (format s "&" ))
						 (when m
						   (format s "mut "))
						 (format s "~a" declaration)
						 )
					       rp)))))
		  (let ((r (gethash 'return-values env)))
		    (if (< 1 (length r))
			(funcall emit `(paren ,@r))
			(when (car r)
			 (funcall emit (car r))))))
	  (unless header-only
	    (format s "~a" (funcall emit `(progn ,@body)))))))))

(defun parse-lambda (code emit)
  ;;  lambda lambda-list [declaration*] form*
  ;; no return value:
  ;;  |a, b| {body}
  ;; with (declaration (values float)):
  ;;  [] (int a, float b) -> float { body }
  ;; currently no support for captures (which would be placed into the first set of brackets)
  (destructuring-bind (lambda-list &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (let ((req-param lambda-list))
	(declare (ignorable req-param))
	(with-output-to-string (s)
	  (format s "|~a|~@[-> ~a ~]"
		  (funcall emit `(comma
				  ,@(loop for p in req-param collect
					 (format nil "~a ~a"
						 (let ((type (gethash p env)))
						   (if type
						       type
						       ""))
						 p
						 ))))
		  (let ((r (gethash 'return-values env)))
		    (if (< 1 (length r))
			(funcall emit `(paren ,@r))
			(car r))))
	  (format s "~a" (funcall emit `(progn ,@body))))))))