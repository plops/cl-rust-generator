(in-package :cl-rust-generator)

(declaim (optimize (speed 0)
		   (safety 3)
		   (debug 3)))

(setf (readtable-case *readtable*) :invert)

(defun write-source (name code &optional (dir (user-homedir-pathname))
				 ignore-hash)
  (let* ((fn (merge-pathnames (format nil "~a" name)
			      dir))
	(code-str (emit-rs :code code))
	(fn-hash (sxhash fn))
	 (code-hash (sxhash code-str)))
    (multiple-value-bind (old-code-hash exists) (gethash fn-hash *file-hashes*)
      (when (or (not exists) ignore-hash (/= code-hash old-code-hash)
		(not (probe-file fn)))
	;; store the sxhash of the c source in the hash table
	;; *file-hashes* with the key formed by the sxhash of the full
	;; pathname
	(setf (gethash fn-hash *file-hashes*) code-hash)
	(with-open-file (s fn
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
	  (write-sequence code-str s))

	(sb-ext:run-program "/home/martin/.cargo/bin/rustfmt"
			    (list (namestring fn)))))))

(defun emit-rs (&key code (str nil)  (level 0) (hook-defun nil))
  "evaluate s-expressions in code, emit a string. if hook-defun is not nil, hook-defun will be called with every function definition. this functionality is intended to collect function declarations."
  (flet ((emit (code &optional (dl 0))
	     "change the indentation level. this is used in do"
	     (emit-rs :code code :level (+ dl level) :hook-defun hook-defun)))
      ;(format t "~a~%" code)
      (if code
	  (if (listp code)
	      (progn
		(case (car code)
		  
		  (comma
		   ;; comma {args}*
		   (let ((args (cdr code)))
		     (format nil "~{~a~^, ~}" (mapcar #'emit args))))
		  (semicolon
		   ;; semicolon {args}*
		   (let ((args (cdr code)))
		     (format nil "~{~a~^; ~}" (mapcar #'emit args))))
		  (space
		   ;; space {args}*
		   (let ((args (cdr code)))
		     (format nil "~{~a~^ ~}" (mapcar #'emit args))))
		  (paren
		   ;; paren {args}*
		   (let ((args (cdr code)))
		     (format nil "(~{~a~^, ~})" (mapcar #'emit args))))
		  (values
		   ;; values {args}* -- same as paren
		   (emit `(paren ,@(cdr code))))
		  (bracket
		   ;; bracket {args}*
		   (let ((args (cdr code)))
		     (format nil "[~{~a~^, ~}]" (mapcar #'emit args))))
		  (list
		   (emit `(bracket ,@(cdr code))))
		  (curly
		   ;; curly {args}*
		   (let ((args (cdr code)))
		     (format nil "{~{~a~^, ~}}" (mapcar #'emit args))))
		  (make-instance
		   ;;https://doc.rust-lang.org/book/ch05-01-defining-structs.html
		   (let ((args (cdr code)))
		     (destructuring-bind (name &rest params) args
		       (emit `(space ,(emit name)
				     (curly
				      ,@(let ((i 0))
					  (loop while (< i (length params))  collect
					       (if (keywordp (elt params i))
						   (prog1
						     (format nil "~a: ~a"
							     (elt params i)
							     (emit (elt params (+ i 1))))
						     (incf i 2))
						   (prog1
						    (format nil "~a"
							    (emit (elt params i)))
						     (incf i)))))))))))
		  (macroexpand
		   (let ((args (cdr code)))
		     (destructuring-bind (name &rest params) args
		       (emit `(space ,(emit name)
				     (curly
				      ,@(let ((i 0))
					  (loop while (< i (length params))  collect
					       (if (keywordp (elt params i))
						   (prog1
						     (format nil "~a: ~a"
							     (elt params i)
							     (emit (elt params (+ i 1))))
						     (incf i 2))
						   (prog1
						    (format nil "~a"
							    (emit (elt params i)))
						    (incf i)))))))))))
		  (new
		   ;; new arg
		   (let ((arg (cadr code)))
		     (format nil "new ~a" (emit arg))))
		  (indent
		   ;; indent form
		   (format nil "~{~a~}~a"
			   ;; print indentation characters
			   (loop for i below level collect "    ")
			   (emit (cadr code))))
		  (impl (destructuring-bind (_impl name &rest body) code
			  (emit `(space impl
					,name
					(progn
					  ,@body)))))
		  (? (let ((args (cdr code)))
		       (format nil "~a?" (emit (car args)))))
		  (use
		   ;; use {(a b c)}*
		   ;; (use ((a b c) (q r))) => use a::b::c; use q::r
		   (let ((args (cdr code)))
		     (with-output-to-string (s)
			   (loop for e in args collect
				(format s "use ~{~a~^::~};~%" (mapcar #'emit e))))))
		  (mod
		   ;; mod {module}*
		   (let ((args (cdr code)))
			 (with-output-to-string (s)
			   (loop for e in args collect
			    (format s "mod ~a;~%" (emit e))))))
		  (do0 (with-output-to-string (s)
			 ;; do0 {form}*
			 ;; write each form into a newline, keep current indentation level
			 (format s "~{~&~a~}"
				 (mapcar
				  #'(lambda (x)
				      (let ((b (emit `(indent ,x) 0)))
					(format nil "~a~a"
						b
						;; don't add semicolon if there is already one
						;; or if x contains a string
						;; or if x is an s-expression with a c thing that doesn't end with semicolon
						(if (or (eq #\; (aref b (- (length b) 1)))
							(and (typep x 'string))
							
							(and (listp x)
							     (member (car x)
								     *keywords-without-semicolon*
								     )))
						    "" 
						    ";"))))
				  (cdr code)))))
		  (do0-no-final-semicolon
		   (with-output-to-string (s)
		     ;; do0 {form}*
		     ;; write each form into a newline, keep current indentation level
		     (let ((count 0)
			   (args (cdr code)))
		      (format s "~{~&~a~}"
			      (mapcar
			       #'(lambda (x)
				   (prog1
				    (let ((b (emit `(indent ,x) 0)))
				      (format nil "~a~a"
					      b
					      ;; don't add semicolon if there is already one
					      ;; don't add semicolon after last statement
					      ;; or if x contains a string
					      ;; or if x is an s-expression with a c thing that doesn't end with semicolon
					      (if (or (eq #\; (aref b (- (length b) 1)))
						      (and (typep x 'string))
						      (and (listp x)
							   (member (car x)
								   *keywords-without-semicolon*))
						      (= count (- (length args) 1)))
						  "" 
						  (format nil "; // ~a" count))))
				     (incf count)))
			       args)))))
		  (include (let ((args (cdr code)))
			     ;; include {name}*
			     ;; (include <stdio.h>)   => #include <stdio.h>
			     ;; (include interface.h) => #include "interface.h"
			     (with-output-to-string (s)
			       (loop for e in args do
				  ;; emit string if first character is not <
				    (format s "~&#include ~a"
					    (emit (if (eq #\< (aref (format nil "~a" e) 0))
						      e
						      `(string ,e))))))))
		  (progn (with-output-to-string (s)
			   ;; progn {form}*
			   ;; like do but surrounds forms with braces.
			   ;; don't place semicolon after last statement (implicit return in rust)
			   (format s "{~{~&~a~}~&}" (mapcar #'(lambda (x) (emit `(indent (do0-no-final-semicolon ,x)) 1)) (cdr code)))))
		  (block (with-output-to-string (s)
			   ;; progn {form}*
			   ;; like do but surrounds forms with braces.
			   ;; like progn but semicolon after last statement
			   ;; equivalent to (progn (bla) "()")
			   ;; https://doc.rust-lang.org/reference/expressions/block-expr.html
			   (format s "{~{~&~a~}~&}" (mapcar #'(lambda (x) (emit `(indent (do0 ,x)) 1)) (cdr code)))))
		  (do (with-output-to-string (s)
			;; do {form}*
			;; print each form on a new line with one more indentation.
			(format s "~{~&~a~}" (mapcar #'(lambda (x) (emit `(indent (do0 ,x)) 1)) (cdr code)))))
		  (defclass
			;; defclass class-name ({superclass-name}*) ({slot-specifier}*) [[class-option]]
			;; class TA : public Faculty, public Student { ... } 
			(destructuring-bind (name parents &rest body) (cdr code)
			  (format nil "class ~a ~@[: ~a~] ~a"
				  (emit name)
				  (when parents
				    (emit `(comma ,parents)))
				  (emit `(progn ,@body))
				  )))
		  (protected (format nil "protected ~a" (emit (cadr code))))
		  (public (format nil "public ~a" (emit (cadr code))))
		  (defun
		      (prog1
			  (parse-defun code #'emit)
			(when hook-defun
			  (funcall hook-defun (parse-defun code #'emit :header-only t)))))
		  (return (format nil "return ~a" (emit (car (cdr code)))))
		  (throw (format nil "throw ~a" (emit (car (cdr code)))))
		  (cast (destructuring-bind (type value) (cdr code)
			  (format nil "(~a) ~a"
				  (emit type)
				  (emit value))))
		  (slice (let ((args (cdr code)))
			       (format nil "(~{~a~^..~})" (mapcar #'emit args))))
		  (let (parse-let code #'emit :mutable-default nil))
		  (let* (parse-let code #'emit :mutable-default t))
		  (setf 
		   (let ((args (cdr code)))
		     ;; "setf {pair}*"
		     (format nil "~a"
			     (emit
			      `(do0 
				,@(loop for i below (length args) by 2 collect
				       (let ((a (elt args i))
					     (b (elt args (+ 1 i))))
					 `(= ,a ,b))))))))
		  (not (format nil "!(~a)" (emit (car (cdr code)))))
		  (deref (format nil "*(~a)" (emit (car (cdr code)))))
		  (ref (format nil "&(~a)" (emit (car (cdr code)))))
		  (+ (let ((args (cdr code)))
		       ;; + {summands}*
		       (format nil "(~{~a~^+~})" (mapcar #'emit args))))
		  (- (let ((args (cdr code)))
		       (if (eq 1 (length args))
			   (format nil "(-~a)" (emit (car args))) ;; py
			   (format nil "(~{~a~^-~})" (mapcar #'emit args)))))
		  (* (let ((args (cdr code)))
		       (format nil "(~{(~a)~^*~})" (mapcar #'emit args))))
		  (^ (let ((args (cdr code)))
		       (format nil "(~{(~a)~^^~})" (mapcar #'emit args))))
		  (& (let ((args (cdr code)))
		       (format nil "(~{(~a)~^&~})" (mapcar #'emit args))))
		  (/ (let ((args (cdr code)))
		       (if (eq 1 (length args))
			   (format nil "(1.0/(~a))" (emit (car args))) ;; py
			   (format nil "(~{(~a)~^/~})" (mapcar #'emit args)))))
		  
		  (logior (let ((args (cdr code))) ;; py
			    (format nil "(~{(~a)~^ | ~})" (mapcar #'emit args))))
		  (logand (let ((args (cdr code))) ;; py
			    (format nil "(~{(~a)~^ & ~})" (mapcar #'emit args))))
		  (logxor (let ((args (cdr code))) ;; py
			    (format nil "(~{(~a)~^ ^ ~})" (mapcar #'emit args))))
		  (or (let ((args (cdr code)))
			(format nil "(~{(~a)~^||~})" (mapcar #'emit args))))
		  (and (let ((args (cdr code)))
			 (format nil "(~{(~a)~^&&~})" (mapcar #'emit args))))
		  (= (destructuring-bind (a b) (cdr code)
		       ;; = pair
		       (format nil "~a=~a" (emit a) (emit b))))
		  (/= (destructuring-bind (a b) (cdr code)
			(format nil "~a/=(~a)" (emit a) (emit b))))
		  (*= (destructuring-bind (a b) (cdr code)
			(format nil "~a*=(~a)" (emit a) (emit b))))
		  (^= (destructuring-bind (a b) (cdr code)
			(format nil "(~a)^=(~a)" (emit a) (emit b))))
		  (<= (destructuring-bind (a b) (cdr code)
			(format nil "(~a)<=(~a)" (emit a) (emit b))))
		  (!= (destructuring-bind (a b) (cdr code)
			(format nil "(~a)!=(~a)" (emit a) (emit b))))
		  (== (destructuring-bind (a b) (cdr code)
			(format nil "(~a)==(~a)" (emit a) (emit b))))
		  (< (destructuring-bind (a b) (cdr code)
		       (format nil "~a<~a" (emit a) (emit b))))
		  (% (destructuring-bind (a b) (cdr code)
		       (format nil "~a%~a" (emit a) (emit b))))
		  (<< (destructuring-bind (a &rest rest) (cdr code)
			(format nil "(~a)~{<<(~a)~}" (emit a) (mapcar #'emit rest))))
		  (>> (destructuring-bind (a &rest rest) (cdr code)
			(format nil "(~a)~{>>(~a)~}" (emit a) (mapcar #'emit rest))))
		  (incf (destructuring-bind (a &optional (b 1)) (cdr code) ;; py
			  (format nil "~a += ~a " (emit a) (emit b))
			  ))
		  (decf (destructuring-bind (a &optional (b 1)) (cdr code)
			  (format nil "~a -= ~a" (emit a) (emit b))
			  ))
		  (byte  (format nil "b'~a'" (cadr code)))
		  (string (format nil "\"~a\"" (cadr code)))
		  (string-b (format nil "b\"~a\"" (cadr code))) 
		  (string# (let* ((str (cadr code))
				   (n-of-hash (count #\# str))
				   (list-of-hash (loop for i upto n-of-hash collect "#")))
			      (format nil "r~{~a~}\"~a\"~{~a~}"
				      list-of-hash
				      str
				      list-of-hash)))
		  (char (format nil "'~a'" (cadr code)))
		  (hex (destructuring-bind (number) (cdr code)
			 (format nil "0x~x" number)))
		  (if (destructuring-bind (condition true-statement &optional false-statement) (cdr code)
			(with-output-to-string (s)
			  (format s "if  ~a  ~a"
				  (emit condition)
				  (emit `(progn ,true-statement)))
			  (when false-statement
			    (format s " else ~a"
				    (emit `(progn ,false-statement)))))))
		  (when (destructuring-bind (condition &rest forms) (cdr code)
			  (emit `(if ,condition
				     (do0
				      ,@forms)))))
		  (unless (destructuring-bind (condition &rest forms) (cdr code)
			    (emit `(if (not ,condition)
				       (do0
					,@forms)))))
		  (coerce (let ((args (cdr code)))
			(destructuring-bind (name type) args
			    (format nil "(~a as ~a)" (emit name) (emit type)))))
		  (dot (let ((args (cdr code)))
			 (format nil "~{~a~^.~}" (mapcar #'emit args))))
		  
		  (aref (destructuring-bind (name &rest indices) (cdr code)
			  ;(format t "aref: ~a ~a~%" (emit name) (mapcar #'emit indices))
			  (format nil "~a[~{~a~^,~}]" (emit name) (mapcar #'emit indices))))
		  
		  (-> (let ((args (cdr code)))
			(format nil "~{~a~^->~}" (mapcar #'emit args))))
		  
		  (lambda (parse-lambda code #'emit))
		  (unsafe (let ((args (cdr code)))
			    (emit `(space "unsafe"
					  (progn
					    ,@args)))))
		  (extern (let ((args (cdr code)))
			    (emit `(space "extern"
					  (progn
					    ,@args)))))
		  (case
		      ;; case keyform {normal-clause}* [otherwise-clause]
		      ;; normal-clause::= (keys form*) 
		      ;; otherwise-clause::= (t form*) 
		      
		      (destructuring-bind (keyform &rest clauses)
			  (cdr code)
			(format
			 nil "match ~a ~a"
			 (emit keyform)
			 (emit
			  `(progn
			     ,@(loop for c in clauses collect
				    (destructuring-bind (key &rest forms) c
				      (let ((code (if (listp forms)
						      `(progn
							 ,@(mapcar #'emit
								   forms))
						      (emit forms))))
				       (if (eq key t)
					   (format nil "_ => ~a,"
						   (emit code))
					   (format nil "~a => ~a,"
						   (emit key)
						   (emit code)))))))))))
		  (dotimes (destructuring-bind ((i n &optional (step 1)) &rest body) (cdr code)
			     (emit `(for (,(format nil "int ~a = 0" (emit i))
					   (< ,(emit i) ,(emit n))
					   (incf ,(emit i) ,(emit step)))
					 ,@body))))
		  (loop (let ((args (cdr code)))
			  (format nil "loop ~a"
				  (emit `(progn ,@args)))))
		  (for (destructuring-bind ((item collection) &rest body) (cdr code)
			     (format nil "for  ~a in ~a ~a"
				     (emit item)
				     (emit collection)
				     (emit `(progn ,@body)))))
		  (while  ;; while condition {forms}*
		      (destructuring-bind (condition &rest body) (cdr code)
			(format nil "while (~a) ~a"
				(emit condition)
				(emit `(progn ,@body)))))
		  (deftype
		      ;; deftype name lambda-list {form}*
		      ;; only the first form of the body is used, lambda list is ignored
		      (destructuring-bind (name lambda-list &rest body) (cdr code)
			(declare (ignore lambda-list))
			(format nil "typedef ~a ~a" (emit (car body)) name)))
		  (struct (format nil "struct ~a" (emit (car (cdr code)))))
		  (defstruct0
		   ;; defstruct without init-form
		   ;; defstruct name {slot-description}*
		   ;; slot-description::= slot-name | (slot-name [slot-type])
		   
		   ;; a slot-name without type can be used to create a
		   ;; composed type with a struct embedding
		   
		   ;; i think i should use this pattern that works in C
		   ;; and in C++. Typedef isn't strictly necessary in
		   ;; C++, execept if you overload the struct name with
		   ;; a function:
		   
		   ;; struct 
		   ;; { 
		   ;;    char name[50]; 
		   ;;    char street[100]; 
		   ;;    char city[50]; 
		   ;;    char state[20]; 
		   ;;    int pin; 
		   ;; } Address;
		   ;; typedef struct Address Address;
		   ;; int Address(int b){ ...}
		   
		   ;; https://stackoverflow.com/questions/1675351/typedef-struct-vs-struct-definitions
		   (destructuring-bind (name &rest slot-descriptions) (cdr code)
		     (format nil "~a"
			     (emit `(do0
				     ,(format nil "struct ~a ~a"
					      name
					      (emit
					       `(progn
						  ,@(loop for desc in slot-descriptions collect
							 (destructuring-bind (slot-name &optional type value) desc
							   (declare (ignorable value))
							   (format nil "~a: ~a,"  (emit slot-name)(emit type))))))
					      )
				     )))))
		  (handler-case
		      ;; handler-case expression [[{error-clause}*]]
		    ;;; error-clause::= (typespec ([var]) declaration* form*) ;; note: declarations are currently unsupported
		      ;; error-clause::= (typespec ([var]) form*)
		      ;; if typespec is t, catch any kind of exception

		      ;; (handler-case (progn forma formb)
		      ;;   (typespec1 (var1) form1)
		      ;;   (typespec2 (var2) form2))

		      ;; a clause such as:
		      ;; (typespec (var) (declare (ignore var)) form)
		      ;; can be written as (typespec () form)
		      
		      ;; try {
		      ;;   // code here
		      ;; }
		      ;; catch (int param) { cout << "int exception"; }
		      ;; catch (char param) { cout << "char exception"; }
		      ;; catch (...) { cout << "default exception"; }
		      
		      (destructuring-bind (expr &rest clauses) (cdr code)
			(with-output-to-string (s)
			  (format s "try ~a"
				  (if (eq 'progn (car expr))
				      (emit expr)
				      (emit `(progn ,expr))))
			  (loop for clause in clauses do
			       (destructuring-bind (typespec (var) &rest forms) clause
				 (format s "catch (~a) ~a"
					 (if (and (eq 't typespec)
						  (null var))
					     (format nil "...")
					     (format nil "~a ~a" typespec var))
					 (emit `(progn ,@forms))))))))
		  (t (destructuring-bind (name &rest args) code

		       (if (listp name)
			   ;; lambda call and similar complex constructs
			   (format nil "(~a)~a"
				   (emit name)
				   (emit `(paren ,@args))
				   )
			   ;; function call
			   
			   (progn
			     (format nil "~a~a" (emit name)
				     (emit `(paren ,@args)))))))))
	      (cond
		((symbolp code)
		 ;; print variable or function name
		 ;; convert - to : 
		 (substitute #\: #\- (format nil "~a" code))
		 )
		((stringp code) 
		 ;; print variable or function name
		 ;; don't transform characters
		 (format nil "~a" code)
		 )
		((numberp code) ;; print constants
		 (cond ((integerp code)
			(if (< code 0)
			    (format str "(~a)" code)
			    (format str "~a" code)))
		       ((floatp code)
			(typecase code
			  (single-float (let ((v (print-sufficient-digits-f32 code)))
					  (if (< code 0)
					      (format str "(~a)" v)
					      (format str "~a" v))))
			  (double-float (let ((v (print-sufficient-digits-f64 code)))
					  (if (< code 0)
					      (format str "(~a)" v)
					      (format str "~a" v)))))))))
	  ""))))