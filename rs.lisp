;; When rs.lisp is loaded through cl-rust-generator.asd, package.lisp has
;; already created the package and ASDF has already pulled in alexandria.  The
;; block below only fires when somebody loads this file directly from a REPL.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :cl-rust-generator)
    (uiop:symbol-call :ql :quickload "alexandria")
    (eval (read-from-string
	   "(defpackage :cl-rust-generator
              (:use :cl :alexandria)
              (:export #:write-source #:emit-rs #:*rustfmt-program* #:*rustfmt-arguments*))"))))


(declaim (optimize (speed 0)
		   (safety 3)
		   (debug 3)))
					;(setf *features* (union *features* '(:generic-c)))
;(setf *features* (set-difference *features* '(:generic-c)))
(in-package :cl-rust-generator)

(setf (readtable-case *readtable*) :invert)

(defparameter *file-hashes* (make-hash-table :test #'equal))

(defparameter *rustfmt-program* "rustfmt"
  "Name (or absolute path) of the program used to format the generated Rust
code.  It is looked up in PATH.  Set to NIL to disable formatting.")

(defparameter *rustfmt-arguments* nil
  "Extra command line arguments handed to *RUSTFMT-PROGRAM*, e.g.
'(\"--edition\" \"2018\").  rustfmt defaults to edition 2015.")

(defun run-rustfmt (filename)
  "Format FILENAME in place with *RUSTFMT-PROGRAM*.  Missing or failing rustfmt
is reported as a warning; it never aborts code generation.  Returns the exit
code, or NIL when rustfmt could not be started."
  (when *rustfmt-program*
    (handler-case
	(multiple-value-bind (out err code)
	    (uiop:run-program (append (list *rustfmt-program*)
				      *rustfmt-arguments*
				      (list (namestring filename)))
			      :output nil
			      :error-output :string
			      :ignore-error-status t)
	  (declare (ignorable out))
	  (unless (eql 0 code)
	    (warn "~a failed on ~a (exit ~a): ~a" *rustfmt-program* filename code err))
	  code)
      (error (e)
	(warn "could not run ~a: ~a" *rustfmt-program* e)
	nil))))

(defun write-source (name code &optional (dir (user-homedir-pathname))
				 ignore-hash)
  "Emit CODE as Rust and write it to NAME (relative to DIR).  The file is only
touched when the generated text actually changed, then it is run through
rustfmt.  Returns the pathname that was written, or NIL when nothing changed."
  (let* ((fn (merge-pathnames (format nil "~a" name)
			      dir))
	 (code-str (emit-rs :code code))
	 (fn-key (namestring fn))
	 (code-hash (sxhash code-str)))
    (multiple-value-bind (old-code-hash exists) (gethash fn-key *file-hashes*)
      (when (or (not exists) ignore-hash (/= code-hash old-code-hash)
		(not (probe-file fn)))
	;; store the sxhash of the rust source in the hash table
	;; *file-hashes* with the full pathname as key
	(setf (gethash fn-key *file-hashes*) code-hash)
	(ensure-directories-exist fn)
	(with-open-file (s fn
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
	  (write-sequence code-str s))
	(run-rustfmt fn)
	fn))))

;; http://clhs.lisp.se/Body/s_declar.htm
;; http://clhs.lisp.se/Body/d_type.htm

;; go through the body until no declare anymore



  ;; (declare (type int a b) (type float c)
  ;; (declare (values int &optional))
  ;; (declare (values int float &optional))

  ;; FIXME doesnt handle documentation strings

(defstruct type-definition
  (declaration)
  (mutable)
  (reference))



(defun type-definition-supersede-declaration (rname
					      hashtable decl
					      &optional (mutable t))
  "mutable by default"
  (multiple-value-bind (name ref) (remove-ampersand rname)
					;let*
    #+nil
   ((sname (format nil "~a" rname))
    (name (remove #\& sname))
    (ref  (< 0 (count #\& sname))))
   #+nil (format t "~a~%" `(:rname rname
				   :env
				   ,(loop for key being the hash-keys using (hash-value v) of hashtable collect `(,key ,v))))
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
				       :reference ref))
					;(format t "~a doesnt exist ~a~%" name `(:decl ,decl :entry ,(gethash name hashtable)))
	   )))))

(defun type-definition-supersede-mutable (rname hashtable mutable)
  (multiple-value-bind (name ref) (remove-ampersand rname)
   #+nil ((sname (format nil "~a" rname))
	(name (remove #\& sname))
	(ref  (< 0 (count #\& sname))))
    #+nil (format t "~a~%" `(:rname rname
			    :env
			    ,(loop for key being the hash-keys using (hash-value v) of hashtable collect `(,key ,v))))
    (multiple-value-bind (el exists) (gethash name hashtable)
     (if exists
	 (let ((decl (type-definition-declaration el)))
	   (remhash name hashtable)
	   (setf (gethash name hashtable)
		 (make-type-definition :declaration decl
				       :mutable mutable
				       :reference ref)))
	 (progn
					;(format t "~a doesnt exist~%" name)
	   (setf (gethash name hashtable)
		 (make-type-definition :declaration nil
				       :mutable mutable
				       :reference ref)))))))



(defun consume-declare (body &optional (mutable-default nil))
  "take a list of instructions from body, parse type declarations,
return the body without them and a hash table with an environment. the
entry return-values contains a list of return values"
  (let ((env (make-hash-table :test #'equalp))
	(looking-p t)
	(new-body nil))
    (loop for e in body do
	 (if looking-p
	     (if (listp e)
		 (if (eq (car e) 'declare)
		     (loop for declaration in (cdr e) do
			  ;(format t "declaration: ~a~%" declaration)
			  (cond 
			    ((eq (first declaration) 'type)
			     (destructuring-bind (symb type &rest vars) declaration
			       (declare (ignorable symb))
			       (loop for var in vars do
				    (type-definition-supersede-declaration
				     var env type mutable-default)
				    #+nil (type-definition-supersede-mutable
				     var env mutable-default))))
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
			     (destructuring-bind (symb &rest types-opt) declaration
			       (declare (ignorable symb))
			       (let ((types nil))
				 ;; only collect types until occurrance of &optional
				 (loop for type in types-opt do
				      (unless (eq #\& (aref (format nil "~a" type) 0))
					(push type types)))
				 (setf (gethash 'return-values env) (reverse types)))))
			    (t (error "unknown declaration: ~a" declaration))))
		     (progn
		       (push e new-body)
		       (setf looking-p nil)))
		 (progn
		   (setf looking-p nil)
		   (push e new-body)))
	     (push e new-body)))
    (values (reverse new-body) env)))

(defun remove-ampersand (rname)
  "Split a leading & off a variable or parameter name.  Returns (values NAME
REFERENCE-P) where NAME is a string without any & and REFERENCE-P tells whether
the original name asked for a Rust reference.  Lists (array type specifiers) are
passed through unchanged.

  (remove-ampersand '&x) => \"x\", T
  (remove-ampersand 'x)  => \"x\", NIL"
  (if (listp rname)
      (values rname nil)
      (let* ((sname (format nil "~a" rname))
	     (ref (< 0 (count #\& sname)))
	     (name (remove #\& sname)))
	(values name ref))))

(defun lookup-type (rname &key env)
  "get the type of a variable from an environment"
  (let* ((name (remove-ampersand rname))
	   (el (gethash name env)))
    #+nil (format t "search for ~a in ~a gives ~a ~%" name (loop for key being the hash-keys using (hash-value v) of env
							collect `(,key ,v))
	    el)
    el))


(defun rust-array-type (element-type dims emit)
  "Render a (possibly multi dimensional) Rust array type.
  (rust-array-type 'i32 '(4) emit)   => \"[i32; 4]\"
  (rust-array-type 'i32 '(2 3) emit) => \"[[i32; 3]; 2]\""
  (if (null dims)
      (funcall emit element-type)
      (format nil "[~a; ~a]"
	      (rust-array-type element-type (cdr dims) emit)
	      (funcall emit (car dims)))))

(defun variable-declaration (&key name env emit mutable-default)
  (let* ((name (remove-ampersand name))
	 (decl-m (lookup-type name :env env))
	 (type (when decl-m
		   (type-definition-declaration decl-m)))
	 (m (if decl-m
		(type-definition-mutable decl-m)
		mutable-default)))
    (with-output-to-string (s)
      (when m
	(format s "mut "))
      (if (listp type)
	  (if (null type)
	      (format s "~a" (funcall emit name))
	      ;; array
	      (destructuring-bind (array_ element-type &rest dims) type
		(assert (eq array_ 'array))
		(format s "~a: ~a"
			(funcall emit name)
			(rust-array-type element-type dims emit))))
	 (format s "~a~@[: ~a~]"
		 (funcall emit name)
		 (funcall emit type))))))

(defun render-parameter (rp env emit)
  "Render one entry of a DEFUN/LAMBDA lambda list as a Rust parameter.  When no
type declaration is known the entry is emitted verbatim, which is the escape
hatch used all over the examples for things like \"&mut self\" or
\"text: &str\".

Mutability is placed the way Rust wants it: a mutable binding of an owned value
is `mut x: T', a mutable reference is `x: &mut T'."
  (multiple-value-bind (p) (remove-ampersand rp)
    (let ((decl-m (lookup-type p :env env)))
      (if decl-m
	  (let ((declaration (type-definition-declaration decl-m))
		(m (type-definition-mutable decl-m))
		(ref (type-definition-reference decl-m)))
	    (with-output-to-string (s)
	      (when (and m (not ref))
		(format s "mut "))
	      (format s "~a: " (funcall emit p))
	      (when ref
		(format s "&"))
	      (when (and m ref)
		(format s "mut "))
	      (format s "~a" (funcall emit declaration))))
	  (funcall emit rp)))))


(defun parse-let (code emit &key (mutable-default nil))
  "let ({var | (var [init-form])}*) declaration* form*"
  (destructuring-bind (decls &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body mutable-default)
      (with-output-to-string (s)
	(format s "~a"
		(funcall emit
			`(do0
			  ,@(loop for decl in decls collect
				  (destructuring-bind (name &optional value)
				      (if (listp decl) decl (list decl))
				    (format nil "let ~a~@[ = ~a~];"
					    (variable-declaration
					     :name name :env env :emit emit
					     :mutable-default mutable-default)
					    (when value
					      (funcall emit value)))))
			  ,@body)))))))

(defun parse-defun (code emit &key header-only)
  ;; defun function-name lambda-list [declaration*] form*
  (destructuring-bind (name lambda-list &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (let ((req-param lambda-list))
	(with-output-to-string (s)
	  (format s "fn ~a~a~@[ -> ~a~]"
		  (funcall emit name)
		  (funcall emit `(paren
				  ,@(loop for rp in req-param collect
					  (render-parameter rp env emit))))
		  (let ((r (gethash 'return-values env)))
		    (if (< 1 (length r))
			(funcall emit `(paren ,@r))
			(when (car r)
			 (funcall emit (car r))))))
	  (unless header-only
	    (format s " ~a" (funcall emit `(progn ,@body)))))))))

(defun parse-lambda (code emit)
  ;;  lambda lambda-list [declaration*] form*
  ;; no return value:
  ;;  |a, b| {body}
  ;; with (declare (values f32)):
  ;;  |a: i32, b: f32| -> f32 { body }
  ;; captures are not modelled; use the `move' escape hatch by writing the
  ;; whole closure head as a string if you need it.
  (destructuring-bind (lambda-list &rest body) (cdr code)
    (multiple-value-bind (body env) (consume-declare body)
      (let ((req-param lambda-list))
	(with-output-to-string (s)
	  (format s "|~a|~@[ -> ~a~]"
		  (funcall emit `(comma
				  ,@(loop for rp in req-param collect
					  (render-parameter rp env emit))))
		  (let ((r (gethash 'return-values env)))
		    (if (< 1 (length r))
			(funcall emit `(paren ,@r))
			(when (car r)
			  (funcall emit (car r))))))
	  (format s " ~a" (funcall emit `(progn ,@body))))))))





(defun clean-float-string (s)
  "Turn the output of ~G into something the Rust lexer accepts.  ~G pads its
result with blanks and happily produces \"0.\" or \".5\", none of which belongs
into generated source."
  (let ((s (string-trim '(#\Space #\Tab #\Newline) s)))
    (cond
      ;; "1." -> "1.0"
      ((and (< 0 (length s))
	    (char= #\. (aref s (1- (length s)))))
       (concatenate 'string s "0"))
      ;; "1.e+10" -> "1.0e+10"
      ((search ".e" s)
       (let ((p (search ".e" s)))
	 (concatenate 'string (subseq s 0 (1+ p)) "0" (subseq s (1+ p)))))
      ;; ".5" -> "0.5"
      ((and (< 0 (length s)) (char= #\. (aref s 0)))
       (concatenate 'string "0" s))
      ;; "1e10" (no dot at all) -> keep, Rust accepts 1e10
      (t s))))

(defun print-sufficient-digits-f32 (f)
  "print a single floating point number as a string with a given nr. of
  digits. parse it again and increase nr. of digits until the same bit
  pattern."
  (let* ((a f)
	 (digits 1)
	 (b (- a 1)))
    (unless (= a 0)
     (loop while (< 1d-6
		     (/ (abs (- a b))
		       (abs a))
		     ) do
	  (setf b (read-from-string (format nil "~,vG" digits a)))
	  (incf digits)))
    (clean-float-string (format nil "~,vG" digits a))))



(defun print-sufficient-digits-f64 (f)
  "print a double floating point number as a string with a given nr. of
  digits. parse it again and increase nr. of digits until the same bit
  pattern."

  (let* ((a f)
	 (digits 1)
	 (b (- a 1)))

    (unless (= a 0)			; (< (abs a) 1d-30)
      (loop while (< 1d-11
		     (/ (abs (- a b))
			(abs a))
		     ) do
	   (let ((str (format nil "~20,vG"  digits a))
		 (*read-default-float-format* 'double-float))
	     (setf b (read-from-string str)))
	   (incf digits)))
    (clean-float-string (format nil "~,v,,,,,'eG" digits a))))

			  
(defun top-level-comma-p (s)
  "True when S contains a comma at paren/bracket/brace depth 0.  Used to protect
tuple and argument lists from STRIP-OUTER-PARENS."
  (let ((depth 0))
    (loop for c across s
	  do (case c
	       ((#\( #\[ #\{) (incf depth))
	       ((#\) #\] #\}) (decf depth))
	       (#\, (when (<= depth 0) (return t))))
	  finally (return nil))))

(defun strip-outer-parens (s)
  "Remove one layer of enclosing parentheses from S if (and only if) the first
character is an opening paren that matches the last character and the content is
not a comma separated list.  Used where rustc's unused_parens lint would
otherwise complain about the parentheses that the operator forms add around
their result: if/while conditions, match subjects, for iterators, return values
and the right hand side of an assignment.

The scan is deliberately conservative: a paren inside a string literal makes the
depth count bail out early and S is returned unchanged.  A top level comma keeps
tuples such as (a, b) intact."
  (if (and (< 1 (length s))
	   (char= #\( (aref s 0))
	   (char= #\) (aref s (1- (length s))))
	   (let ((depth 0))
	     (loop for i below (length s)
		   do (case (aref s i)
			(#\( (incf depth))
			(#\) (decf depth)))
		   when (and (<= depth 0) (< i (1- (length s))))
		     do (return nil)
		   finally (return (= depth 0))))
	   (not (top-level-comma-p (subseq s 1 (1- (length s))))))
      (subseq s 1 (1- (length s)))
      s))

(progn
  (defparameter *keywords-without-semicolon*
    `(;; forms that expand into a Rust item or a block expression used as a
      ;; statement.  Rust does not want (and clippy complains about) a
      ;; semicolon after those.
      defun defstruct0 deftrait impl use mod
      if when unless case for dotimes while loop
      progn do0 do0-no-final-semicolon
      extern unsafe macroexpand space let let*)
    "Heads of forms that must not get a semicolon appended by DO0.")
  (defun emit-rs (&key code (str nil)  (level 0) (hook-defun nil))
    "evaluate s-expressions in code, emit a string. if hook-defun is not nil, hook-defun will be called with every function definition. this functionality is intended to collect function declarations."
    (declare (ignorable str))
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
		  (angle
		   ;; angle {args}*
		   (let ((args (cdr code)))
		     (format nil "<~{~a~^, ~}>" (mapcar #'emit args))))
		  (paren
		   ;; paren {args}*
		   (let ((args (cdr code)))
		     (format nil "(~{~a~^, ~})" (mapcar #'emit args))))
		  (tuple
		   ;; tuple {args}*, same as paren
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
				  (cdr code)))
			 #+nil
			 (let ((a (emit (cadr code))))
			   (format s "~&~a~a~{~&~a~}"
				   a
				   (if (eq #\; (aref a (- (length a) 1)))
				       ""
				       ";")
				   (mapcar
				    #'(lambda (x)
					(let ((b (emit `(indent ,x) 0)))
					  (format nil "~a~a"
						  b
						  (if (eq #\; (aref b (- (length b) 1)))
						      ""
						      ";"))))
				    (cddr code))))))
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
						  ";")))
				     (incf count)))
			       args)))))
		  (progn (with-output-to-string (s)
			   ;; progn {form}*
			   ;; like do but surrounds forms with braces.
			   ;; don't place semicolon after last statement (implicit return in rust)
			   ;; NOTE: all forms must go through ONE
			   ;; do0-no-final-semicolon.  Wrapping every form
			   ;; separately (as an earlier version did) makes every
			   ;; form the "last" one and drops all semicolons.
			   (format s "{~&~a~&}"
				   (emit `(do0-no-final-semicolon ,@(cdr code)) 1))))
		  (block (with-output-to-string (s)
			   ;; block {form}*
			   ;; like progn but with a semicolon after the last
			   ;; statement, i.e. the block evaluates to ().
			   ;; https://doc.rust-lang.org/reference/expressions/block-expr.html
			   (format s "{~&~a~&}"
				   (emit `(do0 ,@(cdr code)) 1))))
		  (do (with-output-to-string (s)
			;; do {form}*
			;; print each form on a new line with one more indentation.
			(format s "~a" (emit `(do0 ,@(cdr code)) 1))))
		  (defun
		      (prog1
			  (parse-defun code #'emit)
			(when hook-defun
			  (funcall hook-defun (parse-defun code #'emit :header-only t)))))
		  (return (format nil "return ~a"
				  (strip-outer-parens (emit (car (cdr code))))))
		  (break
		   ;; break [value]
		   (let ((args (cdr code)))
		     (format nil "break~@[ ~a~]" (when args (emit (car args))))))
		  (continue "continue")
		  (cast
		   ;; cast value type  ->  (value as type)
		   ;; deprecated alias of COERCE; the C-style (cast type value)
		   ;; that this used to emit is not valid Rust.
		   (destructuring-bind (value type) (cdr code)
		     (format nil "(~a as ~a)" (emit value) (emit type))))
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
		  ;; NOTE: the unary prefix operators wrap the WHOLE expression in
		  ;; parentheses, not just their operand.  `*(p).x' parses as
		  ;; `*((p).x)' in Rust, which is not what (dot (deref p) x) means.
		  
		  (not (format nil "(!~a)" (emit (car (cdr code)))))
		  (deref (format nil "(*~a)" (emit (car (cdr code)))))
		  (ref (format nil "(&~a)" (emit (car (cdr code)))))
		  (ref-mut (format nil "(&mut ~a)" (emit (car (cdr code)))))
		  (scope (let ((args (cdr code)))
			   (format nil "~{~a~^::~}" (mapcar #'emit args))))
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
		       (format nil "~a=~a" (emit a)
			       (strip-outer-parens (emit b)))))
		  (/= (destructuring-bind (a b) (cdr code)
			;; NOTE: this is division-assignment (a /= b), NOT the
			;; Common Lisp "not equal".  Use != for a comparison.
			(format nil "~a/=(~a)" (emit a) (emit b))))
		  (*= (destructuring-bind (a b) (cdr code)
			(format nil "~a*=(~a)" (emit a) (emit b))))
		  (^= (destructuring-bind (a b) (cdr code)
			(format nil "(~a)^=(~a)" (emit a) (emit b))))
		  ;; NOTE: binary operators parenthesise their RESULT as well as
		  ;; their operands.  Without the outer pair
		  ;;   (dot (% a b) c) -> (a)%(b).c
		  ;; which Rust parses as (a)%((b).c), because `.' binds tighter
		  ;; than any binary operator.  The redundant parentheses in
		  ;; if/while conditions are removed again by STRIP-OUTER-PARENS.
		  (<= (destructuring-bind (a b) (cdr code)
			(format nil "((~a)<=(~a))" (emit a) (emit b))))
		  (>= (destructuring-bind (a b) (cdr code)
			(format nil "((~a)>=(~a))" (emit a) (emit b))))
		  (!= (destructuring-bind (a b) (cdr code)
			(format nil "((~a)!=(~a))" (emit a) (emit b))))
		  (== (destructuring-bind (a b) (cdr code)
			(format nil "((~a)==(~a))" (emit a) (emit b))))
		  (< (destructuring-bind (a b) (cdr code)
		       (format nil "((~a)<(~a))" (emit a) (emit b))))
		  (> (destructuring-bind (a b) (cdr code)
		       (format nil "((~a)>(~a))" (emit a) (emit b))))
		  (% (destructuring-bind (a b) (cdr code)
		       (format nil "((~a)%(~a))" (emit a) (emit b))))
		  (<< (destructuring-bind (a &rest rest) (cdr code)
			(format nil "((~a)~{<<(~a)~})" (emit a) (mapcar #'emit rest))))
		  (>> (destructuring-bind (a &rest rest) (cdr code)
			(format nil "((~a)~{>>(~a)~})" (emit a) (mapcar #'emit rest))))
		  #+nil (>> (destructuring-bind (a b) (cdr code)
			      (format nil "(~a)>>~a" (emit a) (emit b))))
		  (incf (destructuring-bind (a &optional (b 1)) (cdr code) ;; py
			  (format nil "~a += ~a" (emit a) (emit b))
			  ))
		  (%= (destructuring-bind (a &optional (b 1)) (cdr code) 
			  (format nil "~a %= ~a" (emit a) (emit b))
			  ))
		  (decf (destructuring-bind (a &optional (b 1)) (cdr code)
			  (format nil "~a -= ~a" (emit a) (emit b))
			  ))
		  (byte  (format nil "b'~a'" (cadr code)))
		  (string (format nil "\"~a\"" (cadr code)))
		  (string-b (format nil "b\"~a\"" (cadr code)))
		  ;; string# / string-r  ->  raw string literal r#"..."#
		  ;; enough hashes are added so that the payload may itself
		  ;; contain quote-hash sequences.
		  ((string# string-r)
		   (let* ((str (cadr code))
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
			  (format s "if ~a ~a"
				  (strip-outer-parens (emit condition))
				  (emit `(progn ,true-statement)))
			  (when false-statement
			    (format s " else ~a"
				    (emit `(progn ,false-statement)))))))
		  (when (destructuring-bind (condition &rest forms) (cdr code)
			  ;; like IF but with an implicit body block, so several
			  ;; forms can be given
			  (format nil "if ~a ~a"
				  (strip-outer-parens (emit condition))
				  (emit `(progn ,@forms)))))
		  (unless (destructuring-bind (condition &rest forms) (cdr code)
			    (format nil "if ~a ~a"
				    (strip-outer-parens (emit `(not ,condition)))
				    (emit `(progn ,@forms)))))
		  (coerce (let ((args (cdr code)))
			(destructuring-bind (name type) args
			    (format nil "(~a as ~a)" (emit name) (emit type)))))
		  (dot (let ((args (cdr code)))
			 (format nil "~{~a~^.~}" (mapcar #'emit args))))
		  

		  (aref (destructuring-bind (name &rest indices) (cdr code)
			  ;(format t "aref: ~a ~a~%" (emit name) (mapcar #'emit indices))
			  (format nil "~a[~{~a~^,~}]" (emit name) (mapcar #'emit indices))))
		  
		  (lambda (parse-lambda code #'emit))
		  #+nil (unsafe (let ((args (cdr code)))
			    (format nil "unsafe {~{~a~}}" args)))
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
		      ;; normal-clause::= (key form*)
		      ;; otherwise-clause::= (t form*)
		      ;; key may be a symbol (None), a list that emits a pattern
		      ;; ((Some x) ...) or a string for anything else.
		      (destructuring-bind (keyform &rest clauses)
			  (cdr code)
			(format
			 nil "match ~a ~a"
			 (strip-outer-parens (emit keyform))
			 (emit
			  `(progn
			     ,@(loop for c in clauses collect
				    (destructuring-bind (key &rest forms) c
				      ;; NOTE: pass FORMS on unevaluated, do not
				      ;; pre-emit them.  Emitting twice turned
				      ;; every arm body into a string, which
				      ;; suppressed all statement semicolons.
				      (format nil "~a => ~a,"
					      (if (eq key t)
						  "_"
						  (emit key))
					      (emit `(progn ,@forms))))))))))
		  (dotimes
		      ;; dotimes (var count [step]) {form}*
		      ;;   -> for var in 0..count { }
		      ;;   -> for var in (0..count).step_by(step) { }
		      (destructuring-bind ((i n &optional (step 1)) &rest body) (cdr code)
			(format nil "for ~a in ~a ~a"
				(emit i)
				(if (eql step 1)
				    (format nil "0..~a" (emit n))
				    (format nil "(0..~a).step_by(~a)"
					    (emit n) (emit step)))
				(emit `(progn ,@body)))))
		  (loop (let ((args (cdr code)))
			  (format nil "loop ~a"
				  (emit `(progn ,@args)))))
		  (for (destructuring-bind ((item collection) &rest body) (cdr code)
			     (format nil "for ~a in ~a ~a"
				     (emit item)
				     (strip-outer-parens (emit collection))
				     (emit `(progn ,@body)))))
		  (while  ;; while condition {forms}*
		      (destructuring-bind (condition &rest body) (cdr code)
			;; no parentheses around the condition, rustc's
			;; unused_parens lint complains about them
			(format nil "while ~a ~a"
				(strip-outer-parens (emit condition))
				(emit `(progn ,@body)))))
		  (deftype
		      ;; deftype name lambda-list {form}*
		      ;; only the first form of the body is used, lambda list is ignored
		      ;; (deftype my-u () u64) -> type my_u = u64
		      (destructuring-bind (name lambda-list &rest body) (cdr code)
			(declare (ignore lambda-list))
			(format nil "type ~a = ~a" (emit name) (emit (car body)))))
		  (struct (format nil "struct ~a" (emit (car (cdr code)))))
		  (defstruct0
		   ;; defstruct0 name {slot-description}*
		   ;; slot-description::= (slot-name slot-type)
		   ;;
		   ;; (defstruct0 Point (x f64) (y f64))
		   ;;  -> struct Point { x: f64, y: f64, }
		   ;;
		   ;; Attributes such as "#[derive(Clone)]" are written as plain
		   ;; strings in front of the form.
		   (destructuring-bind (name &rest slot-descriptions) (cdr code)
		     (format nil "struct ~a ~a"
			     (emit name)
			     (emit
			      `(progn
				 ,@(loop for desc in slot-descriptions collect
					 (destructuring-bind (slot-name &optional type value) desc
					   (declare (ignorable value))
					   (format nil "~a: ~a," (emit slot-name) (emit type)))))))))
		  ((handler-case throw include defclass protected public ->  new)
		   ;; These forms are leftovers from the C/C++ generator this file
		   ;; started out as.  Whatever they used to emit is not valid
		   ;; Rust, so fail loudly instead of producing garbage.
		   (error "cl-rust-generator: the form ~s is not supported.~%~a"
			  (car code)
			  (case (car code)
			    (handler-case "Rust has no exceptions. Use (case expr ((Ok v) ...) ((Err e) ...)) or the ? operator.")
			    (throw "Rust has no exceptions. Return (Err ...) or use panic!.")
			    (include "Use (use (std io)) / (mod name) instead of #include.")
			    (defclass "Use (defstruct0 ...) together with (impl ...).")
			    ((protected public) "Write \"pub\" as a plain string in front of the item.")
			    (-> "-> is only valid in a function signature; use (dot a b) for member access.")
			    (new "Rust has no `new' keyword; call the associated function, e.g. (\"Vec::new\")."))))
		  (t (destructuring-bind (name &rest args) code

		       (if (listp name)
			   ;; lambda call and similar complex constructs
			   (format nil "(~a)~a"
				   (emit name)
				   (emit `(paren ,@args))
				   )
			   ;; function call
			   
			   
			   (progn	;if
			     
			     #+nil(and
				   (= 1 (length args))
				   (eq (aref (format nil "~a" (car args)) 0) #\.))
			     #+nil (format nil "~a~a" name
					   (emit args))
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
		#+nil ((stringp code) 
		 (format nil "~a" code))
		((numberp code) ;; print constants
		 (cond ((integerp code)
			(if (< code 0)
			    (format nil "(~a)" code)
			    (format nil "~a" code)))
		       ((floatp code)
			(typecase code
			  (single-float (let ((v (print-sufficient-digits-f32 code)))
					  (if (< code 0)
					      (format nil "(~a)" v)
					      (format nil "~a" v))))
			  (double-float (let ((v (print-sufficient-digits-f64 code)))
					  (if (< code 0)
					      (format nil "(~a)" v)
					      (format nil "~a" v))))))))))
	  "")))
  #+nil (progn
   (defparameter *bla*
     (emit-rs :code `(do0
		     (include <stdio.h>)
		     (defun main (argc argv)
		       (declare (type int argc)
				(type char** argv)
				(values int))
		       (printf (string "hello world!"))
		       (return 0)))))
   (format t "~a" *bla*)))

#+nil((ntuple (let ((args (cdr code)))
			   (format nil "~{~a~^, ~}" (mapcar #'emit args))))
		 (paren
		  ;; paren {args}*
		  (let ((args (cdr code)))
		    (format nil "(~{~a~^, ~})" (mapcar #'emit args))))
		 (braces
		  ;; braces {args}*
		  (let ((args (cdr code)))
		    (format nil "{~{~a~^, ~}}" (mapcar #'emit args))))
      (curly ;; name{arg1, args}
		  ;; or name{key1:arg1, key2:arg2}
		  (destructuring-bind (name &rest args) (cdr code)
		    (emit `(cast ,name
				 (braces
				  ,@(if (keywordp (car args))
					(loop for i below (length args) by 2 collect
					     (let ((a (elt args i))
						   (b (elt args (+ 1 i))))
					       (format nil "~a: ~a" (emit a) (emit b))))
					args))))))
		      (cast ;; cast type value
		       (destructuring-bind (type value) (cdr code)
			 (format nil "~a ~a" (emit type) (emit value)))
		       )
		      (dict
		       ;; dict {pair}*
		       (let* ((args (cdr code)))
			 (let ((str (with-output-to-string (s)
				      (loop for (e f) in args
					 do
					   (format s "~a: ~a," (emit e) (emit f))))))
			   (format nil "{~a}" ;; remove trailing comma
				   (subseq str 0 (- (length str) 1))))))
		      (go (format nil "go ~a" (emit (car (cdr code)))))
		      (range (format nil "range ~a" (emit (car (cdr code)))))
		      (chan (format nil "chan ~a" (emit (car (cdr code)))))
		      (defer (format nil "defer ~a" (emit (car (cdr code)))))
      (return (format nil "return ~a" (emit (car (cdr code)))))
		      
      (do (with-output-to-string (s)
			    ;; do {form}*
			    ;; print each form on a new line with one more indentation.
	    (format s "~{~&~a~}" (mapcar #'(lambda (x) (emit `(indent ,x) 1)) (cdr code)))
	    (progn (with-output-to-string (s)
		     ;; progrn {form}*
		     ;; like do but surrounds forms with braces.
		     (format s "{~{~&~a~}~&}" (mapcar #'(lambda (x) (emit `(indent ,x) 1)) (cdr code)))))))
		      
		      (let (parse-let code #'emit))
		      
		      (defun (parse-defun code #'emit))
		      (defun-declaration (parse-defun-declaration code #'emit))
      
		      (defmethod (parse-defmethod code #'emit))
		      (defmethod-interface (parse-defmethod-interface code #'emit))
		      (defmethod-declaration (parse-defmethod-declaration code #'emit))
		      #+nil (defstruct
				;;  defstruct name {slot-description}*
				;; slot-description::= slot-name | (slot-name [slot-initform [[slot-option]]]) 
				;; slot-option::= :type slot-type
				(destructuring-bind (name &rest slot-descriptions) (cdr code)
				  (format
				   nil "type ~a struct ~a"
				   name
				   (emit
				    `(progn
				       ,@(loop for desc in slot-descriptions collect
					      (destructuring-bind (slot-name ;; &optional init
								   ;; init doesnt really fit into go semantics
								   &key type) desc
						(format nil "~a~@[ ~a~]" slot-name type))))))))
		      (deftype
			  ;; deftype name lambda-list {form}*
			  ;; only the first form of the body is used, lambda list is ignored
			  (destructuring-bind (name lambda-list &rest body) (cdr code)
			    (declare (ignore lambda-list))
			    (format nil "type ~a ~a" name (emit (car body)))))
		      

		      (definterface
			  
			  ;; definterface name {slot-description}*
			  ;; slot-description::= other-interface-name | method-interface-declaration

			  (destructuring-bind (name &rest slot-descriptions) (cdr code)
			    (format nil "type ~a interface ~a"
				    name
				    (emit
				     `(progn
					,@(mapcar #'emit slot-descriptions))))))
		      (setf (parse-setf code #'emit))
		      (const (parse-const code #'emit))
		      (assign
		       ;; assign {pair}*
		       (let ((args (cdr code)))
			 (format nil "~a~%"
				 (emit
				  `(do0 
				    ,@(loop for i below (length args) by 2 collect
					   (let ((a (elt args i))
						 (b (elt args (+ 1 i))))
					     `(:= ,a ,b))))))))
      
		      (ecase
			  ;; ecase keyform {normal-clause}*
			  ;; normal-clause::= (keys form*) 
			  (destructuring-bind (keyform &rest clauses)
			      (cdr code)
			    (format
			     nil "switch ~a ~a"
			     (emit keyform)
			     (emit
			      `(progn
				 ,@(loop for c in clauses collect
					(destructuring-bind (key &rest forms) c
					  (format nil "case ~a:~&~a"
						  (emit key)
						  (emit
						   `(do0
						     ,@(mapcar #'emit
							       forms)))))))))))
      
		      (for
		       ;; for [init [condition [update]]] {forms}*
		       (destructuring-bind ((&optional init condition update) &rest body)
			   (cdr code)
			 (with-output-to-string (s)
			   (format s "for ~a ; ~a; ~a "
				   (if init
				       (emit init)
				       "")
				   (if condition
				       (emit condition)
				       "")
				   (if update
				       (emit update)
				       ""))
			   (format s "~a" (emit `(progn ,@body))))))
		      (foreach
		       ;; foreach [var] range {forms}*
		       ;; foreach range {forms}*
		       (destructuring-bind ((&rest decl) &rest body) (cdr code)
			 (with-output-to-string (s)
			   (format s "for ~a "
				   (if (< 1 (length decl))
				       (destructuring-bind (var range) decl
					 (emit `(:= ,var ,range)))
				       (emit (car decl))))
			   (format s "~a" (emit `(progn ,@body))))))

		      
		      (dotimes (destructuring-bind ((var end) &rest body) (cdr code)
				 (emit `(for ((:= ,var 0)
					      (< ,var ,end)
					      (incf ,var))
					     ,@body))))
      
		      
      (slice (let ((args (cdr code)))
			       (if (null args)
				   (format nil ":")
				   (format nil "~{~a~^:~}" (mapcar #'emit args)))))
		      
		      #+nil (-> (let ((forms (cdr code)))
				  ;; clojure's thread first macro, thrush operator
				  ;; http://blog.fogus.me/2010/09/28/thrush-in-clojure-redux/
				  ;; -> {form}*
				  (emit (reduce #'(lambda (x y) (list (emit x) (emit y))) forms)))))
