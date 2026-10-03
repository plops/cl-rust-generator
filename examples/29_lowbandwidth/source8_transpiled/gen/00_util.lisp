(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload :cl-rust-generator :silent t))

(in-package :cl-rust-generator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (readtable-case *readtable*) :invert))

;;;; 00_util.lisp --- shared paths, builders and tables for the
;;;; source8_transpiled generator. Load first; gen.lisp loads the rest.
;;;;
;;;; Conventions (see SUPPORTED_FORMS.md at the repo root):
;;;; - Lisp `my-fn-name` becomes Rust `my_fn_name`; `a--b` becomes `a::b`.
;;;; - Write every identifier exactly as it shall appear in Rust
;;;;   (readtable :invert round-trips the case).
;;;; - Floats are written as strings ("0.3"), never as Lisp numbers
;;;;   (the emitter would print 0.50-style digits).
;;;; - Generics, lifetimes, `self`, struct-variant patterns and let-chains
;;;;   are strings (the documented escape hatch).
;;;; - (range ...) forms keep their parentheses: fine for `for` (which
;;;;   strips them) and dot receivers (which need them), but rejected by
;;;;   unused_parens in index/argument position. There, build the range
;;;;   with (space ..): (aref buf (space ".." HEADER)) => buf[..HEADER].
;;;; - Mirror every explicit paren pair of the source7 code with (paren ...);
;;;;   omit-mode drops only redundant ones, and clippy's precedence lint
;;;;   wants the rest back.
;;;; - A leading string in a defun/let/lambda body is swallowed as a Lisp
;;;;   docstring. Emitted leading comments need a progn wrapper:
;;;;   (defun f () (progn "// comment" ...)) -- the singleton progn is
;;;;   spliced, so no extra braces appear.

(defparameter *source-dir* #P"examples/29_lowbandwidth/source8_transpiled/")

(defun s8-path (name)
  (asdf:system-relative-pathname 'cl-rust-generator
                                 (merge-pathnames name *source-dir*)))

(defun pub_ (form)
  "Wrap a defun/defstruct0/defenum/impl item as a pub item."
  `(space "pub" ,form))

(defparameter *blank* " "
  "Blank-line item for do0/progn sequences: splice with ,*blank* as a
direct template child, but as plain *blank* inside an already-unquoted
call like ,(testmod ...) (a comma there would be outside the backquote).
Inside quoted defuns (',(defun ...)) use a literal single-space string.
A single space becomes a true blank line after rustfmt (which strips
the space, so `cargo fmt --check` stays green). Needed between
use/mod groups: without blank lines rustfmt would reorder them
(reorder_imports/reorder_modules). Cosmetic everywhere else.")

(defun doc (&rest lines)
  "Module doc comment block: each line becomes `//! ...` (empty line: `//!`)."
  (with-output-to-string (s)
    (loop for (l . more) on lines
          do (if (string= l "")
                 (write-string "//!" s)
                 (format s "//! ~a" l))
             (when more (terpri s)))))

(defun testmod (&rest body)
  "Build `#[cfg(test)] mod tests { ... }`."
  `(attr "cfg(test)" (space "mod tests" (progn ,@body))))

(defun write-text-file (relpath content &key executable)
  "Write CONTENT (a string) to RELPATH under *source-dir*, only when changed.
Returns T when the file was written."
  (let ((fn (s8-path relpath)))
    (ensure-directories-exist fn)
    (let ((old (when (probe-file fn)
                 (with-open-file (s fn :external-format :utf-8)
                   (let ((buf (make-string (file-length s))))
                     (subseq buf 0 (read-sequence buf s)))))))
      (unless (and old (string= old content))
        (with-open-file (s fn :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
          (write-sequence content s))
        #+sbcl (when executable
                 (sb-posix:chmod fn #o755))
        t))))

;;; ------------------------------------------------------------------
;;; Key table: single source of truth for special keys.
;;; Each entry: (protocol-name client-keycodes enigo-key).
;;; - protocol-name: the String sent over TCP ("Enter", "Esc", ...).
;;; - client-keycodes: macroquad KeyCode symbols mapping to it
;;;   (Shift/Control/Alt have left+right variants).
;;; - enigo-key: enigo::Key variant used by the server injector
;;;   (note "Alt" -> Option, the enigo name for the Alt key).

(defparameter +key-table+
  '(("Enter" (Enter) Return)
    ("Esc" (Escape) Escape)
    ("Tab" (Tab) Tab)
    ("Backspace" (Backspace) Backspace)
    ("Delete" (Delete) Delete)
    ("Up" (Up) UpArrow)
    ("Down" (Down) DownArrow)
    ("Left" (Left) LeftArrow)
    ("Right" (Right) RightArrow)
    ("Home" (Home) Home)
    ("End" (End) End)
    ("PageUp" (PageUp) PageUp)
    ("PageDown" (PageDown) PageDown)
    ("Shift" (LeftShift RightShift) Shift)
    ("Control" (LeftControl RightControl) Control)
    ("Alt" (LeftAlt RightAlt) Option)))

(defun server-key-arms ()
  "Match arms for the server `key_code` fn: `\"Enter\" => Key::Return`."
  (loop for (name codes key) in +key-table+
        collect `(,(format nil "~s" name) (scope Key ,key))))

(defun known-keys-test-list ()
  "Subset asserted in the server `known_keys_resolve` test."
  (loop for name in '("Enter" "Esc" "Tab" "Left" "Control" "Alt")
        collect `(string ,name)))

(defun client-key-pairs ()
  "Pairs for the client `send_input` table: `(KeyCode::Enter, \"Enter\")`."
  (loop for (name codes key) in +key-table+
        append (loop for c in codes
                     collect `(paren (scope KeyCode ,c) (string ,name)))))

(defun channel-arefs (arr idx &optional (order '(0 1 2)))
  "`ARR[IDX+k]` je k in ORDER (vermeidet `+ 0`, damit clippy's
identity_op still bleibt). ARR/IDX sind gequotete Ausdrücke/Symbole."
  (loop for k in order
        collect `(aref ,arr ,(if (zerop k) idx (list '+ idx k)))))

(defun clap-struct (doc-lines struct-attrs name fields)
  "Assemble a clap derive-Parser struct as one string (field docs and
field attributes fit no defstruct0 slot). DOC-LINES is a list of ///
lines (\"\" for a bare ///), STRUCT-ATTRS a list of attribute strings,
FIELDS a list of (doc attr name type) specs."
  (with-output-to-string (s)
    (loop for l in doc-lines do
      (if (string= l "")
          (write-string "///" s)
          (format s "/// ~a" l))
      (terpri s))
    (loop for a in struct-attrs do (format s "#[~a]~%" a))
    (format s "pub struct ~a {~%" name)
    (loop for (doc attr fname ftype) in fields do
      (format s "    /// ~a~%" doc)
      (format s "    #[~a]~%" attr)
      (format s "    pub ~a: ~a,~%" fname ftype))
    (write-string "}" s)))

(defun trymap (expr err-body)
  "EXPR.map_err(|e| ERR-BODY)? -- the ubiquitous String-error mapping.
Splice with ,(trymap 'EXPR 'ERR-BODY) (both quoted data), but ONLY as a
direct template child -- never inside ,(pub_ '(...)): the inner comma
would sit at backquote depth 0 (\"Comma not inside a backquote\").
There, inline (? (dot EXPR (map_err (lambda (e) ERR-BODY)))) instead."
  `(? (dot ,expr (map_err (lambda (e) ,err-body)))))
