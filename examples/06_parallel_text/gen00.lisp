(eval-when (:compile-toplevel :execute :load-toplevel)
  (ql:quickload "cl-rust-generator"))

(in-package :cl-rust-generator)

(declaim (optimize (speed 0)
		   (safety 3)
		   (debug 3)))

;; cargo test
;; cargo run
;; cargo clean
;; cargo fix

;; https://github.com/ProgrammingRust/fingertips
;; example from the channels chapter

(progn
  (defparameter *source-dir* #P"examples/06_parallel_text/code/src/")

  (with-open-file (s (asdf:system-relative-pathname 'cl-rust-generator
				   (merge-pathnames #P"../Cargo.toml"
						    *source-dir*))
		     
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (format s "~a"
	    "[package]
name = \"code\"
version = \"0.1.0\"
authors = [\"Martin Kielhorn <kielhorn.martin@gmail.com>\"]
edition = \"2018\"

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

[dependencies]
argparse = \"*\"
byteorder = \"*\"
"


	    ))
  
  #+nil (defparameter *code-file*
    (asdf:system-relative-pathname 'cl-rust-generator
				   (merge-pathnames #P"main.rs"
						    *source-dir*)))


  (defparameter *module* nil)
  (defun define-module (args)
    (destructuring-bind (module-name module-code) args
	(push `(:name ,module-name :code ,module-code)
	      *module*)))


  (define-module
      `(index
	(do0
	 (use (std collections HashMap)
	      (byteorder "{LittleEndian,WriteBytesExt}"))
	 (defun tokenize ("text: &str")
	   (values "Vec<&str>")
	   (dot text
		(split (lambda ("ch: char")
			 (return (not (ch.is_alphanumeric)))))
		(filter (lambda (word)
			  (return (not (word.is_empty)))))
		(collect)))
	 (space pub
		(defstruct0 InMemoryIndex
		    ("pub word_count" usize)
		  ("pub map" "HashMap<String,Vec<Hit>>"))
		)
	 "pub type Hit = Vec<u8>;"
	 (impl InMemoryIndex
	       (space pub
		      (defun new ()
			(declare (values InMemoryIndex))
			(make-instance InMemoryIndex
				       :word_count 0
				       :map ("Hashmap::new"))))
	       (space pub
		      (defun from_single_document ("document_id: usize"
						   "text: String")
			(declare (values InMemoryIndex))
			(let ((document_id "document_id as u32")))))))))
  
  (define-module
      `(main
	(do0
	 (use (std fs File)
	      (std io prelude *)
	      (std thread spawn)
	      (std sync mpsc channel))
	 (defun start_file_reader_thread ("documents: Vec<PathBuf>")
	   (declare (values "Receiver<String>"
			    "JoinHandle<io::Result<()>>"))
	   ;; use std::sync::mpsc::sync_channel (sync_channel 100) to create back pressure
	   (let (((paren sender receiver) (channel))
		 (handle (spawn
			  ;; transfer ownership of sender
			  (space
			   move
			   (lambda ()
			     (for (filename documents)
				  (let* ((f (? ("File::open" filename)))
					 (text ("String::new")))

				    ;; use ? so that errors don't pass silently
				    (? (f.read_to_string "&mut text"))
				    ;; after successful read send to channel (only 3 words go through channel)
				    (when (dot sender
					       (send text)
					       (is_err))
				      ;; send only fails if the receiver has been dropped
				      ;; in that case the receiver just exits early with Ok
				      break)))
			     (return (Ok "()"))
			     ))))
		 
		 )
	     (do0
	      ;; result of closure is in threads JoinHandle
	      (return (values receiver handle)))))


	 (defun start_file_indexing_thread ("texts: Receiver<String>")
	   (declare (values "Receiver<InMemoryIndex>"
			    "JoinHandle<()>"))
	   (let (((paren sender
			 receiver)
		  (channel))
		 (handle
		  (spawn
		   (space move
			  (lambda ()
			    (for ((paren doc_id text)
				  (dot texts
				       (into_iter)
				       (enumerate)))
				 (let ((index ("InMemoryIndex::from_single_document" doc_id text)))
				   (if (dot sender
					    (send index)
					    (is_err))
				       break)))
			    )))))
	     (return (values receiver handle))))

	 (do0
	  ;; receiver will block
	  ;; loop exits when channel is empty and sender has been dropped
	  (for (text receiver)
	       
	       ))


	 (defun start_in_memory_merge_thread ("file_indexes: Receiver<InMemoryIndex>")
	   (declare (values "Receiver<InMemoryIndex>"
			    "JoinHandle<()>")))

	 (defun start_index_writer_thread ("big_indexes: Receiver<InMemoryIndex>"
					   "output_dir: &Path")
	   (declare (values "Receiver<PathBuf>"
			    "JoinHandle<io::Result<()>>")))

	 (defun merge_index_files ("files: Receiver<PathBuf>"
				   "output_dir: &Path")
	   (declare (values "io::Result<()>"))
	   ;; produces single output file on disk
	   )

	 (defun run_pipeline ("documents: Vec<PathBuf>"
			      "output_dir: PathBuf")
	   (declare (values "io::Result<()>"))
	   (let (((paren texts h1) (start_file_reader_thread documents))
		 ((paren pints h2) (start_file_indexing_thread texts))
		 ((paren gallons h3) (start_in_memory_merge_thread pints))
		 ((paren files h4) (start_index_writer_thread gallons &output_dir))
		 (result (merge_index_files files &output_dir))
		 (r1 (dot h1
			  (join)
			  (unwrap))))
	     ;; h2 and h3 can't fail (pure in-memory)
	     (dot h2 (join) (unwrap))
	     (dot h3 (join) (unwrap))
	     (let ((r4 (dot h4 (join) (unwrap)))))
	     ;; return first error encountered
	     (? r1)
	     (? r4)
	     (return result)))

	 (defun expand_filename_arguments ("args: Vec<String>")
	   (values "io::Result<Vec<PathBuf>>")
	   (let* ((filenames "vec![]"))
	     (for (arg args)
		  (let ((path ("PathBuf::from" arg)))
		    (if (dot path
			     (? (metadata))
			     (is_dir))
			(for (entry (? (path.read_dir)))
			     (let ((entry (? entry)))
			       (when (dot entry
					  (? (file_type))
					  (is_file))
				 (filenames.push
				  (entry.path)))))
			(filenames.push path))))
	     (return (Ok filenames))))

	 (defun run ("filenames: Vec<String>")
	   (values "io::Result<()>")
	   (let ((output_dir ("PathBuf::from" (string ".")))
		 (documents (? (expand_filename_arguments filenames))))
	     (run_pipeline documents output_dir)))
	 
	 (defun main ()
	   (let* ((filenames "vec![]"))
	     (case (run filenames)
	       ((Ok "()") (return "{}"))
	       ((Err err) (println! (string "error: {:?}")
				    (err.description)))))))))


  (loop for e in (reverse *module*) and i from 0 do
	   (destructuring-bind (&key name code) e
	     (write-source (asdf:system-relative-pathname 'cl-rust-generator
							  (merge-pathnames (format nil "~a.rs" name)
									   *source-dir*))
			   code))))
