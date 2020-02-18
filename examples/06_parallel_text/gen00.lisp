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
# edition = \"2018\"

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
	   (declare (values "Vec<&str>"))
	   (return (dot text
		 (split (lambda ("ch: char")
			  (return (not (ch.is_alphanumeric)))))
		 (filter (lambda (word)
			   (return (not (word.is_empty)))))
		 (collect))))
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
			(return (make-instance InMemoryIndex
					:word_count 0
					:map ("HashMap::new")))))
	       (space pub
		      (defun from_single_document ("document_id: usize"
						   "text: String")
			(declare (values InMemoryIndex))
			(let ((document_id "document_id as u32")
			      (index ("InMemoryIndex::new"))
			      (text (text.to_lowercase))
			      (tokens (tokenize &text)))
			  (declare (mutable index))
			  (for ((values i token)
				(dot tokens
				     (iter)
				     (enumerate)))
			       (let ((hits (dot index
						map
						(entry (token.to_string))
						(or_insert_with (lambda ()
								  (let* ((hits ("Vec::with_capacity" (+ 4 4))))
								    (dot hits
									 ("write_u32::<LittleEndian>" document_id)
									 (unwrap))
								    (return "vec![hits]")))))))
				 (dot (aref hits 0)
				      ("write_u32::<LittleEndian>" document_id)
				      (unwrap))
				 (incf index.word_count)))
			  (when (== 0 (% document_id 100))
			    (println! (string "indexed document {}, {} bytes, {} words")
				      document_id
				      (text.len)
				      index.word_count))
			  (return index))))
	       (space pub
		      (defun merge ("&mut self"
				    "other: InMemoryIndex")
			(for ((values term hits)
			      other.map)
			     (dot self
				  map
				  (entry term)
				  (or_insert_with (lambda () (return "vec![]")))
				  (extend hits)))
			(incf self.word_count other.word_count)))
	       (space pub
		      (defun is_empty (&self)
			(declare (values bool))
			(return (== 0 self.word_count))))
	       (space pub
		      (defun is_large (&self)
			(declare (values bool))
			(return (< "100_000_000" self.word_count))))
	       ))))
  (define-module
      `(tmp
	(do0
	 (use (std io (curly self BufWriter))
	      (std fs (curly self File))
	      (std path (curly Path PathBuf)))
	  "#[derive(Clone)]"
	 (space pub
		(defstruct0 TmpDir
		    (dir PathBuf)
		  (n usize)))
	 (impl TmpDir
	       (space pub
		      (defun "new<P: AsRef<Path>>"  ("dir: P")
			(declare (values TmpDir))
			(return (make-instance TmpDir
					       :dir (dot dir
							 (as_ref)
							 (to_owned))
					       :n 1))))
	       (space pub
		      (defun create ("&mut self")
			(declare (values "io::Result<(PathBuf, BufWriter<File>)>"))
			(let* ((try_count 1))
			  (loop
			     (let ((filename (dot self
						  dir
						  (join ("PathBuf::from"
							 (format! (string "tmp{:08x}.dat")
								  self.n))))))
			       (incf self.n)
			       (case (dot ("fs::OpenOptions::new")
					  (write true)
					  (create_new true)
					  (open &filename))
				 ((Ok f) (return (Ok (values filename
							     ("BufWriter::new" f)))))
				 ((Err exc)
				  (unless (and (< try_count 999)
					       (== "io::ErrorKind::AlreadyExists"
						   (exc.kind)))
				    (return (Err exc))))))
			     (incf try_count)))))))))

  (define-module
      `(write
	(do0
	 (use (std fs File)
	      (std io (curly self BufWriter SeekFrom))
	      (std io prelude *)
	      (std path PathBuf)
	      (index InMemoryIndex)
	      (tmp TmpDir)
	      (byteorder (curly LittleEndian WriteBytesExt)))
	 
	 (space pub
		(defstruct0 IndexFileWriter
		    (offset u64)
		  (writer BufWriter<File>)
		  (contents_buf Vec<u8>)))
	 (impl IndexFileWriter
	       (space pub
		      (defun new ("mut f: BufWriter<File>")
			(declare (values "io::Result<IndexFileWriter>"))
			"const HEADER_SIZE: u64 = 8;"
			(? ("f.write_u64::<LittleEndian>" 0))
			
			(return (Ok (make-instance IndexFileWriter
						   :offset HEADER_SIZE
						   :writer f
						   :contents_buf "vec![]")))))
	       (space pub
		      (defun write_main ("&mut self"
					 "buf: &[u8]")
			(declare (values "io::Result<()>"))
			(?
			 (self.writer.write_all buf)
			 )
			(incf self.offset (coerce (buf.len) u64))
			(return (Ok "()"))))
	       (space pub
		      (defun write_contents_entry ("&mut self"
						   "term: String"
						   "df: u32"
						   "offset: u64"
						   "nbytes: u64")
			,@(loop for (e type) in `((offset u64)
						  (nbytes u64)
						  (df u32)) collect
			       `(dot self
				     contents_buf
				     (,(format nil "write_~a::<LittleEndian>" type) ,e)
				     (unwrap)))
			(let ((bytes (term.bytes)))
			  ,@(loop for (e type) in `(((bytes.len) u32)) collect
			       `(dot self
				     contents_buf
				     (,(format nil "write_~a::<LittleEndian>" type) (coerce ,e ,type))
				     (unwrap))))
			(self.contents_buf.extend bytes)))
	       (space pub
		      (defun finish ("mut self")
			(declare (values "io::Result<()>"))
			(let ((contents_start self.offset))
			  (?
			   (dot self
				writer
				(write_all &self.contents_buf)))
			  (println! (string "{} bytes main, {} bytes total")
				    contents_start
				    (+ contents_start (coerce (dot self
							       contents_buf
							       (len))
							      u64)))
			  (?
			   (dot self
				writer
				(seek ("SeekFrom::Start" 0))))
			  (?
			   (dot self
				writer
				("write_u64::<LittleEndian>" contents_start)))
			  (return (Ok "()"))))))
	 (space pub
		(defun write_index_to_tmp_file ("index: InMemoryIndex"
						"tmp_dir: &mut TmpDir")
		  (declare (values "io::Result<PathBuf>"))
		  (let (((values filename f) (? (tmp_dir.create))))
		    (let* ((writer (? ("IndexFileWriter::new" f)))
			   (index_as_vec (dot index
					      map
					      (into_iter)
					      (collect))))
		      (declare (type "Vec<_>" index_as_vec))
		      (dot index_as_vec
			   (sort_by (lambda ("&(ref a,_)"
					     "&(ref b,_)")
				      (return (a.cmp b)))))
		      (for ((values term hits) index_as_vec)
			   (let ((df (coerce (hits.len) u32))
				 (start writer.offset))
			     (for (buffer hits)
				  (?
				   (writer.write_main &buffer)))
			     (let ((stop writer.offset))
			       (writer.write_contents_entry
				term df start (- stop start)))))
		      (?
		       (writer.finish)
		       )
		      (println! (string "wrote file {:?}") filename)
		      (return (Ok filename)))))))))

  (define-module
      `(read
	(do0
	 (use (std fs (curly self File))
	      (std io (curly self BufReader SeekFrom))
	      (std io prelude *)
	      (std path Path)
	      (byteorder (curly LittleEndian ReadBytesExt))
	      (write IndexFileWriter))
	 
	 (space pub
		(defstruct0 IndexFileReader
		    (main BufReader<File>)
		  (contents BufReader<File>)
		  (next Option<Entry>)))
	 
	 (space pub
		(defstruct0 Entry
		    ("pub term" String)
		  ("pub df" u32)
		  ("pub offset" u64)
		  ("pub nbytes" u64)))

	 

	 
	 (impl IndexFileReader
	       (space pub
		      (defun "open_and_delete<P: AsRef<Path>>" ("filename: P")
			(declare (values io--Result<IndexFileReader>))
			(let ((filename (filename.as_ref)))
			  (let* ((main_raw (? (File--open filename))))
			    (let ((contents_offset (? (main_raw.read_u64--<LittleEndian>))))
			      (println! (string "opened {}, table of contents starts at {}")
					(filename.display)
					contents_offset)
			      (let* ((contents_raw (? (File--open filename))))
				(? (contents_raw.seek (SeekFrom--Start contents_offset)))
				(let ((main (BufReader--new main_raw))
				      (contents (BufReader--new contents_raw))
				      (first (? (IndexFileReader--read_entry "&mut contents"))))
				  (declare (mutable contents))
				  (? (fs--remove_file filename))
				  (return (Ok (make-instance IndexFileReader
						      :main main
						      :contents contents
						      :next first))))))))))
	       
	       (space pub
		      (defun read_entry ("f: &mut BufReader<File>")
			(declare (values io--Result<Option<Entry>>))
			(let ((offset (case (f.read_u64--<LittleEndian>)
					((Ok value) value)
					((Err err) (if (== io--ErrorKind--UnexpectedEof
							   (err.kind))
						       (return (Ok None))
						       (return (Err err))))))
			      (nbytes (? (f.read_u64--<LittleEndian>)))
			      (df (? (f.read_u32--<LittleEndian>)))
			      (term_len (coerce (? (f.read_u32--<LittleEndian>))
						usize))
			      (bytes (Vec--with_capacity term_len)))
			  (declare (mutable bytes))
			  (bytes.resize term_len 0)
			  (? (f.read_exact "&mut bytes"))
			  (let ((term (case (String--from_utf8 bytes)
					((Ok s) s)
					((Err _) (return (Err (io--Error--new
							       io--ErrorKind--Other
							       (string "unicode fail"))))))))
			    (return (Ok (Some (make-instance Entry
							  term
							  df
							  offset
							  nbytes))))))))
	       (space pub
		      (defun peek (&self)
			(declare (values Option<&Entry>))
			(return (self.next.as_ref))))

	       (space pub
		      (defun is_at (&self
				    "term: &str")
			(declare (values bool))
			(case self.next
			  ((Some "ref e") (return (== term e.term)))
			  (None (return false)))))

	       (space pub
		      (defun move_entry_to ("&mut self"
					    "out: &mut IndexFileWriter")
			(declare (values "io::Result<()>"))
			(progn
			  (let ((e (dot self
					next
					(as_ref)
					(expect (string "no entry to move")))))
			    (when (< (coerce (usize--max_value) u64)
				     e.nbytes)
			      (return (Err (io--Error--new
					    io--ErrorKind--Other
					    (string "computer not big enough to hold index entry")))))
			    (let* ((buf (Vec--with_capacity (coerce e.nbytes usize))))
			      (buf.resize (coerce e.nbytes usize)
					  0)
			      (? (self.main.read_exact "&mut buf"))
			      (? (out.write_main &buf)))))
			(setf self.next (? (Self--read_entry "&mut self.contents")))
			(return (Ok "()"))))))))

  (define-module
      `(merge
	(do0
	 (use (std fs (curly self File))
	      (std io (curly self BufWriter))
	      (std mem)
	      (std path (curly Path PathBuf))
	      (tmp TmpDir)
	      (read IndexFileReader)
	      (write IndexFileWriter))
	 
	 (space pub
		(defstruct0 FileMerge
		    (output_dir PathBuf)
		  (tmp_dir TmpDir)
		  (stacks Vec<Vec<PathBuf>>)))

	 "const NSTREAMS: usize = 12;"
	 "const MERGED_FILENAME: &'static str =  \"index.dat\";"
	 
	 	 
	 (impl FileMerge
	       (space pub
		      (defun new ("output_dir: &Path")
			(declare (values FileMerge))
			(return (make-instance
				 FileMerge
				 :output_dir (output_dir.to_owned)
				 :tmp_dir (TmpDir--new (output_dir.to_owned))
				 :stacks "vec![]"))))
	       
	       
	       (space pub
		      (defun add_file ("&mut self"
				       "mut file: PathBuf")
			(declare (values "io--Result<()>"))
			(let* ((level 0))
			  (loop
			     (when (== level
				       (self.stacks.len))
			       (self.stacks.push "vec![]"))
			     (dot self
				  (aref stacks level)
				  (push file))
			     (when (< (dot self
					   (aref stacks level)
					   (len))
				      NSTREAMS)
			       break)
			     (let (((values filename out) (? (self.temp_dir.create)))
				   (to_merge "vec![]"))
			       (declare (mutable to_merge))
			       (mem--swap "&mut self.stacks[level]"
					  "&mut to_merge")
			       (? (merge_streams to_merge out))
			       (setf file filename)
			       (incf level)))
			  (return (Ok "()")))))
	       (space pub
		      (defun finish ("mut self")
			(declare (values "io::Result<()>"))
			(let* ((tmp (Vec--with_capacity NSTREAMS)))
			  (for (stack self.stacks)
			       (for (file (dot stack
					       (into_iter)
					       (rev)))
				    (tmp.push file)
				    (when (== (tmp.len)
					      NSTREAMS)
				      (? (merge_reversed "&mut tmp"
							 "&mut self.tmp_dir")))))
			  (when (< 1 (tmp.len))
			    (? (merge_reversed "&mut tmp"
					       "&mut self.tmp_dir")))
			  (assert! (<= (tmp.len) 1))
			  
			  (return
			    (case (tmp.pop)
			     ((Some last_file)
			      (fs--rename last_file
					  (self.output_dir.join MERGED_FILENAME)))
			     (None
			      (Err (io--Error--new
				    io--ErrorKind--Other
				    (string "no documents were parsed")))))))))
	       )

	 (defun merge_streams ("files: Vec<PathBuf>"
			       "out: BufWriter<File>")
	   (declare (values "io::Result<()>"))
	   (let* ((streams (? (dot files
				   (into_iter)
				   (map IndexFileReader--open_and_delete)
				   (collect--<io--Result<_>>))))
		  (output (? (IndexFileWriter--new out)))
		  (point 0)
		  (count (dot streams
			      (iter)
			      (filter (lambda (s)
					(return (dot s
						     (peak)
						     (is_some)))))
			      (count))))
	     (declare (type u64 point)
		      (type Vec<IndexFileReader> streams))
	     (while (< 0 count)
	       (let* ((term None)
		      (nbytes 0)
		      (df 0))
		 (for (s &streams)
		      (case (s.peek)
			(None (return "{}"))
			((Some entry) (if (or (term.is_none)
					      (< entry.term
						 (deref (dot term
							     (as_ref)
							     (unwrap)))))
					  (do0
					   (setf term (Some (entry.term.clone))
						 nbytes entry.nbytes
						 df entry.df))
					  (if (== entry.term
						  (deref (dot term
							     (as_ref)
							     (unwrap))))
					      (do0
					       (incf nbytes entry.nbytes)
					       (incf df entry.df)))))))
		 (let ((term (term.expect (string "bug in algorithm!"))))
		   (for (s "&mut streams")
			(when (s.is_at &term)
			  (? (s.move_entry_to "&mut output"))
			  (when (dot s
				     (peek)
				     (is_none))
			    (decf count))))
		   (output.write_contents_entry term df point (coerce nbytes u64))
		   (incf point (coerce nbytes u64)))))
	     (assert! (dot streams
			   (iter)
			   (all (lambda (s) (return (dot s
							 (peek)
							 (is_none)))))))
	     (return (output.finish))))
	 (defun merge_reversed ("filenames: &mut Vec<PathBuf>"
				"tmp_dir: &mut TmpDir")
	   (declare (values "io::Result<()>"))
	   (filenames.reverse)
	   (let (((values merged_filename out) (? (tmp_dir.create))))
	     (let* ((to_merge (Vec--with_capacity NSTREAMS)))
	       (mem--swap filenames "&mut to_merge")
	       (? (merge_streams to_merge out))
	       (filenames.push merged_filename)
	       (return (Ok "()"))))))))



  
  (define-module
      `(main
	(do0
	 
	 
	 "extern crate argparse;"
	 "extern crate byteorder;"

	 (mod index read write  tmp)
	

	 (use
	  (std error Error)
	  (std fs File)
	  (std io)
	  (std io prelude *)
	  (std path (curly Path PathBuf))
	  (std sync mpsc (curly channel Receiver))
	  (std thread (curly spawn JoinHandle))

	  (argparse (curly ArgumentParser
			   ;StoreTrue
			   Collect))
	  
	  (self index InMemoryIndex)
	  (self write write_index_to_tmp_file)
	  (self tmp TmpDir)
	  )
	 
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

	 #+nil
	 (do0
	  ;; receiver will block
	  ;; loop exits when channel is empty and sender has been dropped
	  (for (text receiver)
	       
	       ))


	 (defun start_in_memory_merge_thread ("file_indexes: Receiver<InMemoryIndex>")
	    (declare (values "Receiver<InMemoryIndex>"
			     "JoinHandle<()>"))
	    (let (((values sender receiver) (channel))
		  (handle (spawn
			   (space move
				  (lambda ()
				    (let* ((accumulated_index ("InMemoryIndex::new")))
				      (for (fi file_indexes)
					   (accumulated_index.merge fi)
					   (when (accumulated_index.is_large)
					     (when (dot sender
							(send accumulated_index)
							(is_err))
					       (return))
					     (setf accumulated_index ("InMemoryIndex::new"))))
				      (unless (accumulated_index.is_empty)
					(let ((_ (sender.send accumulated_index)))))))))))
	      (return (values receiver handle))))

	 
	 (defun start_index_writer_thread ("big_indexes: Receiver<InMemoryIndex>"
					    "output_dir: &Path")
	    (declare (values "Receiver<PathBuf>"
			     "JoinHandle<io::Result<()>>"))
	    (let (((values sender receiver) (channel)))
	      (let* ((tmp_dir ("TmpDir::new" output_dir)))
		(let ((handle (spawn (space move
					    (lambda ()
					      (for (index big_indexes)
						   (let ((file (? (write_index_to_tmp_file index "&mut tmp_dir"))))
						     (when (dot sender
								(send file)
								(is_err))
						       break)))
					      (return (Ok "()")))))))
		  (return (values receiver handle))))))
	 
	 #+nil (do0
		

		

	  (defun merge_index_files ("files: Receiver<PathBuf>"
				    "output_dir: &Path")
	    (declare (values "io::Result<()>"))
	    ;; produces single output file on disk
	    ))

	 (defun run_pipeline ("documents: Vec<PathBuf>"
			      "output_dir: PathBuf")
	   (declare (values "io::Result<()>"))
	   (let (((paren texts h1) (start_file_reader_thread documents))
		 ((paren pints h2) (start_file_indexing_thread texts))
		 ((paren gallons h3) (start_in_memory_merge_thread pints))
		 ((paren files h4) (start_index_writer_thread gallons &output_dir))
		 ;(result (merge_index_files files &output_dir))
		 (r1 (dot h1
			  (join)
			  (unwrap))))
	     #+nil (do0
	      ;; h2 and h3 can't fail (pure in-memory)
	      (dot h2 (join) (unwrap))
	      (dot h3 (join) (unwrap))
	      (let ((r4 (dot h4 (join) (unwrap)))))
	      ;; return first error encountered
	      (? r1)
	      (? r4))
	     (return (Ok "()") ;result
	       )))

	 (defun expand_filename_arguments ("args: Vec<String>")
	   (declare (values "io::Result<Vec<PathBuf>>"))
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
	   (declare (values "io::Result<()>"))
	   (let ((output_dir ("PathBuf::from" (string ".")))
		 (documents (? (expand_filename_arguments filenames))))
	     (return (run_pipeline documents output_dir))))
	 
	 (defun main ()
	   (let* ((filenames "vec![]"))

	     (progn
	       (let* ((ap ("ArgumentParser::new")))
		 (ap.set_description (string "make inverted index for searching documents"))
		 (dot ap
		      (refer "&mut filenames")
		      (add_argument (string "filenames") Collect
				    (string "files/directories to index")))
		 (ap.parse_args_or_exit)))
	     
	     (case (run filenames)
	       ((Ok "()") (return "{}"))
	       ((Err err) (println! (string "error: {:?}")
				    (err.description)))))))))


  (loop for e in (reverse *module*) and i from 0 do
	   (destructuring-bind (&key name code) e
	     (write-source (asdf:system-relative-pathname 'cl-rust-generator
							  (merge-pathnames (format nil "~a.rs" name)
									   *source-dir*))
			   `(do0
			     "#[allow(unused_parens)]"
			     ,code)))))
