(asdf:defsystem cl-rust-generator
    :version "0"
    :description "Emit Rust Language code"
    :maintainer " <kielhorn.martin@gmail.com>"
    :author " <kielhorn.martin@gmail.com>"
    :licence "GPL"
    :depends-on ("alexandria")
    :serial t
    :components ((:file "package")
		 (:file "rs")) )
