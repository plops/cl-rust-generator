(asdf:defsystem cl-rust-generator
  :version "0.2.0"
  :description "Emit Rust Language code via AST registry"
  :maintainer "Martin Kielhorn <kielhorn.martin@gmail.com>"
  :author "Martin Kielhorn <kielhorn.martin@gmail.com>"
  :licence "GPL"
  :depends-on ("alexandria")
  :serial t
  :components ((:file "src/package")
               (:file "src/vars")
               (:file "src/utils")
               (:file "src/environment")
               (:file "src/registry")
               (:file "src/parsers")
               (:file "src/emitters")
               (:file "src/core")))
