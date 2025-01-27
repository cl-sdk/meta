(in-package :cl-user)

(asdf:defsystem #:meta-definitions
  :license "Unlicense"
  :author "Bruno Dias"
  :serial t
  :depends-on (#:closer-mop #:alexandria #:serapeum #:uiop)
  :components ((:file "package")))
