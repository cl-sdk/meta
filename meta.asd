(in-package :cl-user)

(asdf:defsystem #:meta
  :license "Unlicense"
  :author "Bruno Dias"
  :serial t
  :depends-on (:alexandria :serapeum :uiop)
  :components ((:file "package")))
