(asdf:defsystem #:meta.tests
  :author "Bruno Dias"
  :serial t
  :depends-on (#:closer-mop #:meta #:fiveam)
  :components ((:file "tests")))
