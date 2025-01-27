(asdf:defsystem #:meta-definitions.tests
  :author "Bruno Dias"
  :serial t
  :depends-on (#:closer-mop #:meta #:fiveam #:meta-definitions)
  :components ((:file "tests")))
