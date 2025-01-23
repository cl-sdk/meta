(defpackage #:meta.tests
  (:use #:cl #:meta #:fiveam))

(in-package :meta.tests)

(def-suite* meta.tests)

(eval-when (:compile-toplevel :load-toplevel)
  (define
    (class (class-a))
    (slots))
  ;; TODO(dias): check if we force finalize a class when defining it.
  (define-class class-a)
  (closer-mop:ensure-finalized (find-class 'class-a)))

(def-test class-a-must-exist-after-defining-it ()
  (is (not (null (find-class 'class-a)))))

(def-test define-a-class-without-slots ()
  (is (= 0 (length (closer-mop:class-direct-slots (find-class 'class-a))))))

(eval-when (:compile-toplevel :load-toplevel)
  (define
    (class (class-b))
    (slots (id :type integer)))
  (define-class class-b)
  (closer-mop:ensure-finalized (find-class 'class-b)))

(def-test define-a-class-with-one-slot ()
  (is (= 1 (length (closer-mop:class-direct-slots (find-class 'class-b))))))

(eval-when (:compile-toplevel :load-toplevel)
  (define
    (class (class-c))
    (slots (id :type integer :required t)))
  (define-class class-c)
  (closer-mop:ensure-finalized (find-class 'class-c)))

(def-test add-initform-with-error-if-slot-has-attribute-required ()
  (is (not (null (closer-mop:slot-definition-initform (car (closer-mop:class-slots (find-class 'class-c))))))))

(eval-when (:compile-toplevel :load-toplevel)
  (define
    (class (class-d))
    (slots (id :type integer)))
  (define-class class-d)
  (closer-mop:ensure-finalized (find-class 'class-d)))

(def-test must-not-have-initform-if-not-required-or-not-defined ()
  (is (null (closer-mop:slot-definition-initform (car (closer-mop:class-slots (find-class 'class-d)))))))
