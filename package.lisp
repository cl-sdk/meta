(defpackage #:meta
  (:use #:cl)
  (:export
   #:declarations
   #:define-class
   #:derive-readers
   #:derive-print-object
   #:derive-equality
   #:define-from
   #:define))

(in-package :meta)

(defvar declarations
  (make-hash-table :test 'equal))

(defvar default-class-slots
  '(;; value and doc
    :initarg :initform :documentation
    ;; visibility
    :writer :reader :accessor
    ;; type
    :type
    ;; allocation
    :allocation :location))

(defmacro define-class (class-name)
  (let ((spec (gethash (symbol-name class-name) declarations)))
    (labels ((make-slot (class-name slot)
	       (destructuring-bind (name &rest attrs)
		   slot
		 (let ((attributes (copy-list attrs)))
		   (when (member :required attributes)
		     (remf attributes :initform)
		     (setf attributes
			   (append attributes
				   `(:initform (error (concatenate
						       'string
						       ,(symbol-name class-name)
						       "."
						       ,(symbol-name name)
						       " must be defined"))))))
		   (loop
		     for (key value) on attributes by #'cddr
		     when (not (member key default-class-slots))
		       do (remf attributes key))
		   `(,name
		     :initarg ,(alexandria:make-keyword
				(string-upcase (symbol-name name)))
		     ,@attributes)))))
      (destructuring-bind (class-spec slot-spec)
	  spec
	(let ((slots (mapcar (lambda (slot)
			       (make-slot (car class-spec) slot))
			     (cdr slot-spec))))
	  `(progn
	     (defclass ,(caadr class-spec) ()
	       ,slots)))))))

(defmacro derive-readers (class-name)
  (let ((spec (gethash (symbol-name class-name) declarations)))
    (destructuring-bind (class-spec slots-spec)
	spec
      (declare (ignore class-spec))
      (let ((slots (cdr slots-spec)))
	`(progn
	   ,@(mapcar (lambda (slot)
		       (destructuring-bind (slot-name &rest attrs)
			   slot
			 (declare (ignore attrs))
			 (let* ((sym-name (string-upcase
					   (concatenate
					    'string
					    (symbol-name class-name)
					    "-"
					    (symbol-name slot-name))))
				(reader-name (intern sym-name)))
			   `(progn
			      (defun ,reader-name (obj)
				(slot-value obj ',slot-name))
			      (export ',reader-name)))))
		     slots))))))

(defmacro derive-print-object (class-name)
  "Defines a print-object method for the given class."
  (let ((spec (gethash (symbol-name class-name) declarations)))
    (destructuring-bind (class-spec slots-spec)
	spec
      (let* ((slots (mapcar #'car (cdr slots-spec))))
	`(defmethod print-object ((object ,class-name) stream)
	   (format stream "#<~A~%" ',(caadr class-spec))
	   (dolist (slot ',slots)
	     (format stream "  ~A: ~A~%" slot (slot-value object slot)))
	   (format stream ">"))))))

(defmacro derive-equality (class-name)
  (let ((spec (gethash (symbol-name class-name) declarations)))
    (destructuring-bind (class-spec slots-spec)
	spec
      (declare (ignore class-spec))
      (let* ((slots (cdr slots-spec))
	     (check-pairs (mapcar (lambda (slot)
				    (let* ((slot-name (car slot))
					   (attrs (cdr slot))
					   (slot-type (getf attrs :type))
					   (is-nullable (member 'null (uiop:ensure-list slot-type)))
					   (eq-fn (getf attrs :equality-fn))
					   (slot-eq (if eq-fn eq-fn ''equal))
					   (base `(funcall ,slot-eq
							   (slot-value a ',slot-name)
							   (slot-value b ',slot-name))))
				      (if is-nullable
					  `(if (and (not (equal (slot-value a ',slot-name) :null))
						    (not (equal (slot-value b ',slot-name) :null)))
					       ,base
					       (funcall 'equal
							(slot-value a ',slot-name)
							(slot-value b ',slot-name)))
					  base)))
				  slots))
	     (eq-name (intern (string-upcase (concatenate 'string (symbol-name class-name) "=")))))
	`(progn
	   (defun ,eq-name (a b)
	     (and ,@check-pairs))
	   (export ',eq-name))))))

(defmacro define-from (class-name name selected-slots &optional extra-slots)
  (let ((spec (gethash (symbol-name class-name) declarations)))
    (destructuring-bind (class-spec slots-spec)
	spec
      (declare (ignore class-spec))
      (let ((slots (serapeum:~>
		    (cdr slots-spec)
		    (remove-if (lambda (slot)
				 (not (member (car slot) selected-slots)))
			       _))))
	`(progn
	   (define
	     (class ,(uiop:ensure-list name))
	     (slots ,@slots ,@extra-slots)))))))

(defmacro define (&body spec)
  `(progn
     (setf (gethash ,(symbol-name (caadar spec)) declarations) ',spec)
     t))
