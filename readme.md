# meta

Meta is library to create infinite types and derivations.

## example

```lisp
(meta:define
  (class (user :table-name "user"))
  (slots (id :json-key "id"
             :table-column "id"
             :validation assert-id
             :validation-error-message "invalid owner id")
         (name :json-key "name"
               :table-column "name"
               :validation assert-name
               :validation-error-message "invalid title")
         (created-at :initform (niav.time:now)
                     :type local-time:timestamp
                     :equality #'local-time:timestamp=
                     :json-key "created_at"
                     :table-column "created_at")
         (updated-at :initform (local-time:now)
                     :type local-time:timestamp
                     :equality #'local-time:timestamp=
                     :json-key "updated_at"
                     :table-column "updated_at")
         (deleted-at :type (or local-time:timestamp null)
                     :equality #'local-time:timestamp=
                     :json-key "deleted_at"
                     :table-column "deleted_at")))

(meta:define-class user)

;; (defclass user ()
;;   ((id :type id)
;;    (name :type string)
;;    (created-at :initform (niav.time:now)
;;                :type local-time:timestamp)
;;    (updated-at :initform (local-time:now)
;;                :type local-time:timestamp)
;;    (deleted-at :type (or local-time:timestamp null))))

(meta:derive-readers user)
(meta:derive-equality user)
(meta:derive-print-object user)

;; create a input type create user 

(meta:define-from user create-user (name))

(meta:define-class create-user)

;; (defclass create-user ()
;;   ((name :type string)))

(meta:derive-readers create-user)
(meta:derive-print-object create-user)
(derive-validation create-user)
```

## license

Unlicense
