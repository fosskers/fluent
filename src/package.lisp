(defpackage fluent
  (:use :cl)
  (:shadow #:variable #:number)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom))
  (:export #:parse #:resolve)
  (:documentation "Software localisation via Mozilla's Project Fluent."))

(in-package :fluent)

(defun string->keyword (s)
  (intern (string-upcase s) "KEYWORD"))

#+nil
(string->keyword "hello")

(define-condition missing-input (error)
  ((expected :initarg :expected :reader missing-input-expected))
  (:documentation "A certain arg was expected for a localisation line, but it wasn't given.")
  (:report (lambda (c stream)
             (format stream "Missing localisation argument: ~a" (missing-input-expected c)))))

(defun get-input (col k)
  "Extra error handling around a `getf' call."
  (let ((v (getf col k)))
    (cond (v v)
          (t (error 'missing-input :expected k)))))
