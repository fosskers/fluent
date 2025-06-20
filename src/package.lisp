(defpackage fluent
  (:use :cl)
  (:shadow #:variable #:number)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom))
  ;; --- Types --- ;;
  (:export #:fluent #:fluent-locale #:fluent-fallback #:fluent-locs)
  ;; --- Entry --- ;;
  (:export #:parse #:resolve #:resolve-with)
  ;; --- Conditions --- ;;
  (:export #:missing-line #:unknown-locale #:missing-input)
  (:documentation "Software localisation via Mozilla's Project Fluent."))

(in-package :fluent)

(defun string->keyword (s)
  (intern (string-upcase s) "KEYWORD"))

#+nil
(string->keyword "hello")

(define-condition missing-line (error)
  ((line     :initarg :line :reader missing-line-line)
   (locale   :initarg :locale :reader missing-line-locale)
   (fallback :initarg :fallback :reader missing-line-fallback))
  (:documentation "A certain localisation couldn't be found in any language.")
  (:report (lambda (c stream)
             (format stream "The localisation '~a' could not be found.~%Locale: ~a~%Fallback: ~a"
                     (missing-line-line c)
                     (missing-line-locale c)
                     (missing-line-fallback c)))))

(define-condition unknown-locale (error)
  ((locale :initarg :locale :reader unknown-locale-locale))
  (:documentation "The user attempted to resolve via an unknown locale.")
  (:report (lambda (c stream)
             (format stream "Unknown locale: ~a" (unknown-locale-locale c)))))

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
