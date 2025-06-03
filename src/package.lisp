(defpackage fluent
  (:use :cl)
  (:shadow #:variable)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom))
  (:export #:parse)
  (:documentation "Software localisation via Mozilla's Project Fluent."))

(in-package :fluent)

(defun string->keyword (s)
  (intern (string-upcase s) "KEYWORD"))

#+nil
(string->keyword "hello")
