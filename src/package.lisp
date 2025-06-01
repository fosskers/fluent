(defpackage fluent
  (:use :cl)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom))
  (:documentation "Software localisation via Mozilla's Project Fluent."))

(in-package :fluent)
