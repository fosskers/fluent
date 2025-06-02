(defpackage fluent/tests
  (:use :cl :parachute)
  (:local-nicknames (#:f #:fluent)))

(in-package :fluent/tests)

(define-test parsing)

(define-test parsing-files
  :parent parsing
  (let ((s (uiop:read-file-string #p"tests/data/basic.ftl")))
    (finish (f:parse s)))
  (let ((s (uiop:read-file-string #p"tests/data/aura.ftl")))
    (finish (f:parse s))))
