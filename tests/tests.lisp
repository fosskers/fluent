(defpackage fluent/tests
  (:use :cl :parachute)
  (:local-nicknames (#:f #:fluent)
                    (#:p #:parcom)))

(in-package :fluent/tests)

(define-test parsing)

(define-test parsing-plaintext
  :parent parsing
  (is equal '("this is a" "multiline string") (p:parse #'f::entry "this is a
  multiline string"))
  (is equal '("this has" "more than" "two lines") (p:parse #'f::entry "this has
  more than
  two lines")))

(define-test parsing-files
  :parent parsing
  (let ((s (uiop:read-file-string #p"tests/data/basic.ftl")))
    (finish (f:parse s)))
  (let ((s (uiop:read-file-string #p"tests/data/aura.ftl")))
    (finish (f:parse s))))
