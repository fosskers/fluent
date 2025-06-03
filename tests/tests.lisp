(defpackage fluent/tests
  (:use :cl :parachute)
  (:local-nicknames (#:f #:fluent)
                    (#:p #:parcom)))

(in-package :fluent/tests)

(define-test parsing)

(define-test parsing-placeables
  :parent parsing
  (is equalp (f::make-variable :name :foo)
      (p:parse #'f::variable "{ $foo }"))
  (is equalp (f::make-variable :name :foo)
      (p:parse #'f::variable "{$foo}"))
  (is equalp (f::make-term :name "foo")
      (p:parse #'f::term "{ foo }"))
  (is equalp (f::make-term :name "foo")
      (p:parse #'f::term "{foo}")))

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
