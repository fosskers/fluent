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
      (p:parse #'f::term "{foo}"))
  (is equal "}" (p:parse #'f::quoted "{\"}\"}"))
  (finish (p:parse #'f::placeable "{ NUMBER($ratio, minimumFractionDigits: 2) }")))

(define-test parsing-plaintext
  :parent parsing
  (is equal '("this is a" "multiline string") (p:parse #'f::entry "this is a
  multiline string"))
  (is equal '("this has" "more than" "two lines") (p:parse #'f::entry "this has
  more than
  two lines")))

#+nil
(define-test parsing-ideally-passes-but-doesnt
  :parent parsing
  ;; By the spec, that extra newline in between should be preserved.
  (fail (p:parse #'f::entry "this has
  an extra

  blank line!")))

(define-test parsing-pairs
  :parent parsing
  (finish (p:parse (p:<* #'f::pair #'p:eof) "dpi-ratio = Your DPI ratio is { NUMBER($ratio, minimumFractionDigits: 2)}")))

(define-test parsing-files
  :parent parsing
  (let ((s (uiop:read-file-string #p"tests/data/basic.ftl")))
    (finish (f:parse s)))
  (let ((s (uiop:read-file-string #p"tests/data/aura.ftl")))
    (finish (f:parse s))))

(define-test parsing-minor-things
  :parent parsing
  (is eq :photocount (p:parse #'f::dollared "$photoCount"))
  (is eq :other (p:parse #'f::category "other"))
  (is eq :few (p:parse #'f::branch-selection-term "[few]"))
  (is equal "male" (p:parse #'f::branch-selection-term "[male]"))
  (is = 1 (p:parse #'f::branch-selection-term "[1]"))
  (is = 1.0 (p:parse #'f::branch-selection-term "[1.0]")))

(define-test parsing-selections
  :parent parsing
  (let* ((def (f::make-branch :term :other
                              :line (list "added " (f::make-variable :name :photocount) " new photos")
                              :default t))
         (sel (f::make-selection :input :photocount
                                 :func nil
                                 :branches (list (f::make-branch :term :one
                                                                 :line (list "added a new photo")
                                                                 :default nil)
                                                 def)
                                 :default def)))
    (is equalp sel (p:parse #'f::selection "{$photoCount ->
  [one] added a new photo
 *[other] added {$photoCount} new photos
}"))))

(define-test functions)

(define-test number
  :parent functions
  (is equal "1" (f::resolve-number (f::make-numberf :input :foo :min-frac 0) 1))
  (is equal "1.0" (f::resolve-number (f::make-numberf :input :foo :min-frac 0) 1.0))
  (is equal "1.123" (f::resolve-number (f::make-numberf :input :foo :min-frac 0) 1.123))
  (is equal "1.000" (f::resolve-number (f::make-numberf :input :foo :min-frac 3) 1))
  (is equal "1.000" (f::resolve-number (f::make-numberf :input :foo :min-frac 3) 1.0))
  (is equal "1.123" (f::resolve-number (f::make-numberf :input :foo :min-frac 2) 1.123))
  (is equal "1" (f::resolve-number (f::make-numberf :input :foo :max-frac 0) 1))
  (is equal "1" (f::resolve-number (f::make-numberf :input :foo :max-frac 0) 1.0))
  (is equal "1.0" (f::resolve-number (f::make-numberf :input :foo :max-frac 1) 1.02))
  (is equal "1.02" (f::resolve-number (f::make-numberf :input :foo :max-frac 2) 1.02))
  (is equal "1.02" (f::resolve-number (f::make-numberf :input :foo :max-frac 2) 1.023)))
