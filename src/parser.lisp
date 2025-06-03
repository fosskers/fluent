;;; Imagined usage:
;;;
;;;    > (render ctx "hello-user" :userName "Colin")
;;;    ;; => "Hello, Colin!"
;;;
;;; where the `ctx' is the entire parsed localisation context, initialized to a
;;; specific language.

(in-package :fluent)

;; --- Types --- ;;

#+nil
(deftype entry ()
  '(or string list))

(defstruct variable
  "An external value that we expect the user to provide at runtime."
  (name nil :type keyword))

(defstruct term
  "A reference to another static message in the localisation."
  (name nil :type string))

;; --- Static Parsers --- ;;

(defparameter +comment+        (*> (p:char #\#) (p:consume (lambda (c) (not (eql c #\newline))))))
(defparameter +skip-space+     (p:consume (lambda (c) (equal c #\space))))
(defparameter +skip-all-space+ (p:consume #'p:space?))
(defparameter +skip-comments+  (p:skip (*> +comment+ +skip-all-space+)))
(defparameter +skip-junk+      (*> +skip-all-space+ +skip-comments+))
(defparameter +equal+          (p:char #\=))
(defparameter +brace-open+     (p:char #\{))
(defparameter +brace-close+    (p:char #\}))
(defparameter +dollar+         (p:char #\$))
(defparameter +space+          (p:char #\space))

#+nil
(funcall +comment+ (p:in "# hello"))
#+nil
(funcall +skip-junk+ (p:in "# hello   "))

;; --- Entry --- ;;

(defun parse (s)
  "Parse a given string into a Hash Table of localisations."
  (let ((pairs (p:parse (*> +skip-junk+
                            (<* (p:sep-end1 +skip-junk+ #'pair)
                                #'p:eof))
                        s))
        (ht (make-hash-table :test #'equal :size 64)))
    (dolist (pair pairs)
      (setf (gethash (car pair) ht) (cadr pair)))
    ht))

#+nil
(parse "language-name = English")
#+nil
(parse (uiop:read-file-string #p"tests/data/basic.ftl"))

;; --- Parsers --- ;;

;; NOTE: "The syntax should allow good error recovery: an error in one message
;; should not break the whole file. The parser should resume normal parsing as
;; soon as possible and with as few losses as possible."
(defun pair (offset)
  "A single localisation pair."
  (funcall (<*> (<* (p:take-while1 (lambda (c) (not (eql c #\space))))
                    +skip-space+
                    +equal+
                    +skip-all-space+)
                #'entry)
           offset))

;; NOTE: No support for smart detection of differed indenting from line to line.
;; All leading whitespace is stripped.
(defun entry (offset)
  "Many lines."
  (p:fmap (lambda (lists) (apply #'append lists))
          (funcall (p:sep-end1 (*> #'p:newline +space+ +skip-space+) #'line)
                   offset)))

#+nil
(entry (p:in "Failed to edit: { $file }!"))

(defun line (offset)
  "A single line of a potentially multiline text group."
  (funcall (p:many1 (p:alt #'variable
                           #'term
                           (p:take-while1 (lambda (c)
                                            (not (or (eql c #\newline)
                                                     (eql c #\{)))))))
           offset))

#+nil
(line (p:in "Failed to edit: { $file }!"))

#+nil
(line (p:in "Placing a { term } here!"))

(defun variable (offset)
  "Parse a variable chunk."
  (p:fmap (lambda (s) (make-variable :name (string->keyword s)))
          (funcall (*> +brace-open+
                       +skip-space+
                       +dollar+
                       (<* (p:take-while1 (lambda (c)
                                            (not (or (eql c #\space)
                                                     (eql c #\})))))
                           +skip-space+
                           +brace-close+))
                   offset)))

(defun term (offset)
  "Parse a single, swappable term."
  (p:fmap (lambda (s) (make-term :name s))
          (funcall (*> +brace-open+
                       +skip-space+
                       (<* (p:take-while1 (lambda (c)
                                            (not (or (eql c #\space)
                                                     (eql c #\})))))
                           +skip-space+
                           +brace-close+))
                   offset)))
