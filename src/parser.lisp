;;; Imagined usage:
;;;
;;;    > (render ctx "hello-user" :userName "Colin")
;;;    ;; => "Hello, Colin!"
;;;
;;; where the `ctx' is the entire parsed localisation context, initialized to a
;;; specific language.
;;;
;;; Out of Scope:
;;;   - Handling country-based number formatting.

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

(defstruct selection
  "Branching possibilities of a localisation depending on some input value."
  (input    nil :type keyword)
  (func     nil :type (or null function))
  (branches nil :type list)
  (default  nil :type branch))

(defstruct branch
  "A particular branch of a selection block."
  (term nil :type (or real string plurals:category))
  (line nil :type list))

;; TODO: 2025-06-12 Move this.
(defun resolve-branch (branch val)
  "Replace a placeholder in a branch with its actual value."
  (reduce (lambda (chunk acc)
            (etypecase chunk
              (string (cons chunk acc))
              ;; NOTE: Might also need to check against the variable's keyword.
              (variable (cons val acc))))
          (branch-line branch)
          :initial-value '()
          :from-end t))

#+nil
(resolve-branch (make-branch :term :other
                             :line (list "added" (make-variable :name :photocount) "new photos"))
                5)

;; TODO: 2025-06-12 Move this.
(defun resolve-selection (locale sel val)
  (resolve-branch (find-branch locale sel val) val))

(defun find-branch (locale sel val)
  (let ((found (etypecase val
                 (real (let* ((s   (format nil "~a" val))
                              (cat (plurals:cardinal locale s)))
                         (find-if (lambda (branch)
                                    (let ((term (branch-term branch)))
                                      (etypecase term
                                        (real (= val term))
                                        (plurals:category (eq cat term)))))
                                  (selection-branches sel))))
                 (string (find-if (lambda (branch) (equal val (branch-term branch)))
                                  (selection-branches sel))))))
    (cond ((not found) (selection-default sel))
          (t found))))

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
(defparameter +quote+          (p:char #\"))

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
  (funcall (p:many1 (p:alt #'placeable
                           (p:take-while1 (lambda (c)
                                            (not (or (eql c #\newline)
                                                     (eql c #\{)))))))
           offset))

#+nil
(line (p:in "Failed to edit: { $file }!"))

#+nil
(line (p:in "Placing a { term } here!"))

(defun placeable (offset)
  "Something within curly braces."
  (funcall (p:alt #'variable #'quoted #'term) offset))

#+nil
(placeable (p:in "{ $foo }"))

(defun variable (offset)
  "Parse a variable chunk."
  (p:fmap (lambda (s) (make-variable :name (string->keyword s)))
          (funcall (p:between (*> +brace-open+ +skip-space+ +dollar+)
                              (p:take-while1 (lambda (c)
                                               (not (or (eql c #\space)
                                                        (eql c #\})))))
                              (*> +skip-space+ +brace-close+))
                   offset)))

(defun term (offset)
  "Parse a single, swappable term."
  (p:fmap (lambda (s) (make-term :name s))
          (funcall (p:between (*> +brace-open+ +skip-space+)
                              (p:take-while1 (lambda (c)
                                               (not (or (eql c #\space)
                                                        (eql c #\})))))
                              (*> +skip-space+ +brace-close+))
                   offset)))

;; NOTE: Unicode escaping is not supported. Just use the raw character itself.
(defun quoted (offset)
  "Parse special, quoted characters."
  (funcall (p:between (*> +brace-open+ +skip-space+)
                      (p:between +quote+
                                 (p:take-while1 (lambda (c)
                                                  (not (or (eql c #\")
                                                           (eql c #\newline)))))
                                 +quote+)
                      (*> +skip-space+ +brace-close+))
           offset))

#+nil
(quoted (p:in "{ \"}\" }"))
