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
  (term nil    :type (or real string plurals:category))
  (line nil    :type list)
  (default nil :type boolean))

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
  "Choose the correct localisation line."
  (resolve-branch (find-branch locale sel val) val))

;; TODO: 2025-06-12 Eventually I will have to handle functions that manipulate
;; the input before comparing.
(defun find-branch (locale sel val)
  "Find a localisation branch whose condition/term matches the incoming value."
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

#+nil
(let ((def (make-branch :term :other
                        :line (list "added" (make-variable :name :photocount) "new photos"))))
  (resolve-selection
   :en
   (make-selection :input :photocount
                   :func nil
                   :branches (list (make-branch :term :one
                                                :line (list "added a new photo"))
                                   def)
                   :default def)
   1))

;; --- Static Parsers --- ;;

(defparameter +comment+        (*> (p:char #\#) (p:consume (lambda (c) (not (eql c #\newline))))))
(defparameter +skip-space+     (p:consume (lambda (c) (equal c #\space))))
(defparameter +skip-all-space+ (p:consume #'p:space?))
(defparameter +skip-comments+  (p:skip (*> +comment+ +skip-all-space+)))
(defparameter +skip-junk+      (*> +skip-all-space+ +skip-comments+))
(defparameter +equal+          (p:char #\=))
(defparameter +brace-open+     (p:char #\{))
(defparameter +brace-close+    (p:char #\}))
(defparameter +bracket-open+   (p:char #\[))
(defparameter +bracket-close+  (p:char #\]))
(defparameter +dollar+         (p:char #\$))
(defparameter +space+          (p:char #\space))
(defparameter +quote+          (p:char #\"))
(defparameter +asterisk+       (p:char #\*))

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
  (funcall (p:alt #'variable #'quoted #'term #'selection) offset))

#+nil
(placeable (p:in "{ $foo }"))

(defun variable (offset)
  "Parse a variable chunk."
  (p:fmap (lambda (s) (make-variable :name (string->keyword s)))
          (funcall (p:between (*> +brace-open+ +skip-space+)
                              #'dollared
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

(defun dollared (offset)
  "Parse a variable name as a keyword."
  (p:fmap #'string->keyword
          (funcall (*> +dollar+
                       (p:take-while1 (lambda (c)
                                        (not (or (eql c #\space)
                                                 (eql c #\newline)
                                                 (eql c #\}))))))
                   offset)))

#+nil
(p:parse #'dollared "$photoCount")

(defun selection (offset)
  "Parse a multi-condition selection block."
  (p:fmap (lambda (list)
            (destructuring-bind (var branches) list
              (let ((default (find-if #'branch-default branches)))
                (make-selection :input var
                                :func nil
                                :branches branches
                                :default default))))
          (funcall (p:between (*> +brace-open+ +skip-space+)
                              (<*> #'dollared
                                   (*> +skip-space+
                                       (p:string "->")
                                       +skip-all-space+
                                       (p:sep-end1 +skip-all-space+ #'branch)))
                              (*> +skip-all-space+ +brace-close+))
                   offset)))

#+nil
(p:parse #'selection "{$photoCount ->
  [one] added a new photo
 *[other] added {$photoCount} new photos
}")

(defun branch (offset)
  "Parse a single localisation choice."
  (p:fmap (lambda (list)
            (destructuring-bind (default selector line) list
              (make-branch :term selector
                           :line line
                           :default default)))
          (funcall (<*> (p:opt (<$ t +asterisk+))
                        #'branch-selection-term
                        (*> +skip-space+ #'line))
                   offset)))

#+nil
(branch (p:in "[male] his stream"))
#+nil
(branch (p:in "*[other] added {$photoCount} new photos"))

(defun branch-selection-term (offset)
  "Parse a value that can appear between []."
  (funcall (p:between +bracket-open+
                      (p:alt (<* #'p:unsigned (p:sneak #\]))
                             #'p:float
                             #'category
                             (p:take-while1 (lambda (c)
                                              (not (or (eql c #\])
                                                       (eql c #\newline))))))
                      +bracket-close+)
           offset))

#+nil
(branch-selection-term (p:in "[male] his stream"))
#+nil
(branch-selection-term (p:in "[one] added a new photo"))

(defun category (offset)
  (funcall (p:alt (<$ :zero  (p:string "zero"))
                  (<$ :one   (p:string "one"))
                  (<$ :two   (p:string "two"))
                  (<$ :few   (p:string "few"))
                  (<$ :many  (p:string "many"))
                  (<$ :other (p:string "other")))
           offset))

#+nil
(category (p:in "few"))
