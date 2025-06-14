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

(defstruct numberf
  "The NUMBER function."
  (input    nil :type keyword)
  (min-frac nil :type (or null fixnum))
  (max-frac nil :type (or null fixnum)))

(declaim (ftype (function (numberf real) string) resolve-number))
(defun resolve-number (f n)
  "Evaluate a NUMBER function."
  (let ((min (numberf-min-frac f))
        (max (numberf-max-frac f)))
    (cond (min (cond ((zerop min) (format nil "~a" n))
                     (t (multiple-value-bind (_ rem) (floor n)
                          (declare (ignore _))
                          (multiple-value-bind (_ rem) (floor (* rem 10 min))
                            (declare (ignore _))
                            (cond ((> rem 0) (format nil "~f" n))
                                  (t (format nil "~,vf" min n))))))))
          (max (multiple-value-bind (int rem) (floor n)
                 (cond ((zerop max) (format nil "~d" int))
                       (t (multiple-value-bind (_ rem) (floor (* rem 10 max))
                            (declare (ignore _))
                            (cond ((> rem 0) (format nil "~,vf" max n))
                                  (t (format nil "~f" n))))))))
          (t (format nil "~f" n)))))

#+nil
(resolve-number (make-numberf :input :foo :max-frac 2) 1.123)

(defstruct selection
  "Branching possibilities of a localisation depending on some input value."
  (input    nil :type (or keyword numberf))
  (branches nil :type list)
  (default  nil :type branch))

(defstruct branch
  "A particular branch of a selection block."
  (term    nil :type (or string plurals:category))
  (line    nil :type list)
  (default nil :type boolean))

;; TODO: 2025-06-12 Move this.
(defun resolve-branch (branch val)
  "Replace a placeholder in a branch with its actual value."
  (format nil "~{~a~}"
          (reduce (lambda (chunk acc)
                    (etypecase chunk
                      (string (cons chunk acc))
                      ;; NOTE: Might also need to check against the variable's keyword.
                      (variable (cons val acc))
                      (numberf (cons (resolve-number chunk val) acc))))
                  (branch-line branch)
                  :initial-value '()
                  :from-end t)))

#+nil
(resolve-branch (make-branch :term :other
                             :line (list "added" (make-variable :name :photocount) "new photos"))
                5)

;; TODO: 2025-06-12 Move this.
(defun resolve-selection (locale sel val)
  "Choose the correct localisation line."
  (resolve-branch (find-branch locale sel val) val))

(defun find-branch (locale sel val)
  "Find a localisation branch whose condition/term matches the incoming value."
  (let ((found (etypecase val
                 (real (let* ((inp (selection-input sel))
                              (s (etypecase inp
                                   (keyword (format nil "~a" val))
                                   (numberf (resolve-number inp val))))
                              (cat (plurals:cardinal locale s)))
                         (find-if (lambda (branch)
                                    (let ((term (branch-term branch)))
                                      (etypecase term
                                        (plurals:category (eq cat term))
                                        (t (equal s term)))))
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
                   :branches (list (make-branch :term :one
                                                :line (list "added a new photo"))
                                   def)
                   :default def)
   1))

#+nil
(let ((sel (p:parse #'selection "{ NUMBER($score, minimumFractionDigits: 1) ->
        [0.0]   You scored zero points. What happened?
       *[other] You scored { NUMBER($score, minimumFractionDigits: 1) } points.
}")))
  (resolve-selection :en sel 1))

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
(defparameter +paren-open+     (p:char #\())
(defparameter +paren-close+    (p:char #\)))
(defparameter +dollar+         (p:char #\$))
(defparameter +space+          (p:char #\space))
(defparameter +quote+          (p:char #\"))
(defparameter +asterisk+       (p:char #\*))
(defparameter +comma+          (p:char #\,))
(defparameter +colon+          (p:char #\:))

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

#+nil
(p:parse #'pair "dpi-ratio = Your DPI ratio is { NUMBER($ratio, minimumFractionDigits: 2)}")

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
  (funcall (p:alt #'variable #'quoted #'term #'funky #'selection) offset))

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
                                                 (eql c #\})
                                                 (eql c #\,))))))
                   offset)))

#+nil
(p:parse #'dollared "$photoCount")

(defun funky (offset)
  "Parse a function call placeable."
  (funcall (p:between (*> +brace-open+ +skip-space+)
                      #'func
                      (*> +skip-space+ +brace-close+))
           offset))

#+nil
(p:parse #'funky "{ NUMBER($ratio, minimumFractionDigits: 2) }")

(defun selection (offset)
  "Parse a multi-condition selection block."
  (p:fmap (lambda (list)
            (destructuring-bind (var branches) list
              (let ((default (find-if #'branch-default branches)))
                (make-selection :input var
                                :branches branches
                                :default default))))
          (funcall (p:between (*> +brace-open+ +skip-space+)
                              (<*> (p:alt #'dollared #'func)
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
                      (p:alt #'category
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

(defun func (offset)
  ;; TODO: 2025-06-13 Eventually add DATETIME.
  (funcall #'number offset))

;; NOTE: 2025-06-13 For me at this moment, supporting all the formatting options
;; for every language is out of scope. I will start with English defaults and
;; add extra support, piecemeal, as localisations are added to downstream
;; programs.
;;
;; Further, at the moment, only certain formatting options are available as
;; match my immediate needs.
(defun number (offset)
  (p:fmap (lambda (list)
            (destructuring-bind (input (op val)) list
              (case op
                (:min-frac (make-numberf :input input :min-frac val))
                (:max-frac (make-numberf :input input :max-frac val)))))
          (funcall (*> (p:string "NUMBER")
                       (p:between +paren-open+
                                  (<*> #'dollared
                                       (p:opt (*> +comma+
                                                  +skip-space+
                                                  #'number-option)))
                                  +paren-close+))
                   offset)))

#+nil
(p:parse #'number "NUMBER($ratio, minimumFractionDigits: 2)")

(defun number-option (offset)
  (funcall (<*> (p:alt (<$ :min-frac (p:string "minimumFractionDigits"))
                       (<$ :max-frac (p:string "maximumFractionDigits")))
                (*> +colon+
                    +skip-space+
                    #'p:unsigned))
           offset))

#+nil
(p:parse #'number-option "minimumFractionDigits: 2")
