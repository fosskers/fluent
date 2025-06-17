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

;; --- Static Parsers --- ;;

(defparameter +comment+        (*> (p:char #\#) (p:consume (lambda (c) (not (eql c #\newline))))))
(defparameter +skip-space+     (p:consume (lambda (c) (equal c #\space))))
(defparameter +skip-all-space+ (p:consume #'p:space?))
(defparameter +skip-comments+  (p:skip (*> +comment+ +skip-all-space+)))
(defparameter +skip-junk+      (*> +skip-all-space+ +skip-comments+))
(defparameter +equal+          (p:char #\=))
(defparameter +dash+           (p:char #\-))
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

;; --- Entry --- ;;

(defun parse (s)
  "Parse a given string into a collated set of localisations."
  (let ((pairs (p:parse (*> +skip-junk+
                            (<* (p:sep-end1 +skip-junk+ #'pair)
                                #'p:eof))
                        s))
        (terms (make-hash-table :test #'equal :size 16))
        (lines (make-hash-table :test #'equal :size 64)))
    (dolist (pair pairs)
      (destructuring-bind ((type name) line) pair
        (case type
          (:term (setf (gethash name terms) line))
          (:line (setf (gethash name lines) line)))))
    (make-localisations :terms terms :lines lines)))

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
  (funcall (<*> (<* #'key
                    +skip-space+
                    +equal+
                    +skip-all-space+)
                #'entry)
           offset))

#+nil
(p:parse #'pair "dpi-ratio = Your DPI ratio is { NUMBER($ratio, minimumFractionDigits: 2)}")
#+nil
(p:parse #'pair "about = About { -brand-name }.")

(defun key (offset)
  "Either a term key or a normal line key."
  (p:fmap (lambda (list)
            (destructuring-bind (dash? name) list
              (cond (dash? (list :term name))
                    (t (list :line name)))))
          (funcall (<*> (p:opt +dash+)
                        (p:take-while1 (lambda (c) (not (eql c #\space)))))
                   offset)))

#+nil
(p:parse #'key "-brand")
#+nil
(p:parse #'key "about")

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
  (p:fmap (lambda (list)
            (destructuring-bind (name args) list
              (cond ((not args) (make-term :name name))
                    (t (destructuring-bind (arg val) args
                         (make-term :name name
                                    :arg (string->keyword arg)
                                    :val val))))))
          (funcall (p:between (*> +brace-open+ +skip-space+ +dash+)
                              (<*> (p:take-while1 (lambda (c)
                                                    (not (or (eql c #\space)
                                                             (eql c #\newline)
                                                             (eql c #\})
                                                             (eql c #\()))))
                                   (p:opt (p:between +paren-open+
                                                     (<*> (p:take-while1 (lambda (c)
                                                                           (not (or (eql c #\space)
                                                                                    (eql c #\newline)
                                                                                    (eql c #\:)))))
                                                          (*> +colon+
                                                              +skip-space+
                                                              #'quoted-string))
                                                     +paren-close+)))
                              (*> +skip-space+ +brace-close+))
                   offset)))

#+nil
(p:parse #'term "{ -brand-name }")
#+nil
(p:parse #'term "{ -https(host: \"example.com\") }")

(defun quoted-string (offset)
  (funcall (p:between +quote+
                      (p:take-while1 (lambda (c)
                                       (not (or (eql c #\newline)
                                                (eql c #\")))))
                      +quote+)
           offset))

#+nil
(p:parse #'quoted-string "\"hello\"")

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
                (:max-frac (make-numberf :input input :max-frac val))
                (:type     (make-numberf :input input :type val)))))
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
#+nil
(p:parse #'number "NUMBER($ratio, type: \"ordinal\")")

(defun number-option (offset)
  (funcall (p:alt #'min-frac #'max-frac #'plural-type) offset))

#+nil
(p:parse #'number-option "minimumFractionDigits: 2")

(defun min-frac (offset)
  (p:fmap (lambda (n) (list :min-frac n))
          (funcall (*> (p:string "minimumFractionDigits")
                       +colon+
                       +skip-space+
                       #'p:unsigned)
                   offset)))

(defun max-frac (offset)
  (p:fmap (lambda (n) (list :max-frac n))
          (funcall (*> (p:string "maximumFractionDigits")
                       +colon+
                       +skip-space+
                       #'p:unsigned)
                   offset)))

(defun plural-type (offset)
  (p:fmap (lambda (kw) (list :type kw))
          (funcall (*> (p:string "type")
                       +colon+
                       +skip-space+
                       (p:between +quote+
                                  (p:alt (<$ :ordinal (p:string "ordinal"))
                                         (<$ :cardinal (p:string "cardinal")))
                                  +quote+))
                   offset)))
