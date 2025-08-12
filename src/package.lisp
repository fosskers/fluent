(defpackage fluent
  (:use :cl)
  (:shadow #:variable #:number)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:p #:parcom)
                    (#:f #:filepaths))
  ;; --- Types --- ;;
  (:export #:fluent #:fluent-locale #:fluent-fallback #:fluent-locs)
  ;; --- Entry --- ;;
  (:export #:read-all-localisations #:parse #:resolve #:resolve-with)
  ;; --- Conditions --- ;;
  (:export #:missing-line #:unknown-locale #:missing-input)
  (:documentation "Software localisation via Mozilla's Project Fluent."))

(in-package :fluent)

(defun string->keyword (s)
  (multiple-value-bind (kw _) (intern (string-upcase s) "KEYWORD")
    (declare (ignore _))
    kw))

#+nil
(string->keyword "hello")

(define-condition missing-line (error)
  ((line     :initarg :line :reader missing-line-line)
   (locale   :initarg :locale :reader missing-line-locale)
   (fallback :initarg :fallback :reader missing-line-fallback))
  (:documentation "A certain localisation couldn't be found in any language.")
  (:report (lambda (c stream)
             (format stream "The localisation '~a' could not be found.~%Locale: ~a~%Fallback: ~a"
                     (missing-line-line c)
                     (missing-line-locale c)
                     (missing-line-fallback c)))))

(define-condition unknown-locale (error)
  ((locale :initarg :locale :reader unknown-locale-locale))
  (:documentation "The user attempted to resolve via an unknown locale.")
  (:report (lambda (c stream)
             (format stream "Unknown locale: ~a" (unknown-locale-locale c)))))

(define-condition missing-input (error)
  ((expected :initarg :expected :reader missing-input-expected))
  (:documentation "A certain arg was expected for a localisation line, but it wasn't given.")
  (:report (lambda (c stream)
             (format stream "Missing localisation argument: ~a" (missing-input-expected c)))))

(defun get-input (col k)
  "Extra error handling around a `getf' call."
  (let ((v (getf col k)))
    (cond (v v)
          (t (error 'missing-input :expected k)))))

(declaim (ftype (function ((or string pathname)) (simple-array character (*))) read-string))
(defun read-string (path)
  "Read the contents of a file into a string."
  (with-open-file (stream path :direction :input :element-type 'character)
    (with-output-to-string (out)
      (loop :for c := (read-char stream nil :eof)
            :until (eq c :eof)
            :do (write-char c out)))))

#+nil
(read-string #p"tests/data/basic.ftl")

(declaim (ftype (function (hash-table hash-table) hash-table) merge-hash-tables!))
(defun merge-hash-tables! (a b)
  "Merge the elements of a second Hash Table into the first one. If a given key
exists in both Hash Tables, the value of the second will be kept."
  (maphash (lambda (k v) (setf (gethash k a) v)) b)
  a)

#+nil
(let ((a (make-hash-table :test #'eq))
      (b (make-hash-table :test #'eq)))
  (setf (gethash :a a) 1)
  (setf (gethash :b a) 2)
  (setf (gethash :c b) 3)
  (setf (gethash :d b) 4)
  (merge-hash-table! a b))

(declaim (ftype (function (char-string) keyword) parse-locale))
(defun parse-locale (s)
  "Parse a full locale (e.g. hi-IN) from a string into a keyword."
  (p:parse (p:pmap #'string->keyword
                   (p:recognize (*> #'letters (p:opt (*> (p:char #\-) #'letters)))))
           s))

#+nil
(parse-locale "hi-IN")
#+nil
(parse-locale "eo")

(defun letters (offset)
  "Some letters."
  (funcall (p:take-while1 #'p:ascii-letter?) offset))

#+nil
(p:parse #'letters "hi-IN")

(declaim (ftype (function (keyword) keyword) locale->lang))
(defun locale->lang (locale)
  "Extract the language portion of a locale. So, the `en' in `en-US'."
  (p:parse (p:pmap #'string->keyword (p:recognize #'letters))
           #+(or sbcl ecl)
           (elevate-string (symbol-name locale))
           #-(or sbcl ecl)
           (symbol-name locale)))

#+nil
(locale->lang (parse-locale "hi-IN"))

(defun elevate-string (base)
  "Forced workaround involving `simple-base-string' stupidity."
  (let ((simple (make-array (length base) :element-type 'character)))
    (replace simple base)))
