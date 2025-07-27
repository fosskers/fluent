;;; A greater scheme for reading and parsing entire sets of `.ftl' files from
;;; disk, and collating them into a unified `fluent' type.
;;;
;;; Given an initial directory in which to locate localisation files, the logic
;;; here will interpret each subdirectory as a full language/country pair (e.g.
;;; hi-IN, "Hindi in India"), then scour that subdirectory for all `.ftl' files,
;;; parsing them and unifying them into a single localisation. A condition will
;;; be raised if clashing keys exist between different files.
;;;
;;; The result of this is a Hash Table of locales mapped to their
;;; `localisations'.

(in-package :fluent)

(declaim (ftype (function (char-string) keyword) parse-locale))
(defun parse-locale (s)
  "Parse a full locale (e.g. hi-IN) from a string into a keyword."
  (p:parse (p:pmap #'string->keyword
                   (p:recognize (*> #'letters (p:opt (*> +dash+ #'letters)))))
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

(defun all-directories (dir)
  "Find all subdirectories within a given parent directory."
  (directory (f:ensure-directory (f:join dir "*"))))

#+nil
(all-directories #p"/home/colin/code/haskell/aura/rust/aura-pm/i18n")

(defun ftl-files-in-dir (dir)
  "Yield all the `.ftl' files in a given directory."
  (directory (f:join dir "*.ftl")))

#+nil
(ftl-files-in-dir #p"/home/colin/code/haskell/aura/rust/aura-pm/i18n/en-US")
