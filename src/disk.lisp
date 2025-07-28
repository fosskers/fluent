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

;; TODO: 2025-07-28 Start here. You need to parse a locale keyword out of a
;; directory path, and then read each FTL file per subdir and collate the final
;; Hash Table.

(defun dir->locale (dir)
  "Parse a locale keyword from a directory name."
  (parse-locale (car (last (f:components dir)))))

#+nil
(dir->locale (car (all-directories #p"/home/colin/code/haskell/aura/rust/aura-pm/i18n")))

(defun localisations-in-dir (dir)
  "Given a directory filled with `.ftl' files, parse them all and fuse them
into a single `localisation' type."
  (reduce (lambda (acc file) (fuse-localisations acc (parse (read-string file))))
          (ftl-files-in-dir dir)
          :initial-value (make-localisations :terms (make-hash-table :test #'equal)
                                             :lines (make-hash-table :test #'equal))))

#+nil
(localisations-in-dir #p"tests/data/")

(defun read-all-localisations (dir)
  "Given a parent directory, read all per-locale localisations and form a Hash
Table of locales paired to their specific `localisations'."
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (sub (all-directories dir))
      (setf (gethash (dir->locale sub) ht)
            (localisations-in-dir sub)))
    ht))

#+nil
(read-all-localisations #p"/home/colin/code/haskell/aura/rust/aura-pm/i18n")
