;;; Core types necessary for parsing and "resolution".

(in-package :fluent)

(deftype char-string ()
  '(simple-array character (*)))

(defstruct variable
  "An external value that we expect the user to provide at runtime."
  (name nil :type keyword))

(defstruct term
  "A reference to another static message in the localisation."
  (name nil :type string)
  (arg  nil :type (or null keyword))
  (val  nil :type (or null string)))

(defstruct numberf
  "The NUMBER function."
  (input    nil :type keyword)
  (min-frac nil :type (or null fixnum))
  (max-frac nil :type (or null fixnum))
  ;; `:ordinal' is opt-in due to being a somewhat rare choice.
  (type     :cardinal :type keyword))

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

(defstruct localisations
  "A body of localisation lines from a single language."
  (terms nil :type hash-table)
  (lines nil :type hash-table))

(defun fuse-localisations (a b)
  "Merge two localisations."
  (make-localisations :terms (merge-hash-tables! (localisations-terms a)
                                                 (localisations-terms b))
                      :lines (merge-hash-tables! (localisations-lines a)
                                                 (localisations-lines b))))

(defstruct fluent
  "A full localisation context, including all possible languages, the current
expected locale, and the fallback locale."
  (locale        nil :type keyword)
  (locale-lang   nil :type keyword)
  (fallback      nil :type keyword)
  (fallback-lang nil :type keyword)
  (locs          nil :type hash-table))

(defun fluent (locs &key (locale :en-us) (fallback :en-us))
  "Intelligently construct a `fluent' context."
  (make-fluent :locale locale
               :locale-lang (locale->lang locale)
               :fallback fallback
               :fallback-lang (locale->lang fallback)
               :locs locs))

(defun localisation->fluent (locs locale)
  "Construct a `fluent' context from a single collection of localisations."
  (let ((ht (make-hash-table :test #'eq :size 1)))
    (setf (gethash locale ht) locs)
    (fluent locs :locale locale :fallback locale)))
