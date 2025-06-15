;;; Core types necessary for parsing and "resolution".

(in-package :fluent)

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
  (max-frac nil :type (or null fixnum))
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
