(defpackage fluent/tests
  (:use :cl :parachute)
  (:local-nicknames (#:f #:fluent)
                    (#:p #:parcom)))

(in-package :fluent/tests)

(define-test parsing)

(define-test parsing-placeables
  :parent parsing
  (is equalp (f::make-variable :name :foo)
      (p:parse #'f::variable "{ $foo }"))
  (is equalp (f::make-variable :name :foo)
      (p:parse #'f::variable "{$foo}"))
  (is equalp (f::make-term :name "foo")
      (p:parse #'f::term "{ -foo }"))
  (is equalp (f::make-term :name "foo")
      (p:parse #'f::term "{-foo}"))
  (is equal "}" (p:parse #'f::quoted "{\"}\"}"))
  (finish (p:parse #'f::placeable "{ NUMBER($ratio, minimumFractionDigits: 2) }"))
  (finish (p:parse (p:<* #'f::term #'p:eof) "{ -https(host: \"example.com\") }")))

(define-test parsing-plaintext
  :parent parsing
  (is equal '("this is a" "multiline string") (p:parse #'f::entry "this is a
  multiline string"))
  (is equal '("this has" "more than" "two lines") (p:parse #'f::entry "this has
  more than
  two lines")))

#+nil
(define-test parsing-ideally-passes-but-doesnt
  :parent parsing
  ;; By the spec, that extra newline in between should be preserved.
  (fail (p:parse #'f::entry "this has
  an extra

  blank line!")))

(define-test parsing-pairs
  :parent parsing
  (finish (p:parse (p:<* #'f::pair #'p:eof) "dpi-ratio = Your DPI ratio is { NUMBER($ratio, minimumFractionDigits: 2)}")))

(define-test parsing-files
  :parent parsing
  (let ((s (uiop:read-file-string #p"tests/data/basic.ftl")))
    (finish (f:parse s)))
  (let* ((s (uiop:read-file-string #p"tests/data/aura.ftl"))
         (l (f:parse s)))
    (is equal "Validating your system." (f:resolve l "check-start"))
    (fail (f:resolve l "check-env-exec"))
    (is equal "emacs installed and executable?" (f:resolve l "check-env-exec" :exec "emacs"))
    (is equal "Fix: Update your foo.ftl to include Spanish." (f:resolve l "check-env-lang-fix" :file "foo.ftl" :lang "Spanish"))
    (is equal "pacman.conf is older than its .pacnew by 1 day." (f:resolve l "check-pconf-pacnew-old" :path "pacman.conf" :days 1))
    (is equal "pacman.conf is older than its .pacnew by 27 days." (f:resolve l "check-pconf-pacnew-old" :path "pacman.conf" :days 27))))

(define-test parsing-minor-things
  :parent parsing
  (is eq :photocount (p:parse #'f::dollared "$photoCount"))
  (is eq :other (p:parse #'f::category "other"))
  (is eq :few (p:parse #'f::branch-selection-term "[few]"))
  (is equal "male" (p:parse #'f::branch-selection-term "[male]"))
  (is equal "1" (p:parse #'f::branch-selection-term "[1]"))
  (is equal "1.0" (p:parse #'f::branch-selection-term "[1.0]")))

(define-test parsing-selections
  :parent parsing
  (let* ((def (f::make-branch :term :other
                              :line (list "added " (f::make-variable :name :photocount) " new photos")
                              :default t))
         (sel (f::make-selection :input :photocount
                                 :branches (list (f::make-branch :term :one
                                                                 :line (list "added a new photo")
                                                                 :default nil)
                                                 def)
                                 :default def)))
    (is equalp sel (p:parse #'f::selection "{$photoCount ->
  [one] added a new photo
 *[other] added {$photoCount} new photos
}")))
  (finish (p:parse (p:<* #'f::selection #'p:eof) "{ NUMBER($score, minimumFractionDigits: 1) ->
        [0.0]   You scored zero points. What happened?
       *[other] You scored { NUMBER($score, minimumFractionDigits: 1) } points.
    }")))

(define-test functions)

(define-test number
  :parent functions
  (is equal "1" (f::resolve-number (f::make-numberf :input :foo :min-frac 0) 1))
  (is equal "1.0" (f::resolve-number (f::make-numberf :input :foo :min-frac 0) 1.0))
  (is equal "1.123" (f::resolve-number (f::make-numberf :input :foo :min-frac 0) 1.123))
  (is equal "1.000" (f::resolve-number (f::make-numberf :input :foo :min-frac 3) 1))
  (is equal "1.000" (f::resolve-number (f::make-numberf :input :foo :min-frac 3) 1.0))
  (is equal "1.123" (f::resolve-number (f::make-numberf :input :foo :min-frac 2) 1.123))
  (is equal "1" (f::resolve-number (f::make-numberf :input :foo :max-frac 0) 1))
  (is equal "1" (f::resolve-number (f::make-numberf :input :foo :max-frac 0) 1.0))
  (is equal "1.0" (f::resolve-number (f::make-numberf :input :foo :max-frac 1) 1.02))
  (is equal "1.02" (f::resolve-number (f::make-numberf :input :foo :max-frac 2) 1.02))
  (is equal "1.02" (f::resolve-number (f::make-numberf :input :foo :max-frac 2) 1.023)))

(define-test resolution)

(define-test type-selection
  :parent resolution
  (let ((sel (p:parse (p:<* #'f::selection #'p:eof) "{ NUMBER($pos, type: \"ordinal\") ->
   [1] You finished first!
   [one] You finished {$pos}st
   [two] You finished {$pos}nd
   [few] You finished {$pos}rd
  *[other] You finished {$pos}th
}")))
    (is equal "You finished first!" (f::resolve-selection :en (make-hash-table) sel '(:pos 1)))
    (is equal "You finished 2nd" (f::resolve-selection :en (make-hash-table) sel '(:pos 2)))))

(define-test terms
  :parent resolution
  (let ((terms (f::localisations-terms (f:parse "-brand-name = Firefox")))
        (line  (p:parse #'f::entry "About { -brand-name }.")))
    (is equal "About Firefox." (f::resolve-line :en terms line '())))
  (let ((terms (f::localisations-terms (f:parse "-https = https://{ $host }")))
        (line  (p:parse (p:<* #'f::entry #'p:eof) "Visit { -https(host: \"example.com\") } for more info.")))
    (is equal "Visit https://example.com for more info." (f::resolve-line :en terms line '())))
  (let ((terms (f::localisations-terms (f:parse "-brand-name =
    { $case ->
       *[nominative] Firefox
        [locative] Firefoksie
    }")))
        (line0 (p:parse (p:<* #'f::entry #'p:eof) "Informacje o { -brand-name(case: \"locative\") }"))
        (line1 (p:parse (p:<* #'f::entry #'p:eof) "{ -brand-name } został pomyślnie zaktualizowany.")))
    (is equal "Informacje o Firefoksie" (f::resolve-line :en terms line0 '()))
    (is equal "Firefox został pomyślnie zaktualizowany." (f::resolve-line :en terms line1 '()))))
