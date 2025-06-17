;;; Logic for resolving a given particular localisation.

(in-package :fluent)

#+nil
(let* ((s (uiop:read-file-string "tests/data/aura.ftl"))
       (l (parse s)))
  (resolve l "check-pconf-pacnew-old" :path "pacman.conf" :days 1))

(defun resolve (ctx tag &rest inputs)
  "Find a localisation line by name and fully resolve it via some input args."
  (resolve-line (localisations-locale ctx)
                (localisations-terms ctx)
                (gethash tag (localisations-lines ctx))
                inputs))

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
          (t (format nil "~a" n)))))

#+nil
(resolve-number (make-numberf :input :foo :max-frac 2) 1.123)

(defun resolve-line (locale terms line inputs)
  "Completely resolve some localisation line into a single string."
  (format nil "~{~a~}"
          (reduce
           (lambda (chunk acc)
             (let ((next (etypecase chunk
                           (string chunk)
                           (variable (get-input inputs (variable-name chunk)))
                           ;; Since the term line itself can contains inputs, we
                           ;; need to recursively resolve.
                           (term (let ((ins (cond ((not (term-arg chunk)) '())
                                                  (t (list (term-arg chunk) (term-val chunk))))))
                                   (resolve-line locale terms (gethash (term-name chunk) terms) ins)))
                           (numberf (resolve-number chunk (get-input inputs (numberf-input chunk))))
                           (selection (resolve-selection locale terms chunk inputs)))))
               (cons next acc)))
           line
           :initial-value '()
           :from-end t)))

#+nil
(let ((terms (localisations-terms (parse "-brand-name = Firefox")))
      (line (p:parse #'entry "About { -brand-name }.")))
  (resolve-line :en terms line '()))

#+nil
(let ((terms (localisations-terms (parse "-https = https://{ $host }")))
      (line (p:parse (<* #'entry #'p:eof) "Visit { -https(host: \"example.com\") } for more information.")))
  (resolve-line :en terms line '()))

(defun resolve-selection (locale terms sel inputs)
  "Choose the correct localisation line and resolve it."
  (let* ((in   (selection-input sel))
         (val  (getf inputs (etypecase in
                              (keyword in)
                              (numberf (numberf-input in)))))
         (line (branch-line (find-branch locale sel val))))
    (resolve-line locale terms line inputs)))

(defun find-branch (locale sel val)
  "Find a localisation branch whose condition/term matches the incoming value."
  (let ((found (etypecase val
                 (real (let* ((inp (selection-input sel))
                              (s (etypecase inp
                                   (keyword (format nil "~a" val))
                                   (numberf (resolve-number inp val))))
                              (cat (cond ((and (numberf-p inp)
                                               (eq :ordinal (numberf-type inp)))
                                          (plurals:ordinal locale s))
                                         (t (plurals:cardinal locale s)))))
                         (find-if (lambda (branch)
                                    (let ((term (branch-term branch)))
                                      (etypecase term
                                        (plurals:category (eq cat term))
                                        (t (equal s term)))))
                                  (selection-branches sel))))
                 (string (find-if (lambda (branch) (equal val (branch-term branch)))
                                  (selection-branches sel)))
                 ;; For when a usually "parameterized term" actually had no
                 ;; associated argument, and so there is actually no value for
                 ;; each branch selector to compare against.
                 ;;
                 ;; There are potentially other ways a nil could find its way
                 ;; down here, but I still want to keep this as an `etypecase'
                 ;; to avoid other type-related surprises.
                 (null nil))))
    (cond ((not found) (selection-default sel))
          (t found))))

#+nil
(let ((sel (p:parse #'selection "{ NUMBER($score, minimumFractionDigits: 1) ->
        [0.0]   You scored zero points. What happened?
       *[other] You scored { NUMBER($score, minimumFractionDigits: 1) } points.
}")))
  (resolve-selection :en (make-hash-table) sel '(:score 1)))

#+nil
(let ((sel (p:parse #'selection "{ NUMBER($pos, type: \"ordinal\") ->
   [1] You finished first!
   [one] You finished {$pos}st
   [two] You finished {$pos}nd
   [few] You finished {$pos}rd
  *[other] You finished {$pos}th
}")))
  (resolve-selection :en (make-hash-table) sel '(:pos 1)))
