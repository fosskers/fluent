;;; Logic for resolving a given particular localisation.

(in-package :fluent)

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
                                  (selection-branches sel))))))
    (cond ((not found) (selection-default sel))
          (t found))))

#+nil
(let ((sel (p:parse #'selection "{ NUMBER($score, minimumFractionDigits: 1) ->
        [0.0]   You scored zero points. What happened?
       *[other] You scored { NUMBER($score, minimumFractionDigits: 1) } points.
}")))
  (resolve-selection :en sel 1))

#+nil
(let ((sel (p:parse #'selection "{ NUMBER($pos, type: \"ordinal\") ->
   [1] You finished first!
   [one] You finished {$pos}st
   [two] You finished {$pos}nd
   [few] You finished {$pos}rd
  *[other] You finished {$pos}th
}")))
  (resolve-selection :en sel 1))
