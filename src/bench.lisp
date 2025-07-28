(in-package :fluent)

(defun read-string-orig (path)
  "Read the contents of a file into a string."
  (with-open-file (stream path :direction :input)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun read-string-stream (path)
  "Read the contents of a file into a string using a string output stream."
  (with-open-file (stream path :direction :input :element-type 'character)
    (with-output-to-string (out)
      (loop :for char := (read-char stream nil :eof)
            :until (eq char :eof)
            :do (write-char char out)))))

#+nil
(let ((s (read-string-stream #p"/home/colin/code/haskell/aura/rust/aura-pm/i18n/ar-SA/aura_pm.ftl")))
  (schar s (1- (length s))))

#+nil
(let ((s (uiop:read-file-string #p"/home/colin/code/haskell/aura/rust/aura-pm/i18n/ar-SA/aura_pm.ftl")))
  (schar s (1- (length s))))

#+nil
(let ((s #p"/home/colin/code/haskell/aura/rust/aura-pm/i18n/ar-SA/aura_pm.ftl"))
  (time (dotimes (n 1000)
          (uiop:read-file-string s)))
  (time (dotimes (n 1000)
          (read-string-stream s)))
  (time (dotimes (n 1000)
          (read-string-orig s))))

;; CONCLUSION
;;
;; UIOP is about 2x-3x slower than the stream-based approach above. My original
;; implementation is "just wrong" w.r.t. non-ascii chars, so can't be used, even
;; though it is the fastest and most memory efficient. I've adopted the
;; stream-based approach.
