(in-package :fluent)

(defun read-string-sequence (path)
  "Read the contents of a file into a string."
  (with-open-file (stream path :direction :input)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun read-string-vector (path)
  "Read the contents of a file into a string."
  (with-open-file (stream path :direction :input :element-type 'character)
    (let ((s (make-array 16 :element-type 'character :adjustable t :fill-pointer 0)))
      (loop :for c := (read-char stream nil :eof)
            :until (eq c :eof)
            :do (vector-push-extend c s))
      s)))

#+nil
(let ((s (read-string #p"README.org")))
  (schar s (1- (length s))))

#+nil
(let ((s (uiop:read-file-string #p"README.org")))
  (schar s (1- (length s))))

#+nil
(let ((s #p"README.org"))
  (format t "--- UIOP ---~%")
  (time (dotimes (n 1000)
          (uiop:read-file-string s)))
  (format t "--- READ-SEQUENCE ---~%")
  (time (dotimes (n 1000)
          (read-string-sequence s)))
  (format t "--- VECTOR ---~%")
  (time (dotimes (n 1000)
          (read-string-vector s)))
  (format t "--- GOOD ---~%")
  (time (dotimes (n 1000)
          (read-string s))))

;; CONCLUSION
;;
;; UIOP is about 2x-3x slower than the stream-based approach above. My original
;; implementation is "just wrong" w.r.t. non-ascii chars, so can't be used, even
;; though it is the fastest and most memory efficient. I've adopted the
;; stream-based approach.
;;
;; Also note that the stream approach yields a `(simple-array character (*))'!
