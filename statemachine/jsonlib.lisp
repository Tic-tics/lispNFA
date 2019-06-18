(in-package #:statemachine)

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun file-load-json (filename)
  (let ((contents (file-get-contents filename)))
    (jonathan:parse contents :as :hash-table)))
