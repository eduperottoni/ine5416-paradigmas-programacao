(defpackage :main
  (:use :cl)
  (:import-from :printer :print-board))

(in-package :main)
;; Example usage

(defun hello()
"Hello world!"
)

(defun main()
    (let ((board '((0 0 0) (0 0 0) (0 0 0)))
       (regions '((1 1 2) (1 2 2) (3 3 2))))
    (print-board board))
    (write-string (hello))
)