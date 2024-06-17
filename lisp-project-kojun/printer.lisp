(defpackage :printer
  (:use :cl)
  (:export :print-board))

(in-package :printer)

(defun mapconcat (func lst sep)
  "Applies FUNC to each element of LST, concatenating the results with SEP."
  (let ((result ""))
    (dolist (x lst (subseq result 0 (- (length result) (length sep))))
      (setq result (concatenate 'string result (funcall func x) sep)))))


(defun list-to-str (lst)
  "Converts a list of integers to a string."
  (mapconcat #'write-to-string lst " "))    

(defun print-board (board)
  "Prints the board, where each row is a list of integers."
  (if (null board)
      ""
      (progn
        (format t "~a~%" (list-to-str (first board)))
        (print-board (rest board)))))