(defpackage :main
  (:use :cl)
  (:import-from :printer :print-board)
  (:import-from :board :generate-kojun)
  (:import-from :solver :define-regions-struct :get-region-from-position :get-adjacent-numbers :check-vertical-adjacency-validity :is-number-valid-for-the-position :initialize-possibilities :solve :find-empty))

(in-package :main)

(defun main()
    (let* ((size 14)
          (kojun-data (generate-kojun size))
          (board (first kojun-data))
          (regions (second kojun-data))
          (regions-struct (define-regions-struct regions))
          (possibilities (initialize-possibilities board regions regions-struct)))

    (print-board (second (solve board regions possibilities regions-struct))))
)