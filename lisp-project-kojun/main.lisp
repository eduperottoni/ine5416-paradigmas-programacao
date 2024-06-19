(defpackage :main
  (:use :cl)
  (:import-from :printer :print-board)
  (:import-from :board :generate-kojun)
  (:import-from :solver :define-regions-struct :get-region-from-position :get-adjacent-numbers :check-vertical-adjacency-validity :is-number-valid-for-the-position :initialize-possibilities))

(in-package :main)
;; Example usage

(defun main()
    ;; (defparameter *regions-board*
    ;;   '((0 0 2)
    ;;     (1 1 2)
    ;;     (1 1 2)))

    (let* ((size 10)
          (kojun-data (generate-kojun size))
          (board (first kojun-data))
          (regions (second kojun-data))
          (regions-struct (define-regions-struct regions))
          )

    (format t "Kojun Board:~%")
    (print-board board)
    (format t "~%Region Board:~%")
    (print-board regions)
    (format t "~%Regions Struct:~%")
    (print regions-struct)
    ;; Para testes de get-region-from-position
    (format t "~%Teste de GET-REGIONS-FROM-POSITION:~%")
    (format t "~a~%" (get-region-from-position (cons 9 1) regions))
    (format t "~%Teste de GET-ADJACENT-NUMBERS:~%")
    (format t "~a~%" (get-adjacent-numbers (cons 7 9) board))
    (format t "~%Teste de check-vertical-adjacency-validity:~%")
    (format t "~a~%" (check-vertical-adjacency-validity 2 (cons 6 0) board (cons 7 0)))
    (format t "~a~%" (check-vertical-adjacency-validity 5 (cons 6 0) board (cons 7 0)))

    (format t "~%Teste de is-number-valid-for-the-position:~%")
    (format t "~a~%" (is-number-valid-for-the-position 5 (cons 6 0) board regions-struct regions nil))
    (format t "~%Teste de initialize-possibilities:~%")
    (print (initialize-possibilities board regions regions-struct)))

    ;; defun initialize-possibilities (board regions-board regions-struct)


    ;; is-number-valid-for-the-position (num position board regions-struct regions-board before-run)

)