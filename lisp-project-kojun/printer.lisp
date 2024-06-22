(defpackage :printer
  (:use :cl)
  (:export :print-board))

(in-package :printer)


"Aplica func a cada elemento de lst, concatenando os resultados com sep.

Argumentos:
func - Função a ser aplicada a cada elemento da lista.
lst - Lista de elementos.
sep - Separador para concatenar os resultados.

Retorna:
Uma string resultante da aplicação de FUNC a cada elemento de LST, concatenada com SEP."
(defun mapconcat (func lst sep)
  (let ((result ""))
    (dolist (x lst (subseq result 0 (- (length result) (length sep))))
      (setq result (concatenate 'string result (funcall func x) sep)))))



"Converte uma lista de inteiros para uma string. Utiliza a função mapconcat para isso.

Argumentos:
lst - Lista de inteiros.

Retorna:
Uma string representando os inteiros na lista, separados por espaços."
(defun list-to-str (lst)
  (mapconcat #'write-to-string lst " "))



"Imprime o tabuleiro, onde cada linha é uma lista de inteiros.

Argumentos:
board - Lista de listas de inteiros, representando o tabuleiro.

Retorna:
Uma string vazia se o tabuleiro estiver vazio. Caso contrário, imprime cada linha do tabuleiro, recursivamente"
(defun print-board (board)
  (if (null board)
      (progn
        (format t "~a~%" (list-to-str (first board)))
        (print-board (rest board)))))