(defpackage :printer
  (:use :cl)
  (:export :print-board))

(in-package :printer)


"Aplica func a cada elemento de lst, concatenando os resultados com sep.

Parâmetros:
func: Função a ser aplicada a cada elemento da lista.
lst: Lista de elementos.
sep: Separador para concatenar os resultados.

Retorno:
Uma string resultante da aplicação de func a cada elemento de lst, concatenada com sep."
(defun mapconcat (func lst sep)
  (reduce (lambda (a b) (concatenate 'string a sep b))
          (mapcar func lst)))



"Converte uma lista de inteiros para uma string. Utiliza a função mapconcat para isso.

Parâmetros:
lst: Lista de inteiros.

Retorno:
Uma string representando os inteiros na lista, separados por espaços."
(defun list-to-str (lst)
  (mapconcat #'write-to-string lst " "))



"Imprime o tabuleiro, onde cada linha é uma lista de inteiros.

Parâmetros:
board: Lista de listas de inteiros, representando o tabuleiro.

Retorno:
Imprime cada linha do tabuleiro."
(defun print-board (board)
  (dolist (row board)
    (format t "~a~%" (list-to-str row))))