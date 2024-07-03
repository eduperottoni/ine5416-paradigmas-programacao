(defpackage :solver
  (:use :cl)
  (:import-from :printer :print-board)
  (:export :define-regions-struct :get-region-from-position :get-adjacent-numbers :check-vertical-adjacency-validity :initialize-possibilities :solve))

(in-package :solver)


"Define a estrutura de busca baseada nas regiões do tabuleiro.
Cada índice da lista indica o ID da região, e cada lista no índice i contém
as posições no tabuleiro que estão na região i.

Parâmetros:
regions-board: Lista de listas representando as regiões do tabuleiro.

Retorno:
Uma lista onde cada índice contém as posições do tabuleiro na respectiva região."
(defun define-regions-struct (regions-board)
  (let* ((positions (loop for i from 0 below (length regions-board)
                          nconc (loop for j from 0 below (length (first regions-board))
                                      collect (cons i j))))
         (max-region (apply #'max (mapcan #'identity regions-board)))
         (initial-struct (make-list (1+ max-region) :initial-element nil)))
    (reduce (lambda (struct position) 
              (insert-position-on-struct position struct regions-board)) 
            positions 
            :initial-value initial-struct)))



"Insere a posição (i,j) no índice correto da estrutura de pesquisa.

Parâmetros:
position: Posição (i, j) a ser inserida.
struct: Estrutura onde a posição será inserida.
regions-board: Lista de listas representando as regiões do tabuleiro.

Retorno:
A estrutura atualizada com a posição inserida."
(defun insert-position-on-struct (position struct regions-board)
  (let* ((i (car position))
         (j (cdr position))
         (value (nth j (nth i regions-board)))
         (prefix (subseq struct 0 value))
         (suffix (subseq struct (1+ value))))
    (nconc prefix (list (append (nth value struct) (list position))) suffix)))



"Obtém a região para a posição dada no tabuleiro de regiões.

Parâmetros:
position: Posição (i, j) para a qual se deseja obter a região.
regions-board: Lista de listas representando as regiões do tabuleiro.

Retorno:
O ID da região correspondente à posição fornecida."
(defun get-region-from-position (position regions-board)
  (let ((i (car position))
        (j (cdr position)))
    (nth j (nth i regions-board))))



"Obtém os números adjacentes à posição dada no tabuleiro (acima, abaixo, esquerda e direita).

Parâmetros:
position: Posição (i, j) para a qual se deseja obter os números adjacentes.
board: Tabuleiro representado como uma lista de listas de números.

Retorno:
Uma lista com os números adjacentes à posição fornecida."
(defun get-adjacent-numbers (position board)
  (let* ((row (car position))
         (col (cdr position))
         (left (if (> col 0) (nth (- col 1) (nth row board)) nil))
         (right (if (< col (1- (length (first board)))) (nth (1+ col) (nth row board)) nil))
         (up (if (> row 0) (nth col (nth (- row 1) board)) nil))
         (down (if (< row (1- (length board))) (nth col (nth (1+ row) board)) nil)))
    (remove nil (list left right up down))))



"Verifica a validade da adjacência vertical para o número e posição fornecidos.

Parâmetros:
num: Número a ser verificado.
position: Posição (i, j) onde o número será colocado.
board: Tabuleiro representado como uma lista de listas de números.
r-position: Posição adjacente para verificar a validade.

Retorno:
T se a adjacência vertical for válida (número na posição superior é maior que o  número na posição inferior)
Caso contrário, NIL."
(defun check-vertical-adjacency-validity (num position board r-position)
  (let* ((row (car position))
         (col (cdr position))
         (r-row (car r-position))
         (r-col (cdr r-position)))
    (cond
      ((and (= r-col col) (= r-row (1+ row))) (< (nth r-col (nth r-row board)) num))
      ((and (= r-col col) (= r-row (1- row))) (> (nth r-col (nth r-row board)) num))
      (t t))))



"Verifica se o número é válido para a posição fornecida no tabuleiro.

Parâmetros:
num: Número a ser verificado.
position: Posição (i, j) onde o número será colocado.
board: Tabuleiro representado como uma lista de listas de números.
regions-struct: Estrutura das regiões do tabuleiro.
regions-board: Lista de listas representando as regiões do tabuleiro.
before-run: Booleano indicando se a verificação está sendo feita antes da execução principal.

Retorno:
T se o número for válido para a posição, caso contrário, NIL."
(defun is-number-valid-for-the-position (num position board regions-struct regions-board before-run)
  (let* ((current-region (get-region-from-position position regions-board))
         (region-positions (nth current-region regions-struct))
         (is-unique-in-region (every (lambda (pos)
                                       (/= num (nth (cdr pos) (nth (car pos) board))))
                                     region-positions))
         (adjacent-numbers (get-adjacent-numbers position board))
         (is-different-from-adjacents (not (member num adjacent-numbers)))
         (is-vertical-adjacent-valid (if before-run
                                         t
                                         (every (lambda (r-pos)
                                                  (check-vertical-adjacency-validity num position board r-pos))
                                                region-positions))))
    (and is-unique-in-region is-different-from-adjacents is-vertical-adjacent-valid)))



"Inicializa as possibilidades para cada célula do tabuleiro.

Parâmetros:
board: Tabuleiro representado como uma lista de listas de números.
regions-board: Lista de listas representando as regiões do tabuleiro.
regions-struct: Estrutura das regiões do tabuleiro.

Retorno:
Estrutura tridimensional de possibilidades para cada célula do tabuleiro.
Para cada posição do tabuleiro, há uma lista de possibilidades dado o tabuleiro inicial"
(defun initialize-possibilities (board regions-board regions-struct)
  (loop for i below (length board)
        collect
        (loop for j below (length (first board))
              collect (initialize-cell (cons i j) (nth j (nth i board)) board regions-board regions-struct))))



"Inicializa as possibilidades para uma célula específica do tabuleiro.

Parâmetros:
pos: Posição (i, j) da célula.
value: Valor da célula.
board: Tabuleiro representado como uma lista de listas de números.
regions-board: Lista de listas representando as regiões do tabuleiro.
regions-struct: Estrutura das regiões do tabuleiro.

Retorno:
Uma lista de números possíveis para a célula, ou uma lista vazia se a célula já contiver um número."
(defun initialize-cell (pos value board regions-board regions-struct)
  (if (= value 0)
      (let* ((current-region (get-region-from-position pos regions-board))
             (max-region (length (nth current-region regions-struct)))
             (possible-numbers (loop for num from 1 to max-region
                                     if (is-number-valid-for-the-position num pos board regions-struct regions-board t)
                                     collect num)))
        possible-numbers)
      '()))  ; Return an empty list if the cell already has a number



"Resolve o tabuleiro dado.

Parâmetros:
board: Tabuleiro representado como uma lista de listas de números.
regions-board: Lista de listas representando as regiões do tabuleiro.
possibilities: Lista de listas de possibilidades para cada célula do tabuleiro.
regions-struct: Estrutura das regiões do tabuleiro.

Retorno:
Uma lista com dois elementos, onde o primeiro é um booleano indicando se a solução foi encontrada,
e o segundo é o tabuleiro resolvido ou o estado atual do tabuleiro se não resolvido."
(defun solve (board regions-board possibilities regions-struct)
  ;; (format t "Solving board:~%")
  ;; (print-board board)
  (let ((empty (find-empty board)))
    (if (null empty)
        (progn
          (format t "~%No empty positions found, puzzle solved!~%")
          (list t board)) ; Return a list with two elements
        (let* ((row (car empty))
               (col (cdr empty))
              ;;  (current-region (get-region-from-position (cons row col) regions-board))
               (row-possibilities (nth row possibilities))
               (empty-possibilities (if (listp row-possibilities) (nth col row-possibilities) nil)))
          ;; (format t "Empty position: (~a, ~a)~%" row col)
          ;; (format t "Current region: ~a~%" current-region)
          ;; (format t "Possibilities for row ~a: ~a~%" row row-possibilities)
          ;; (format t "Possibilities for cell (~a, ~a): ~a~%" row col empty-possibilities)
          (if (not (listp empty-possibilities))
              (progn
                (format t "Error: Possibilities for position (~a, ~a) are not a list: ~a~%" row col empty-possibilities)
                (list nil board)) ; Return a list with two elements
              (try-numbers (cons row col) empty-possibilities board regions-struct regions-board possibilities))))))



"Tenta inserir números na posição dada a partir das possibilidades fornecidas.

Parâmetros:
pos: Posição (i, j) onde o número será colocado.
possible-values-for-position: Lista de possíveis valores para a posição.
board: Tabuleiro representado como uma lista de listas de números.
regions-struct: Estrutura das regiões do tabuleiro.
regions-board: Lista de listas representando as regiões do tabuleiro.
possibilities: Estrutura tridimensional de possibilidades para cada célula do tabuleiro.

Retorno:
Uma lista com dois elementos, onde o primeiro é um booleano indicando se a solução foi encontrada,
e o segundo é o tabuleiro atualizado ou o estado atual do tabuleiro se não resolvido."
(defun try-numbers (pos possible-values-for-position board regions-struct regions-board possibilities)
  ;; (format t "Trying numbers for position: ~a with possible-values-for-position: ~a~%" pos possible-values-for-position)
  (if (null possible-values-for-position)
      (values nil board) ; Return a list with two elements
      (let ((head (car possible-values-for-position))
            (tail (cdr possible-values-for-position)))
        (if (is-number-valid-for-the-position head pos board regions-struct regions-board nil)
            (progn
              ;; (format t "Number ~a is valid for position ~a. Updating board.~%" head pos)
              (let* ((new-board (update-board board pos head))
                     (result (solve new-board regions-board possibilities regions-struct)))
                (if (car result)
                    result
                    (progn
                      ;; (format t "Backtracking from position ~a with number ~a~%" pos head)
                      (try-numbers pos tail board regions-struct regions-board possibilities)))))
            (progn
              ;; (format t "Number ~a is not valid for position ~a. Trying next possibility.~%" head pos)
              (try-numbers pos tail board regions-struct regions-board possibilities))))))



"Atualiza o tabuleiro com o valor dado na posição especificada.

Parâmetros:
board: Tabuleiro representado como uma lista de listas de números.
pos: Posição (i, j) onde o valor será colocado.
value: Valor a ser inserido na posição.

Retorno:
O tabuleiro atualizado."
(defun update-board (board pos value)
  (let ((row (car pos))
        (col (cdr pos)))
    (loop for i from 0 below (length board)
          collect (if (= i row)
                      (loop for j from 0 below (length (nth i board))
                            collect (if (= j col) value (nth j (nth i board))))
                      (nth i board)))))



"Percorre cada posição do tabuleiro, procurando a primeira posição com zero (a primeira célula vazia no tabuleiro).

Parâmetros:
board: Tabuleiro representado como uma lista de listas de números.

Retorno:
A posição (i, j) da primeira célula vazia encontrada ou NIL se não houver células vazias."
(defun find-empty (board)
  (find-empty-in-rows board 0))



"Percorre as linhas do tabuleiro.

Parâmetros:
rows: Lista de listas representando as linhas do tabuleiro.
row-index: Índice da linha atual.

Retorno:
A posição (row-index, col-index) da primeira célula vazia encontrada ou NIL se não houver células vazias."
(defun find-empty-in-rows (rows row-index)
  (if (null rows)
      nil
      (let ((col-index (find-empty-in-row (car rows) 0)))
        (if col-index
            (cons row-index col-index)
            (find-empty-in-rows (cdr rows) (1+ row-index))))))



"Percorre uma linha, retornando o índice da coluna do primeiro zero.

Parâmetros:
row: Lista representando a linha do tabuleiro.
col-index: Índice da coluna atual.

Retorno:
O índice da coluna do primeiro zero encontrado ou NIL se não houver zeros na linha."
(defun find-empty-in-row (row col-index)
  (if (null row)
      nil
      (if (zerop (car row))
          col-index
          (find-empty-in-row (cdr row) (1+ col-index)))))
