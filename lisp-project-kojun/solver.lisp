(defpackage :solver
  (:use :cl)
  (:export :define-regions-struct :get-region-from-position :get-adjacent-numbers :check-vertical-adjacency-validity :initialize-possibilities))

(in-package :solver)


(defun define-regions-struct (regions-board)
  "Define the search structure based on the regions of the board.
  Each index of the list indicates the region id, and each list at index i contains
  the positions on the board that are in region i."
  (let* ((positions (loop for i from 0 below (length regions-board)
                          nconc (loop for j from 0 below (length (first regions-board))
                                      collect (cons i j))))
         (max-region (apply #'max (mapcan #'identity regions-board)))
         (initial-struct (make-list (1+ max-region) :initial-element nil)))
    (reduce (lambda (struct position) 
              (insert-position-on-struct position struct regions-board)) 
            positions 
            :initial-value initial-struct)))

(defun insert-position-on-struct (position struct regions-board)
  "Insert the position (i,j) into the correct index of the structure."
  (let* ((i (car position))
         (j (cdr position))
         (value (nth j (nth i regions-board)))
         (prefix (subseq struct 0 value))
         (suffix (subseq struct (1+ value))))
    (nconc prefix (list (append (nth value struct) (list position))) suffix)))


(defun get-region-from-position (position regions-board)
  "Gets the region for the given position from the regions board."
  (let ((i (car position))
        (j (cdr position)))
    (nth j (nth i regions-board))))


(defun get-adjacent-numbers (position board)
  "Gets the numbers adjacent to the given position in the board."
  (let* ((row (car position))
         (col (cdr position))
         (left (if (> col 0) (nth (- col 1) (nth row board)) nil))
         (right (if (< col (1- (length (first board)))) (nth (1+ col) (nth row board)) nil))
         (up (if (> row 0) (nth col (nth (- row 1) board)) nil))
         (down (if (< row (1- (length board))) (nth col (nth (1+ row) board)) nil)))
    (remove nil (list left right up down))))


(defun check-vertical-adjacency-validity (num position board r-position)
  "Checks the vertical adjacency validity for the given number and position."
  (let* ((row (car position))
         (col (cdr position))
         (r-row (car r-position))
         (r-col (cdr r-position)))
    (cond
      ((and (= r-col col) (= r-row (1+ row))) (< (nth r-col (nth r-row board)) num))
      ((and (= r-col col) (= r-row (1- row))) (> (nth r-col (nth r-row board)) num))
      (t t))))


(defun is-number-valid-for-the-position (num position board regions-struct regions-board before-run)
  "Checks if the number is valid for the given position in the board."
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


(defun initialize-possibilities (board regions-board regions-struct)
  (loop for i below (length board)
        collect
        (loop for j below (length (first board))
              collect (initialize-cell (cons i j) (nth j (nth i board)) board regions-board regions-struct))))

(defun initialize-cell (pos value board regions-board regions-struct)
  (if (= value 0)
      (let* ((current-region (get-region-from-position pos regions-board))
             (max-region (length (nth current-region regions-struct)))
             (possible-numbers (loop for num from 1 to max-region
                                     if (is-number-valid-for-the-position num pos board regions-struct regions-board t)
                                     collect num)))
        possible-numbers)
      '()))  ; Return an empty list if the cell already has a number