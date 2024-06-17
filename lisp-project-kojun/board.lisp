(defun generate-kojun (size)
  "Generates a Kojun puzzle and its region board for a given size.
  Returns a list where the first element is the Kojun board and the second element is the region board."
  (cond
    ((= size 6) (list board6 regions-board6))
    ((= size 8) (list board8 regions-board8))
    ((= size 10) (list board10 regions-board10))
    ((= size 12) (list board12 regions-board12))
    ((= size 14) (list board14 regions-board14))
    ((= size 17) (list board17 regions-board17))
    (t (error "Unsupported board size"))))

;; Example 6x6 board and regions
(defparameter board6
  '((0 0 0 0 0 2)
    (2 0 0 5 0 0)
    (0 0 3 0 0 4)
    (0 0 0 3 0 1)
    (0 0 0 0 0 0)
    (0 0 3 0 2 5)))

(defparameter regions-board6
  '((0 1 2 2 3 3)
    (0 1 4 3 3 3)
    (0 0 4 4 4 5)
    (6 6 7 5 5 5)
    (6 6 7 8 9 9)
    (10 10 8 8 8 8)))

;; Example 8x8 board and regions
(defparameter board8
  '((0 1 0 0 3 0 0 5)
    (0 0 0 1 0 0 2 3)
    (0 0 2 0 0 0 6 0)
    (0 0 3 4 0 0 0 0)
    (4 7 0 6 1 5 0 0)
    (0 0 1 0 0 0 0 0)
    (6 0 0 0 0 4 0 7)
    (3 4 1 0 2 0 0 0)))

(defparameter regions-board8
  '((0 0 0 6 6 6 6 7)
    (1 1 1 5 5 7 6 7)
    (3 2 4 4 4 7 7 7)
    (3 3 8 8 8 9 9 9)
    (15 15 15 15 8 11 11 10)
    (16 15 15 15 13 11 11 10)
    (16 16 16 14 13 14 11 11)
    (16 16 16 14 14 14 11 12)))

;; Example 10x10 board and regions
(defparameter board10
  '((0 0 0 2 4 0 3 0 0 3)
    (3 0 5 0 0 0 0 1 4 0)
    (2 0 0 0 0 2 1 0 0 2)
    (1 6 5 0 1 5 2 0 0 0)
    (0 0 0 0 0 0 0 5 0 6)
    (6 0 3 0 3 0 0 0 0 4)
    (0 0 0 0 0 2 4 0 7 2)
    (4 0 2 0 2 0 0 5 3 0)
    (0 0 0 3 0 6 0 0 0 0)
    (0 1 0 1 0 0 0 5 0 0)))

(defparameter regions-board10
  '((1 1 1 1 3 3 4 5 5 5)
    (2 1 1 8 3 3 4 4 5 6)
    (2 2 2 8 8 3 3 5 5 7)
    (2 11 11 8 8 9 9 5 7 7)
    (12 12 11 11 9 9 9 10 7 10)
    (12 11 11 11 13 13 10 10 10 10)
    (12 14 16 17 17 13 13 13 13 10)
    (12 14 14 18 17 13 20 21 21 22)
    (12 15 14 19 17 20 20 21 21 21)
    (12 15 15 19 19 20 20 20 21 21)))

;; Example 12x12 board and regions
(defparameter board12
  '((0 0 0 0 1 0 5 7 0 0 0 6)
    (0 0 0 4 3 0 3 5 0 0 0 0)
    (0 4 0 6 0 0 0 3 0 3 0 4)
    (0 0 1 5 7 0 0 0 6 7 0 2)
    (0 0 0 2 0 2 0 0 0 4 0 0)
    (0 0 4 0 0 0 0 0 0 3 0 2)
    (6 0 0 0 0 2 0 0 4 0 0 4)
    (0 0 0 0 2 0 4 0 3 6 0 0)
    (0 3 0 0 0 3 0 5 0 0 1 5)
    (0 0 4 0 5 2 0 3 0 4 0 0)
    (0 3 0 0 0 0 4 0 0 0 0 2)
    (0 0 2 0 2 0 3 0 0 0 4 0)))

(defparameter regions-board12
  '((0 1 1 2 2 2 2 7 7 7 8 8)
    (0 3 3 3 3 6 2 7 7 7 8 8)
    (0 4 3 5 5 6 6 9 9 7 8 8)
    (0 4 4 5 6 6 6 9 10 10 10 10)
    (0 4 16 5 5 6 22 9 11 10 10 10)
    (16 16 16 5 21 21 22 23 11 13 12 12)
    (16 16 17 24 24 21 22 23 23 13 13 13)
    (18 16 17 25 25 21 26 23 23 28 13 14)
    (18 18 17 25 26 26 26 27 27 28 13 15)
    (18 18 19 25 25 27 27 27 27 28 36 37 38 25)
    (18 18 19 25 25 27 27 27 27 28 28 15)
    (18 20 20 20 30 30 31 31 31 28 15 15)))

;; Example 14x14 board and regions
(defparameter board14
  '((4 2 0 6 0 4 0 2 0 4 0 2 4 0)
    (0 0 0 4 1 0 0 3 0 0 6 0 0 5)
    (1 0 0 0 0 0 0 0 2 0 0 0 0 0)
    (0 5 0 0 0 1 0 4 0 5 0 3 0 0)
    (0 0 3 2 4 0 0 0 1 0 3 0 4 0)
    (2 1 0 0 0 0 3 0 0 0 0 5 0 0)
    (6 0 0 0 0 3 0 0 4 6 0 3 7 0)
    (0 0 0 2 0 4 0 5 0 0 2 0 0 0)
    (0 0 4 0 0 0 0 3 0 0 0 2 0 0)
    (0 2 0 0 0 0 0 0 6 0 0 0 0 1)
    (0 0 1 4 3 0 0 0 5 3 0 0 4 0)
    (0 3 0 6 0 0 1 0 0 0 0 0 3 0)
    (4 0 0 0 2 0 0 6 0 0 0 0 0 0)
    (0 6 0 0 4 3 0 5 0 6 0 3 0 0)))

(defparameter regions-board14
  '((0 0 1 2 2 2 3 4 5 6 7 8 8 8)
    (0 0 9 2 2 2 3 4 5 6 7 8 8 8)
    (10 0 9 2 2 2 11 4 5 6 7 7 7 7)
    (10 0 9 12 2 2 11 13 14 6 15 15 16 7)
    (10 0 9 12 2 2 11 13 14 6 15 15 16 7)
    (10 0 9 12 2 2 11 13 14 6 15 15 16 7)
    (10 0 9 12 2 2 11 13 14 6 15 15 16 7)
    (10 0 9 12 2 2 11 13 14 6 15 15 16 7)
    (10 0 9 12 2 2 11 13 14 6 15 15 16 7)
    (10 0 9 12 2 2 11 13 14 6 15 15 16 7)
    (10 0 9 12 2 2 11 13 14 6 15 15 16 7)
    (10 0 9 12 2 2 11 13 14 6 15 15 16 7)
    (10 0 9 12 2 2 11 13 14 6 15 15 16 7)
    (10 0 9 12 2 2 11 13 14 6 15 15 16 7)))

;; Example 17x17 board and regions
(defparameter board17
  '((0 0 0 0 0 1 0 0 0 4 0 2 0 0 0 3 0)
    (2 0 0 4 0 0 3 0 6 0 0 0 5 0 0 0 0)
    (0 0 3 0 0 0 0 0 0 2 0 6 0 0 0 4 0)
    (0 4 0 2 0 6 5 0 0 0 0 0 0 0 0 0 1)
    (0 0 0 5 0 0 0 1 0 0 0 4 2 0 0 0 6)
    (0 0 0 0 0 0 6 0 3 0 0 0 0 0 0 0 0)
    (0 6 5 0 4 0 0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 6 0 2 0 0 0 0 0 0 0 0 0 5 0)
    (0 0 0 0 0 0 0 0 0 0 0 2 4 0 0 3 6)
    (0 0 0 0 0 0 0 0 2 0 4 6 0 0 0 0 0)
    (0 0 0 0 0 0 6 0 0 0 0 0 0 0 0 0 0)
    (6 0 0 0 0 0 0 2 0 0 0 0 0 5 3 0 0)
    (0 0 0 0 2 0 0 0 4 6 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 5 0 4 0 3 2 6 0 0 0)
    (0 0 2 6 0 0 0 0 0 0 0 4 0 0 3 0 0)
    (0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 2 0 0 0 6 0 0 0 0)))

(defparameter regions-board17
  '((0 0 1 1 2 3 3 3 4 4 4 4 5 5 6 7 7)
    (0 8 8 1 2 2 3 3 4 9 10 10 5 5 6 7 7)
    (11 8 1 1 2 2 2 12 4 9 10 10 5 5 6 13 7)
    (11 8 1 13 13 12 12 12 4 9 9 14 15 16 16 13 7)
    (11 8 8 13 13 13 17 12 18 9 19 14 15 16 16 20 20)
    (11 11 8 8 13 17 17 12 18 9 19 14 15 16 16 20 20)
    (21 21 21 21 22 17 17 17 18 9 9 9 15 16 16 16 20)
    (21 23 23 22 22 17 24 17 18 25 26 27 15 15 16 20 20)
    (21 23 23 22 22 17 24 24 18 25 26 27 28 29 29 20 20)
    (21 23 23 22 22 17 24 24 18 25 26 27 28 29 29 20 20)
    (30 31 31 22 22 17 24 24 32 33 34 35 28 29 29 20 20)
    (30 30 31 22 22 17 24 24 32 33 34 35 28 29 29 20 20)
    (30 30 31 22 22 17 24 24 32 33 34 35 28 29 29 20 20)
    (30 30 31 22 22 17 24 24 32 33 34 35 28 29 29 20 20)
    (30 30 31 22 22 17 24 24 32 33 34 35 28 29 29 20 20)
    (30 30 31 22 22 17 24 24 32 33 34 35 28 29 29 20 20)))