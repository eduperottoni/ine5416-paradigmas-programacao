(load "printer.lisp")
(load "board.lisp")
(load "solver.lisp")
(load "main.lisp")

(compile-file "printer.lisp")
(compile-file "board.lisp")
(compile-file "solver.lisp")
(compile-file "main.lisp")

(load "printer.fas")
(load "board.fas")
(load "solver.fas")
(load "main.fas")

(in-package :main)
(main)