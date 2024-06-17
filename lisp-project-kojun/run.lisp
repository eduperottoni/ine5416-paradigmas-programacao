(load "printer.lisp")
(load "main.lisp")

(compile-file "printer.lisp")
(compile-file "main.lisp")

(load "printer.fas")
(load "main.fas")

(in-package :main)
(main)