module Main where

import Board (generateKojun, Possibilities)
import Printer (printBoard, printPossibilities)
import Solver (solve, initializePossibilities)

main :: IO String
main = do
    let (originalBoard, regionsBoard) = generateKojun 6
    let possibilidades = initializePossibilities originalBoard regionsBoard
    printPossibilities possibilidades
        
    let (solvable, solvedBoard) = solve originalBoard regionsBoard
    if solvable then
        printBoard solvedBoard
    else
        putStrLn "Não hhá solução para o Kojun" >> return ""