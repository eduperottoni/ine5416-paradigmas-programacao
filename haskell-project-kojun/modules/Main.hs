module Main where

import Board (originalBoard, regionsBoard)
import Printer (printBoard)
import Solver (solve)

main :: IO String
main = do
    let (solvable, solvedBoard) = solve originalBoard regionsBoard
    if solvable then
        printBoard solvedBoard
    else
        putStrLn "Não hhá solução para o Kojun" >> return ""