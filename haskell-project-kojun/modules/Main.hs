module Main where

import Board (generateKojun)
import Printer (printBoard)
import Solver (solve)

main :: IO String
main = do
    let (originalBoard, regionsBoard) = generateKojun 17
    let (solvable, solvedBoard) = solve originalBoard regionsBoard
    if solvable then
        printBoard solvedBoard
    else
        putStrLn "Não hhá solução para o Kojun" >> return ""