module Main where

import Board (generateKojun, Possibilities)
import Printer (printBoard, printPossibilities)
import Solver (solve, initializePossibilities, defineRegionsStruct)

main :: IO String
main = do
    let (originalBoard, regionsBoard) = generateKojun 17
    let possibilities = initializePossibilities originalBoard regionsBoard
    printPossibilities possibilities
    let regionsStruct = defineRegionsStruct regionsBoard
        
    let (solvable, solvedBoard) = solve originalBoard regionsBoard possibilities regionsStruct
    if solvable then
        printBoard solvedBoard
    else
        putStrLn "Não hhá solução para o Kojun" >> return ""