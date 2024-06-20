module Main where

import Board (generateKojun, Possibilities)
import Printer (printBoard, printPossibilities)
import Solver (solve, initializePossibilities, defineRegionsStruct)

main :: IO String
main = do
    let (originalBoard, regionsBoard) = generateKojun 10
    let regionsStruct = defineRegionsStruct regionsBoard
    let possibilities = initializePossibilities originalBoard regionsBoard regionsStruct
        
    let (solvable, solvedBoard) = solve originalBoard regionsBoard possibilities regionsStruct
    if solvable then
        printBoard solvedBoard
    else
        putStrLn "Não há solução para o Kojun" >> return ""
