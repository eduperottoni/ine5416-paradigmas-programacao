module Main where

import Data.List (intercalate)

import Board (generateKojun, Possibilities)
import Printer (printBoard)
import Solver (solve, initializePossibilities)


-- Função para converter a estrutura de possibilidades em uma string formatada
possibilitiesToString :: Possibilities -> String
possibilitiesToString possibilities =
    intercalate "\n" $ map formatRow possibilities
  where
    formatRow row = intercalate " | " $ map formatCell row
    formatCell cell = "[" ++ intercalate "," (map show cell) ++ "]"

-- Função para imprimir a estrutura de possibilidades
printPossibilities :: Possibilities -> IO ()
printPossibilities = putStrLn . possibilitiesToString

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