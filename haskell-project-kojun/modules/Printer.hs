module Printer (
    printBoard,
    printPossibilities
) where

import Data.List (intercalate)

import Board (Board, Possibilities)

-- FUNÇÃO DE PRINT DO TABULEIRO
printBoard :: Board -> IO String
printBoard [] = return ""
-- cada elemento aqui será uma lista e não um simples inteiro
printBoard (a:b) = do
    putStrLn (listToStr a)
    printBoard b

-- FUNÇÃO QUE TRANSFORMA A LISTA EM UMA STRING
listToStr :: [Int] -> String
listToStr = unwords . map show


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