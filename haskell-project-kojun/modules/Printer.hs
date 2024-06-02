module Printer (
    printBoard
) where

import Board (Board)

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