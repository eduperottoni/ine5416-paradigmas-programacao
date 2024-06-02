module Solver(
    defineRegionsStruct,
    findEmpty
) where

import Board(
    regionsBoard,
    originalBoard,
    Board,
    Position,
    RegionsStruct)

{-
Define Estrutura de Pesquisa (lista de listas)
Cada índice da lista indica o id da região e cada lista no índice i contém as posições do tabuleiro que estão na região i
Ex.:[[0, 0, 2],
     [1, 1, 2],
     [1, 1, 2]] vai gerar uma estrutura:

    [[(0,0), (1,1)],                -- Positions for region 0
    [(1,0), (1,1), (2,0), (2,1)],   -- Positions for region 1
    [(0,2), (1,2), (2,2)]]          -- Positions for region 2
-}
defineRegionsStruct :: Board -> RegionsStruct
-- Aplica insertPositionOnStruct para cada elemento de positions, partindo de initialStruct
defineRegionsStruct regionsBoard = foldr insertPositionOnStruct initialStruct positions
    where
        -- positions é uma lista com todas as posições do tabuleiro
        positions = [(i, j) | i <- [0..(length regionsBoard - 1)], j <- [0..(length (head regionsBoard) - 1)]]
        
        -- initialStruct é uma lista de n listas vazias, sendo que n é o número de regiões do Kojun
        -- concat transforma a lista 2D de listas em uma lista 1D: https://zvon.org/other/haskell/Outputprelude/concat_f.html
        -- maximum retorna o maior valor da lista: https://zvon.org/other/haskell/Outputprelude/maximum_f.html
        maxRegion = maximum (concat regionsBoard)
        -- replicate gera uma lista com (maxRegion + 1) listas vazias
        initialStruct = replicate (maxRegion + 1) []

        -- insertPositionOnStruct adiciona a posição (i,j) dada no índice correto da struct
        insertPositionOnStruct position struct = 
            -- value = regionsBoard[i][j]
            let value = regionsBoard !! fst position !! snd position
            -- struct = struct[0:value - 1] + (struct[value].append(position)) + struct[value + 1:]
            -- take e drop retornam lista a partir do início ou do final com o tamanho especificado: https://zvon.org/other/haskell/Outputprelude/drop_f.html
            in (take value struct) ++ [struct !! value ++ [position]] ++ (drop (value + 1) struct)


-- Function to find an empty (zeroed) cell in the board
findEmpty :: Board -> Maybe Position
findEmpty board = findEmptyInRows board 0
  where
    -- Helper function to iterate over rows with an index
    findEmptyInRows :: [[Int]] -> Int -> Maybe Position
    findEmptyInRows [] _ = Nothing
    findEmptyInRows (row:rows) rowIndex = 
      case findEmptyInRow row 0 of
        Just colIndex -> Just (rowIndex, colIndex)
        Nothing -> findEmptyInRows rows (rowIndex + 1)
    
    -- Helper function to iterate over a single row with an index
    findEmptyInRow :: [Int] -> Int -> Maybe Int
    findEmptyInRow [] _ = Nothing
    findEmptyInRow (x:xs) colIndex
      | x == 0    = Just colIndex
      | otherwise = findEmptyInRow xs (colIndex + 1)
