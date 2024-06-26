module Solver(
    solve,
    initializePossibilities,
    defineRegionsStruct
) where

import Board(
    Board,
    Position,
    RegionsStruct,
    Possibilities)

import Debug.Trace (trace)
import Data.List (minimumBy, delete)
import Data.Function (on)


{-
Define Estrutura de Pesquisa (lista de listas)
Cada índice da lista indica o id da região e cada lista no índice i contém as posições do tabuleiro que estão na região i
Ex.:[[0, 0, 2],
     [1, 1, 2],
     [1, 1, 2]] vai gerar uma estrutura:

    [[(0,0), (0,1)],                -- Positions for region 0
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


{-
Itera por cada posição do tabuleiro, procurando pela primeira posilção
cujo número nela seja zero (primeira célula vazia do tabuleiro).
-}
findEmpty :: Board -> Maybe Position
findEmpty board = findEmptyInRows board 0
  where
    -- Itera nas linhas do tabuleiro
    findEmptyInRows :: [[Int]] -> Int -> Maybe Position
    findEmptyInRows [] _ = Nothing
    findEmptyInRows (head:tail) rowIndex = 
      case findEmptyInRow head 0 of
        Just colIndex -> Just (rowIndex, colIndex)
        Nothing -> findEmptyInRows tail (rowIndex + 1)
    
    -- Itera dentro da linha, selecionando o índice da coluna que possui o valor nulo
    findEmptyInRow :: [Int] -> Int -> Maybe Int
    findEmptyInRow [] _ = Nothing
    findEmptyInRow (head:tail) colIndex
      | head == 0    = Just colIndex
      | otherwise = findEmptyInRow tail (colIndex + 1)


{-
Essa função recebe uma posição do tabuleiro e o tabuleiro de regiões
Devolve a região daquela posição no tabuleiro.
-}
getRegionFromPosition :: Position -> Board -> Int
getRegionFromPosition (i, j) regionsBoard = regionsBoard !! i !! j


{- Essa função recebe uma posição e o tabuleiro de números
Retorna os números que estão nas posições adjacentes à posição recebida -}
getAdjacentNumbers :: Position -> Board -> [Int]
getAdjacentNumbers (row, col) board =
    -- filtra os diferentes de Nothing
    -- mapeia os Justs para o valor em si
    map (\(Just x) -> x) (filter (/= Nothing) [left, right, up, down])
  where
    left = if col > 0 then Just (board !! row !! (col - 1)) else Nothing
    right = if col < (length (head board) - 1) then Just (board !! row !! (col + 1)) else Nothing
    up = if row > 0 then Just (board !! (row - 1) !! col) else Nothing
    down = if row < (length board - 1) then Just (board !! (row + 1) !! col) else Nothing


{-
Recebe um número e sua posição, o tabuleiro de números e uma outra posição da mesma região do número
Se a posição da região for verticalmente adjacente à posição do número:
  Se o número estiver acima do número da mesma região, deve ser maior
  Senão, deve ser menor
Retorna um booleano que indica essa validação
-}
checkVerticalAdjacencyValidity :: Int -> Position -> Board -> Position -> Bool
checkVerticalAdjacencyValidity num (row, col) board (rRow, rCol)
  -- Número deve ser maior que o da célula abaixo
  | rCol == col && rRow == (row + 1) = (board !! rRow !! rCol) < num
  -- Número deve ser menor que o da célula acima
  | rCol == col && rRow == (row - 1) = (board !! rRow !! rCol) > num
  | otherwise = True

{-
Função recebe um número, uma posição, o tabuleiro e a estrutura de pesquisa de regiões
Avalia se o número é válido para aquela posição. Ou seja, se o número sendo colocado
naquela posição obedece as regras do Kojun:
1 - O número é único na região
2 - O número é diferente dos números que estão na sua adjacência ortogonal
3 - Para dois números em posições adjacentes verticais, o número da posição superior deve ser maior
Retorna um booleano indicando se o número pode ser colocado na posição
-}
isNumberValidForThePosition :: Int -> Position -> Board -> RegionsStruct -> Board -> Bool -> Bool
isNumberValidForThePosition num pos board regionsStruct regionsBoard beforeRun =
    isUniqueInRegion && isDifferentFromAdjacents && isVerticalAdjacentValid
  where
    -- VALIDAÇÃO DA REGRA 1
    -- Pega todas as posições nessa região
    currentRegion = getRegionFromPosition pos regionsBoard
    regionPositions = regionsStruct !! currentRegion
    -- Verifica se o número é único dentro da região
    -- Para todas as posições, aplicamos a função lambda que de retornar true
    isUniqueInRegion = all (\(i, j) -> num /= board !! i !! j) regionPositions

    -- VALIDAÇÃO DA REGRA 2
    adjacentNumbers = getAdjacentNumbers pos board
    isDifferentFromAdjacents = notElem num adjacentNumbers

    -- VALIDAÇÃO DA REGRA 3
    -- Caso a função isNumberValidForPosition seja usada para construção das possibilidades, 
    -- beforeRun deve ser True e essa validação não é considerada
    isVerticalAdjacentValid = if beforeRun then True else all (\rPosition -> checkVerticalAdjacencyValidity num pos board rPosition) regionPositions


{-|
  Tenta resolver o Kojun usando backtracking

  Pega o estado atual do tabuleiro e o tabuleiro de regiões e
  retorna uma tupla contendo um booleano indicando se o puzzle
  foi ou não resolvido e o estado final do tabuleiro

  * table: Estado atual do tabuleiro
  * regionsBoard: Tabuleiro de regiões (o id de cada região é um inteiro)

  Retorna uma tupla com um booleano indicando se o puzzle foi resolvido e o estado final do tabuleiro

  Exemplo de uso:

  >>> solve [[2, 0, 0], [0, 2, 4], [6, 0, 0]] [[0, 0, 1], [0, 1, 1], [1, 1, 1]]
  (True, [[2, 3, 5], [1, 2, 4], [6, 1, 3]])
-- -}
solve :: Board -> Board -> Possibilities -> RegionsStruct -> (Bool, Board)
solve board regionsBoard possibilities regionsStruct =
    let empty = findEmpty board
    in case empty of
        Nothing -> trace "No empty positions found, puzzle solved!" (True, board)
        Just (row, col) ->
            let currentRegion = getRegionFromPosition (row, col) regionsBoard
                emptyPossibilities = possibilities !! row !! col
            -- Tenta as possibilidades da célula vazia
            in tryNumbers (row, col) emptyPossibilities board regionsStruct regionsBoard
  where
    -- Tenta números recursivamente para a posição
    tryNumbers :: Position -> [Int] -> Board -> RegionsStruct -> Board -> (Bool, Board)
    tryNumbers pos [] board _ _ = (False, board)
    tryNumbers pos (head:tail) board regionsStruct regionsBoard =
      -- Testa se o número é valido para a posição (verificando as regras do Kojun)
      if isNumberValidForThePosition head pos board regionsStruct regionsBoard False
        then
          -- Se o número for válido para a posição
              -- Atualiza o tabuleiro e chama solve para o novo tabuleiro 
          let newTable = updateBoard board pos head
              (solved, finalTable) = solve newTable regionsBoard possibilities regionsStruct
          in if solved
                -- Se for resolvido, retorna o tabuleiro e que foi resolvido
                then (True, finalTable)
                -- Se não for resolvido, ontinua tentando para os próximos (o atual pode não ser o correto para a solução final)
                else tryNumbers pos tail board regionsStruct regionsBoard
          -- Se o número não for válido, tenta outro número
        else tryNumbers pos tail board regionsStruct regionsBoard


{-
Função auxiliar que atualiza o tabuleiro, inserindo o número na posição dada

  * board: Tabuleiro a ser atualizado
  * (row, col): Posição do tabuleiro que o número será inserido
  * num: Número a ser inserido na posição
-}
updateBoard :: Board -> Position -> Int -> Board
updateBoard board (row, col) num =
  -- 
  take row board ++ [take col (board !! row) ++ [num] ++ drop (col + 1) (board !! row)] ++ drop (row + 1) board


{-
Inicializa as possibilidades de cada célula
(estrutura tridimensional que indica as possibilidades de cada célula do tabuleiro)
Essa estrutura não é atualizada durante a execução do algoritmo de backtracking, mas sim
antes da execução
Nessa estrutura, para cada célula, tem-se uma lista que indica as possibilidades de preenchimento para a célula

  * board: Tabuleiro a ser atualizado
  * regionsBoard: Tabuleiro de regiões
  * regionsStruct: Estrutura de pesquisa de posições por região

-}
initializePossibilities :: Board -> Board -> RegionsStruct -> Possibilities
initializePossibilities board regionsBoard regionsStruct = 
    [ [ initializeCell (i, j) (board !! i !! j) | j <- [0..(length (head board) - 1)] ] 
    | i <- [0..(length board - 1)] ]
  where
    initializeCell (row, col) 0 =
      let currentRegion = getRegionFromPosition (row, col) regionsBoard
          maxOfRegion = length (regionsStruct !! currentRegion)
          possibleNumbers = filter (\num -> isNumberValidForThePosition num (row, col) board regionsStruct regionsBoard True) [1..maxOfRegion]
      in possibleNumbers
    initializeCell (row, col) n = []
