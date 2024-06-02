import Data.List (elemIndex)
import Data.Maybe (fromJust, isJust, isNothing)
import Control.Monad (forM_)

type Table = [[Int]]
type Position = (Int, Int)
type RegionsIndex = [[Position]]

originalTable :: Table
originalTable =
    [ [0, 0, 0, 0, 0, 2]
    , [2, 0, 0, 5, 0, 0]
    , [0, 0, 3, 0, 0, 4]
    , [0, 0, 0, 3, 0, 1]
    , [0, 0, 0, 0, 0, 0]
    , [0, 0, 3, 0, 2, 5]
    ]

regionsTable :: Table
regionsTable =
    [ [0 , 1 , 2, 2, 3, 3]
    , [0 , 1 , 4, 3, 3, 3]
    , [0 , 0 , 4, 4, 4, 5]
    , [6 , 6 , 7, 5, 5, 5]
    , [6 , 6 , 7, 8, 9, 9]
    , [10, 10, 8, 8, 8, 8]
    ]

printTable :: Table -> IO ()
printTable table = forM_ table $ \row -> do
    putStrLn $ unwords (map show row)

defineRegions :: Table -> RegionsIndex
defineRegions table =
    let maxRegion = maximum (concat table)
    in [ [(i, j) | (i, row) <- zip [0..] table, (j, cell) <- zip [0..] row, cell == region]
       | region <- [0..maxRegion]
       ]

getRegionFromPosition :: Position -> Table -> Int
getRegionFromPosition (x, y) regionsTable = regionsTable !! x !! y

getAdjacentCells :: Int -> Int -> Table -> [Int]
getAdjacentCells row col table = 
    let left  = if col > 0 then Just (table !! row !! (col - 1)) else Nothing
        right = if col < length (head table) - 1 then Just (table !! row !! (col + 1)) else Nothing
        up    = if row > 0 then Just (table !! (row - 1) !! col) else Nothing
        down  = if row < length table - 1 then Just (table !! (row + 1) !! col) else Nothing
    in [c | Just c <- [left, right, up, down]]

isNumberValid :: Int -> Position -> Table -> RegionsIndex -> Bool
isNumberValid number pos@(row, col) table regionsIndex = 
    let currentRegion = getRegionFromPosition pos regionsTable
        regionPositions = regionsIndex !! currentRegion
        regionNumbers = [table !! r !! c | (r, c) <- regionPositions]
        isUniqueInRegion = not (number `elem` regionNumbers)
        validVerticalAdjacency = all (\(r, c) -> 
            (r < row && table !! r !! c < number) ||
            (r > row && table !! r !! c > number) ||
            row == r
        ) regionPositions
        adjacentCells = getAdjacentCells row col table
        differentFromAdjacent = number `notElem` adjacentCells
    in isUniqueInRegion && validVerticalAdjacency && differentFromAdjacent

findEmpty :: Table -> Maybe Position
findEmpty table = 
    let positions = [(i, j) | (i, row) <- zip [0..] table, (j, cell) <- zip [0..] row, cell == 0]
    in if null positions then Nothing else Just (head positions)

solve :: Table -> Table -> IO Bool
solve table regionsTable = do
    let regionsIndex = defineRegions regionsTable
    let maybeEmpty = findEmpty table
    case maybeEmpty of
        Nothing -> return True
        Just empty@(row, col) -> do
            let currentRegion = getRegionFromPosition empty regionsTable
            let maxOfRegion = length (regionsIndex !! currentRegion) + 1
            tryNumbers [1..maxOfRegion] table empty regionsIndex
  where
    tryNumbers :: [Int] -> Table -> Position -> RegionsIndex -> IO Bool
    tryNumbers [] _ _ _ = return False
    tryNumbers (n:ns) table pos@(row, col) regionsIndex = do
        if isNumberValid n pos table regionsIndex
        then do
            let newTable = replaceAt row (replaceAt col n (table !! row)) table
            solved <- solve newTable regionsTable
            if solved then return True else tryNumbers ns (replaceAt row (replaceAt col 0 (table !! row)) table) pos regionsIndex
        else tryNumbers ns table pos regionsIndex

    replaceAt :: Int -> a -> [a] -> [a]
    replaceAt idx newVal list = take idx list ++ [newVal] ++ drop (idx + 1) list

main :: IO ()
main = do
    result <- solve originalTable regionsTable
    if result then putStrLn "Solved!" else putStrLn "No solution found"
    printTable originalTable