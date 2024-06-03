module Main where

import Board (originalBoard, regionsBoard)
import Printer (printBoard)
import Solver (defineRegionsStruct, findEmpty, isNumberValidForThePosition)

main = do
    let regionsStruct = defineRegionsStruct regionsBoard
    print regionsStruct
    let empty = findEmpty originalBoard
    print empty
    case empty of
        Nothing -> putStrLn "No empty position found."
        Just pos -> do
            let isNumberValid = isNumberValidForThePosition 1 pos originalBoard regionsStruct regionsBoard
            print isNumberValid

