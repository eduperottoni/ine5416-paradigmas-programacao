module Main where

import Board (originalBoard, regionsBoard)
import Printer (printBoard)
import Solver (defineRegionsStruct, findEmpty)

main = do
    let regionsStruct = defineRegionsStruct regionsBoard
    print regionsStruct
    let empty = findEmpty originalBoard
    print empty


