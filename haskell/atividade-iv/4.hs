xor:: Bool -> Bool -> Bool
xor a b = (a || b) && (not (a && b))

convertToBool:: Char -> Bool
convertToBool 'T' = True
convertToBool 'F' = False

boolToString:: Bool -> String
boolToString x = if x then "True" else "False"

main = do
    putStrLn "Informe o primeiro valor:"
    input <- getLine
    let pChar = (read input::Char)
    let p = convertToBool pChar
    putStrLn "Informe o segundo valor:"
    input2 <- getLine
    let sChar = (read input2::Char)
    let s = convertToBool pChar

    let resultado = xor p s
    putStrLn ((show p) ++ "XOR" ++ (show s) ++ "=" ++ (boolToString resultado))