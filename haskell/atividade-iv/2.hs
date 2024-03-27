absolute:: Float -> Int
absolute x = if x < 0 then truncate (-x) else truncate x


main = do
    putStrLn "Informe o número:"
    input <- getLine
    let original = (read input :: Float)
    let resultado = (absolute original)
    putStrLn ("O valor absoluto é " ++ (show resultado))