potenciacao :: Float -> Float -> Float
potenciacao x y = x ** y

main = do
    putStrLn "Informe a base: "
    baseStr <- getLine
    putStrLn "Informe o expoente"
    expStr <- getLine
    let base = (read baseStr :: Float)
    let exp = (read expStr :: Float)
    let resultado = (potenciacao base exp)
    putStrLn (baseStr ++ " elevado ao expoente " ++ expStr ++ " = " ++ (show resultado))