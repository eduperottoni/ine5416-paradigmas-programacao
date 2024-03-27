triangleArea:: Float -> Float -> Float
triangleArea base altura = (base * altura) / 2 

main = do
    putStrLn "Informe a base do triângulo"
    input <- getLine
    let base = (read input :: Float)
    putStrLn "Informe a altura do triângulo"
    input <- getLine
    let altura = (read input :: Float)
    let area = triangleArea base altura
    putStrLn ("A área do triângulo é: " ++ (show area))