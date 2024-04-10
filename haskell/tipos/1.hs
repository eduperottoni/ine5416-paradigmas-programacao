type Nota = Float
type Disciplina = String
type Nome = String
data Notas = Notas Nota Nota Nota;
type Aluno = (Nome, Disciplina, Notas)

aluno :: Int -> Aluno
aluno 1 = ("Eduardo", "INE5424", Notas 9 8.5 10)
aluno 2 = ("Joana", "INE5419", Notas 5 2 0.6)
aluno 3 = ("Maria", "INE5413", Notas 6 4.5 9)
aluno 4 = ("Amarildo", "INE5443", Notas 7 5.6 10)

getNome :: Aluno -> Nome
getNome (n, _, _) = n

calculaMedia :: Aluno -> Nota
calculaMedia (_, _, Notas a b c) = (a + b + c) / 3

getMediaFromId :: Int -> Nota
getMediaFromId n = calculaMedia (aluno n)

main = do
    putStrLn (show (getNome (aluno 4)))
    putStrLn (show (getMediaFromId 3))