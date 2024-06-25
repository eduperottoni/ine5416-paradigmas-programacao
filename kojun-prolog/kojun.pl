/* Módulos utilizados na solução */
:- use_module(library(apply)).
:- use_module(library(lists)).
:- consult('puzzles.pl').

% ====== PREDICADOS DE DEFINIÇÃO DE PUZZLE ======

% Predicado que devolve o tabuleiro original de 'puzzles.pl' a partir do ID do puzzle
originalBoard(PuzzleID, Board) :-
    puzzle(PuzzleID, Board, _).

% Predicado que devolve o tabuleiro de regiões de 'puzzles.pl' a partir do ID do puzzle
regionsBoard(PuzzleID, Regions) :-
    puzzle(PuzzleID, _, Regions).


% ====== PREDICADOS DE PRINT ======

/* Predicado para impressão de lista (recursivo) */
printList([]).
printList([H|T]) :- write(H), write(" "), printList(T).

/* Predicado para impressão de matriz (recursivo) */
printMatrix([]).
printMatrix([H|T]) :- printList(H), nl, printMatrix(T).










/* A operação "nth0()" pode retornar o enésimo valor de uma lista */
/* Como as matrizes utilizadas nessa solução são listas de listas, "buscarMatriz()" nos retorna o valor desejado */
buscarMatriz(Matriz, Linha, Coluna, Valor) :- nth0(Linha, Matriz, Lista), nth0(Coluna, Lista, Valor).

/* Percorre a lista até Posicao - 1 (length), atualiza o valor e armazena em Lista2 */
atualizarPosicao(Posicao, ValorAntigo, ValorNovo, Lista1, Lista2) :- 
    length(Pos, Posicao),
    append(Pos, [ValorAntigo|Resto], Lista1),
    append(Pos, [ValorNovo|Resto], Lista2).

/* Faz o mesmo que a função anterior, mas em uma lista de listas */
atualizarMatriz(Matriz, I, J, ValorNovo, NovaMatriz) :-
    atualizarPosicao(I, Antigo, Novo, Matriz, NovaMatriz),
    atualizarPosicao(J, _Antigo, ValorNovo, Antigo, Novo).

%-----------------------------------------------------------------------------------------------------------------------------



%-- Listas -------------------------------------------------------------------------------------------------------------------

/* Retornando todos os números presentes em uma região com base em um ID de entrada */
buscarNumerosRegiao(Matriz, RegionsBoard, IdRegiao, Valor) :-
    buscarMatriz(RegionsBoard, I, J, IdRegiao),
    buscarMatriz(Matriz, I, J, Valor).

/* Utilizando "findall()" para gerar uma lista que contenha todos os números de uma determinada região */
listaNumerosRegiao(Matriz, RegionsBoard, IdRegiao, ListaNumerosRegiao) :- 
    findall(Valor, buscarNumerosRegiao(Matriz, RegionsBoard, IdRegiao, Valor), ListaNumerosRegiao).

/* Retornando uma lista com os números que completam uma região */
/* Exemplo: seja Lista = [1, 0, 5, 2, 0], o retorno será [3, 4] */
/* Em uma região completa, o retorno é [] */
listaComplemento(Lista, ListaComplemento) :-
    length(Lista, Tamanho),
    numlist(1, Tamanho, ListaTotal),
    delete(Lista, 0, ListaResto),
    subtract(ListaTotal, ListaResto, ListaComplemento).

/* Retornando as coordenadas de uma região que possuem o valor 0 na matriz de números */
buscarCoordenadasLivres(Matriz, RegionsBoard, IdRegiao, [I, J]) :-
    buscarMatriz(RegionsBoard, I, J, IdRegiao),
    buscarMatriz(Matriz, I, J, 0).

/* Utilizando "findall()" para gerar uma lista que contenha todas as coordenadas livres de uma determinada região */
listaCoordenadasLivres(Matriz, RegionsBoard, IdRegiao, ListaCoordenadasLivres) :- 
    findall([I, J], buscarCoordenadasLivres(Matriz, RegionsBoard, IdRegiao, [I, J]), ListaCoordenadasLivres).

%-----------------------------------------------------------------------------------------------------------------------------



%-- Verificações -------------------------------------------------------------------------------------------------------------

/* Verificando se a coordenada de entrada já possui um valor ou não com base em "listaCoordenadasLivres()" */
verificarCoordenadas(Matriz, RegionsBoard, I, J) :-
    buscarMatriz(RegionsBoard, I, J, IdRegiao),
    listaCoordenadasLivres(Matriz, RegionsBoard, IdRegiao, ListaCoordenadasLivres),
    member([I, J], ListaCoordenadasLivres).

/* Retorna um valor que não esteja na região utilizando "listaComplemento()" */
verificarMembro(Matriz, RegionsBoard, I, J, Valor) :-
    buscarMatriz(RegionsBoard, I, J, IdRegiao),
    listaNumerosRegiao(Matriz, RegionsBoard, IdRegiao, ListaNumerosRegiao),
    listaComplemento(ListaNumerosRegiao, ListaValoresFaltantes),
    member(Valor, ListaValoresFaltantes).

/* Faz parte de "verificarMaiorRegiao()" */
/* Zero não é considerado maior quando está acima de um número na matriz */
verificarValor(Valor1, Valor2) :-
    Valor2 =:= 0 -> true; Valor1 < Valor2.

/* Retorna um valor que é menor que o número da posição acima dele na matriz */
/* Primeiramente é avaliado se as posições pertencem à mesma região para depois comparar os valores */
verificarMaiorRegiao(Matriz, RegionsBoard, I, J, Valor, Cima) :-
    buscarMatriz(RegionsBoard, I, J, IdRegiao),
    buscarMatriz(RegionsBoard, Cima, J, IdRegiaoCima),
    IdRegiao =:= IdRegiaoCima ->
    buscarMatriz(Matriz, Cima, J, ValorCima),
    verificarValor(Valor, ValorCima);
    buscarMatriz(Matriz, Cima, J, ValorCima),
    Valor =\= ValorCima.

/* Retorna um valor que é maior que o número da posição abaixo dele na matriz */
/* Primeiramente é avaliado se as posições pertencem à mesma região para depois comparar os valores */
verificarMenorRegiao(Matriz, RegionsBoard,I, J, Valor, Baixo) :-
    buscarMatriz(RegionsBoard, I, J, IdRegiao),
    buscarMatriz(RegionsBoard, Baixo, J, IdRegiaoBaixo),
    IdRegiao =:= IdRegiaoBaixo ->
    buscarMatriz(Matriz, Baixo, J, ValorBaixo), Valor > ValorBaixo; 
    buscarMatriz(Matriz, Baixo, J, ValorBaixo), Valor =\= ValorBaixo.

/* Dada uma coordenada, verifica se a posição acima dela na matriz é valida */
/* Se a posição de cima for válida, retorna um valor que seja menor que o número dessa posição */
verificarCima(Matriz, RegionsBoard, I, J, Valor) :-
    Cima is (I - 1), Cima >= 0 ->
    verificarMaiorRegiao(Matriz, RegionsBoard, I, J, Valor, Cima); true.

/* Dada uma coordenada, verifica se a posição abaixo dela na matriz é valida */
/* Se a posição de baixo for válida, retorna um valor que seja maior que o número dessa posição */
verificarBaixo(Matriz, RegionsBoard, I, J, Valor) :-
    length(Matriz, Tamanho),
    Baixo is (I + 1), Baixo < Tamanho ->
    verificarMenorRegiao(Matriz, RegionsBoard, I, J, Valor, Baixo); true.

/* Dada uma coordenada, retorna um valor que seja diferente do número à sua esquerda na matriz */
verificarEsquerda(Matriz, I, J, Valor) :-
    Esquerda is (J - 1), Esquerda >= 0 ->
    buscarMatriz(Matriz, I, Esquerda, ValorEsquerda),
    Valor =\= ValorEsquerda; true.

/* Dada uma coordenada, retorna um valor que seja diferente do número à sua direita na matriz */
verificarDireita(Matriz, I, J, Valor) :- 
    length(Matriz, Tamanho),
    Direita is (J + 1), Direita < Tamanho ->
    buscarMatriz(Matriz, I, Direita, ValorDireita),
    Valor =\= ValorDireita; true.

%-----------------------------------------------------------------------------------------------------------------------------



%-- Preenchendo a Matriz -----------------------------------------------------------------------------------------------------

/* Dado uma matriz e uma coordenada, retorna um valor válido para essa posição segundo as regras do jogo */
/* Com o valor válido, gera uma nova matriz de números e a retorna */
/* Caso a posição já esteja preenchida, retorna a própria matriz de entrada */
preencherPosicao(Matriz, RegionsBoard, I, J, NovaMatriz) :-
    verificarCoordenadas(Matriz, RegionsBoard, I, J) ->
    verificarMembro(Matriz, RegionsBoard, I, J, Valor),
    verificarCima(Matriz, RegionsBoard, I, J, Valor),
    verificarBaixo(Matriz, RegionsBoard, I, J, Valor),
    verificarEsquerda(Matriz, I, J, Valor),
    verificarDireita(Matriz, I, J, Valor),
    atualizarMatriz(Matriz, I, J, Valor, NovaMatriz);
    NovaMatriz = Matriz.

%-----------------------------------------------------------------------------------------------------------------------------



%-- Resolvendo o Puzzle ------------------------------------------------------------------------------------------------------

/* Função auxiliar para iterar sobre todos os elementos da matriz */
resolverKojun(Matriz, RegionsBoard, I, J, NovaMatriz) :-
    length(Matriz, N),
    (I >= N -> NovaMatriz = Matriz;  % If row index exceeds, puzzle is solved
    (J >= N -> NI is I + 1, NJ is 0, resolverKojun(Matriz, RegionsBoard, NI, NJ, NovaMatriz);  % Move to next row
    (preencherPosicao(Matriz, RegionsBoard, I, J, M),
    NJ is J + 1,
    resolverKojun(M, RegionsBoard, I, NJ, NovaMatriz)))).  % Move to next column

kojun(PuzzleID) :-
    originalBoard(PuzzleID, MNI),
    regionsBoard(PuzzleID, RegionsBoard),
    resolverKojun(MNI, RegionsBoard, 0, 0, SolvedMatrix),
    nl, printMatrix(SolvedMatrix), nl.

%-----------------------------------------------------------------------------------------------------------------------------