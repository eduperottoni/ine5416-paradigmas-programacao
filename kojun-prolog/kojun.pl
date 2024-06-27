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


% ===== PREDICADOS PARA CONSULTA/MODIFICAÇÃO DO TABULEIRO E/OU DE LISTAS =====


/* 
 * O predicado `searchOnBoard` busca um valor específico em um tabuleiro (lista de listas) dado seu índice de linha e coluna.
 * 
 * Parâmetros:
 * - Board: O tabuleiro (lista de listas) onde o valor será buscado.
 * - I: O índice da linha no tabuleiro onde o valor será procurado.
 * - J: O índice da coluna na linha especificada onde o valor será procurado.
 * - Found: O valor encontrado na posição especificada (I, J) do tabuleiro.
 */
searchOnBoard(Board, I, J, Found) :- 
    nth0(I, Board, Line),
    nth0(J, Line, Found).



/* 
 * O predicado `updatePosition` substitui um valor em uma posição específica de uma lista por um novo valor.
 * 
 * Parâmetros:
 * - Position: O índice da posição na lista onde o valor será substituído.
 * - CurrentValue: O valor atual na posição especificada.
 * - NewValue: O novo valor que substituirá o valor atual na posição especificada.
 * - CurrentList: A lista original.
 * - NewList: A lista resultante após a substituição do valor.
 */
updatePosition(Position, NewValue, CurrentList, NewList) :- 
    % Cria uma lista Prefix de comprimento Position
    length(Prefix, Position),
    % Divide CurrentList em duas partes (antes de Position e o restante).
    % Prefix recebe o s valores anteriores
    append(Prefix, [_|Suffix], CurrentList),
    % NewList recebe lista atualizada
    append(Prefix, [NewValue|Suffix], NewList).



/*
 * O predicado `updateBoard` substitui um valor em uma posição específica de um tabuleiro (lista de listas) por um novo valor.
 * 
 * Parâmetros:
 * - Board: O tabuleiro (lista de listas) original.
 * - I: O índice da linha no tabuleiro onde o valor será substituído.
 * - J: O índice da coluna na linha especificada onde o valor será substituído.
 * - NewValue: O novo valor que substituirá o valor atual na posição especificada.
 * - NewBoard: O tabuleiro resultante após a substituição do valor.
 */
updateBoard(Board, I, J, NewValue, NewBoard) :-
    % Divide Board em OldRow e RestRows
    nth0(I, Board, OldRow, RestRows),
    updatePosition(J, NewValue, OldRow, NewRow),
    % Insere a linha atualizada no tabuleiro
    nth0(I, NewBoard, NewRow, RestRows).


/* 
 * O predicado `searchNumbersOnRegion` encontra um valor em uma região específica de um tabuleiro com base em uma identificação de região.
 * 
 * Parâmetros:
 * - Board: O tabuleiro (lista de listas) onde os valores são procurados.
 * - RegionsBoard: O tabuleiro (lista de listas) que mapeia as regiões.
 * - RegionId: A identificação da região cujo valor será procurado.
 * - Result: O valor encontrado na posição correspondente à identificação da região.
 */
searchNumberOnRegion(Board, RegionsBoard, RegionId, Result) :-
    % Duas chamadas fazem com que todas as posições da região sejam visitadas
    searchOnBoard(RegionsBoard, I, J, RegionId),
    searchOnBoard(Board, I, J, Result).


/*
 * O predicado `listNumbersOnRegion` encontra todos os valores em um tabuleiro que correspondem a uma região específica identificada por RegionId no RegionsBoard.
 * 
 * Parâmetros:
 * - Board: A matriz (lista de listas) onde os valores são procurados.
 * - RegionsBoard: A matriz (lista de listas) que mapeia as regiões.
 * - RegionId: A identificação da região cujos valores serão procurados.
 * - FinalList: A lista de todos os valores encontrados na região especificada.
 */
listNumbersOnRegion(Board, RegionsBoard, RegionId, FinalList) :- 
    % 'findall' retorna todos os FoundNumbers na região RegionId
    findall(FoundNumber, searchNumberOnRegion(Board, RegionsBoard, RegionId, FoundNumber), FinalList).



/*
 * O predicado `complement` retorna uma lista com os números que faltam para completar uma sequência de números contíguos, com base nos elementos não zero da lista dada.
 * 
 * Exemplos:
 * - Se List = [1, 0, 5, 2, 0], então Complement será [3, 4].
 * - Se List contém todos os números de 1 a N sem zeros, o retorno é [].
 * 
 * Parâmetros:
 * - List: A lista de números, possivelmente com zeros.
 * - Complement: A lista de números que estão faltando para completar a sequência contígua.
 */
complement(List, Complement) :-
    length(List, Length),
    % Cria uma lista com todos os números de 1 até Lenght
    numlist(1, Length, UniverseList),
    % Deleta as ocorrências de zero (restando os números preenchidos)
    delete(List, 0, RestList),
    subtract(UniverseList, RestList, Complement).



/*
 * O predicado `searchZeroedPositionOnRegion` encontra a posição [I, J] onde um valor zero está presente em uma região específica identificada por RegionId tanto em RegionsBoard quanto em Board.
 * 
 * Parâmetros:
 * - Board: O tabuleiro (lista de listas) onde os valores são procurados.
 * - RegionsBoard: O tabuleiro (lista de listas) que mapeia as regiões.
 * - RegionId: A identificação da região onde se deseja encontrar a posição com valor zero.
 * - [I, J]: A posição [I, J] onde o valor zero foi encontrado na região identificada por RegionId.
 */
searchZeroedPositionOnRegion(Board, RegionsBoard, RegionId, [I, J]) :-
    searchOnBoard(RegionsBoard, I, J, RegionId),
    searchOnBoard(Board, I, J, 0).




/* 
 * O predicado `listZeroedPositionsOnRegion` utiliza `findall()` para gerar uma lista que contém todas as coordenadas [I, J] onde um valor zero está presente em uma determinada região identificada  * por RegionId em ambos Board e RegionsBoard.
 * 
 * Parâmetros:
 * - Board: O tabuleiro (lista de listas) onde os valores são procurados.
 * - RegionsBoard: O tabuleiro (lista de listas) que mapeia as regiões.
 * - RegionId: A identificação da região onde se deseja encontrar as coordenadas com valor zero.
 * - ZeroedPositions: A lista de todas as coordenadas [I, J] onde o valor zero foi encontrado na região identificada por RegionId.
 */
listZeroedPositionsOnRegion(Board, RegionsBoard, RegionId, ZeroedPositions) :- 
    findall([I, J], searchZeroedPositionOnRegion(Board, RegionsBoard, RegionId, [I, J]), ZeroedPositions).



% ====== PREDICADOS PARA A VERIFICAÇÃO DE REGRAS ================


/* 
 * O predicado `verifyZeroed` verifica se a coordenada [I, J] em um tabuleiro (Board) já possui um valor atribuído, com base nas posições que contêm zeros na região identificada por RegionId em ambos Board e RegionsBoard.
 * 
 * Parâmetros:
 * - Board: O tabuleiro (lista de listas) onde os valores são verificados.
 * - RegionsBoard: O tabuleiro (lista de listas) que mapeia as regiões.
 * - I: A coordenada da linha onde verificar se há um zero.
 * - J: A coordenada da coluna onde verificar se há um zero.
 */
verifyZeroed(Board, RegionsBoard, I, J) :-
    searchOnBoard(RegionsBoard, I, J, RegionId),
    listZeroedPositionsOnRegion(Board, RegionsBoard, RegionId, ListOfZeroedPositions),
    member([I, J], ListOfZeroedPositions).



/*
 * Descrição:
 * O predicado `getPossibleForPosition` retorna um valor possível que não esteja presente na região identificada por RegionId utilizando o predicado `complement`.
 * 
 * Parâmetros:
 * - Board: O tabuleiro (lista de listas) onde os valores são verificados.
 * - RegionsBoard: O tabuleiro (lista de listas) que mapeia as regiões.
 * - I: A coordenada da linha onde deseja-se encontrar um valor possível.
 * - J: A coordenada da coluna onde deseja-se encontrar um valor possível.
 * - PossibleValue: O valor possível que não está presente na região identificada por RegionId.
 */
getPossibleForPosition(Board, RegionsBoard, I, J, PossibleValue) :-
    searchOnBoard(RegionsBoard, I, J, RegionId),
    listNumbersOnRegion(Board, RegionsBoard, RegionId, RegionNumbers),
    complement(RegionNumbers, Complement),
    member(PossibleValue, Complement).



/*
 * O predicado `verificarValor` compara dois valores (`Valor1` e `Valor2`) para determinar se `Valor1` é menor que `Valor2`, com a condição especial de que o valor `0` não é considerado maior que nenhum outro valor.
 * 
 * Parâmetros:
 * - BelowValue: Valor abaixo
 * - AboveValue: Valor acima
 */
verifyAboveGreater(BelowValue, AboveValue) :-
    AboveValue =:= 0 -> true; BelowValue < AboveValue.



/* O predicado `verifyAbove` verifica se o valor na posição `[I, J]` na matriz é menor que o valor na posição acima `[IAbove, J]`. Primeiro, verifica se as posições `[I, J]` e `[IAbove, J]` pertencem à mesma região em
 * `RegionsBoard`. Se pertencerem, compara os valores em `Board` utilizando o predicado `verifyAboveGreater/2`. Caso não pertençam à mesma região, compara diretamente os valores sem considerar a região.
 * 
 * Parâmetros:
 * - Board: A matriz onde as posições são verificadas.
 * - RegionsBoard: O tabuleiro que mapeia as regiões.
 * - I: A coordenada da linha na matriz `[I, J]`.
 * - J: A coordenada da coluna na matriz `[I, J]`.
 * - Value: O valor na posição `[I, J]`.
 * - IAbove: A linha acima da posição `[I, J]` na matriz.
 */
verifyAbove(Board, RegionsBoard, I, J, Value, IAbove) :-
    searchOnBoard(RegionsBoard, I, J, RegionId),
    searchOnBoard(RegionsBoard, IAbove, J, AboveRegionId),
    % Verifica se o número acima está na mesma região
    RegionId =:= AboveRegionId ->
    % Caso pertença a mesma região
    searchOnBoard(Board, IAbove, J, AboveValue),
    verifyAboveGreater(Value, AboveValue);
    % Caso não pertença a mesma região
    searchOnBoard(Board, IAbove, J, AboveValue),
    Value =\= AboveValue.



/* Retorna um valor que é maior que o número da posição abaixo dele na matriz */
/* Primeiramente é avaliado se as posições pertencem à mesma região para depois comparar os valores */
verifyBelow(Board, RegionsBoard, I, J, Value, IBelow) :-
    searchOnBoard(RegionsBoard, I, J, RegionId),
    searchOnBoard(RegionsBoard, IBelow, J, IdRegiaoBaixo),
    RegionId =:= IdRegiaoBaixo ->
    % Sempre deve ser diferent e se estiver na mesma região deve ser maior que o valor abaixo
    searchOnBoard(Board, IBelow, J, BelowValue), Value > BelowValue; 
    searchOnBoard(Board, IBelow, J, BelowValue), Value =\= BelowValue.

/*
 * O predicado `verifyUp` verifica se a posição acima de uma coordenada [I, J] em uma matriz é válida.
 * Se for válida, chama o predicado `verifyAbove` para encontrar um valor que seja menor que o número nessa posição.
 * 
 * Parâmetros:
 * - Board: o tabuleiro.
 * - RegionsBoard: O tabuleiro que mapeia as regiões.
 * - I: A coordenada da linha na matriz.
 * - J: A coordenada da coluna na matriz.
 * - PossibleValue: O valor encontrado que é menor que o número na posição acima de [I, J].
 */
verifyUp(Board, RegionsBoard, I, J, PossibleValue) :-
    IAbove is (I - 1), IAbove >= 0 ->
    verifyAbove(Board, RegionsBoard, I, J, PossibleValue, IAbove); true.

/*
 * O predicado `verifyDown` verifica se a posição abaixo da coordenada `[I, J]` na matriz `Board` é válida. Se for válida (ou seja, `I + 1` é menor que o comprimento da matriz), chama o predicado `verifyBelow/6` para comparar os 
 * valores na posição `[I, J]` com a posição abaixo `[IBelow, J]` na mesma região em `RegionsBoard`. Caso contrário, retorna verdadeiro (true).
 * 
 * Parâmetros:
 * - Board: A matriz onde as posições são verificadas.
 * - RegionsBoard: O tabuleiro que mapeia as regiões.
 * - I: A coordenada da linha na matriz `[I, J]`.
 * - J: A coordenada da coluna na matriz `[I, J]`.
 * - PossibleValue: O valor que é maior que o número da posição abaixo `[IBelow, J]`.
 */
verifyDown(Board, RegionsBoard, I, J, PossibleValue) :-
    length(Board, Lenght),
    IBelow is (I + 1), IBelow < Lenght ->
    verifyBelow(Board, RegionsBoard, I, J, PossibleValue, IBelow); true.



/*
 * O predicado `verifyLeft` retorna um valor `Value` que é diferente do número à esquerda da coordenada `[I, J]` na matriz `Board`. Primeiramente, verifica se a posição à esquerda `[I, Left]` é válida (ou seja, `J - 1 >= 0`).
 * Se for válida, verifica o valor na posição à esquerda utilizando `searchOnBoard/4`. Retorna verdadeiro se `Value` for diferente de `LeftValue`. Caso contrário, retorna verdadeiro.
 * 
 * Parâmetros:
 * - Board: A matriz onde as posições são verificadas.
 * - I: A coordenada da linha na matriz `[I, J]`.
 * - J: A coordenada da coluna na matriz `[I, J]`.
 * - Value: O valor que é diferente do número à esquerda na posição `[I, J]`.
 */
verifyLeft(Board, I, J, Value) :-
    Left is (J - 1), Left >= 0 ->
    searchOnBoard(Board, I, Left, LeftValue),
    Value =\= LeftValue; true.

/* 
 * O predicado `verifyRight` retorna um valor `Value` que é diferente do número à direita da coordenada `[I, J]` na matriz `Board`. Primeiramente, verifica o comprimento da matriz `Board` com `length/2` para obter `Lenght`.
 * Em seguida, verifica se a posição à direita `[I, IRight]` é válida (ou seja, `J + 1 < Lenght`). Se for válida, verifica o valor na posição à direita utilizando `searchOnBoard/4`.
 * Retorna verdadeiro se `Value` for diferente de `RightValue`. Caso contrário, retorna verdadeiro.
 * 
 * Parâmetros:
 * - Board: A matriz onde as posições são verificadas.
 * - I: A coordenada da linha na matriz `[I, J]`.
 * - J: A coordenada da coluna na matriz `[I, J]`.
 * - Value: O valor que é diferente do número à direita na posição `[I, J]`.
 */
verifyRight(Board, I, J, Value) :- 
    length(Board, Lenght),
    IRight is (J + 1), IRight < Lenght ->
    searchOnBoard(Board, I, IRight, RightValue),
    Value =\= RightValue; true.

% ======= PREENCHIMENTO DO TABULEIRO ==========

/* O predicado `fillPosition` preenche uma posição `[I, J]` na matriz `Board` com um valor válido segundo as regras do jogo. Primeiramente, verifica se a posição `[I, J]` está zerada utilizando `verifyZeroed/4`. 
 * Se estiver zerada, determina um `PossibleValue` válido para essa posição utilizando `getPossibleForPosition/5`.
 * Em seguida, verifica se esse `PossibleValue` é válido nas direções acima, abaixo, à esquerda e à direita utilizando os predicados `verifyUp/4`, `verifyDown/5`, `verifyLeft/4` e `verifyRight/4`. 
 * Se todas as verificações forem bem-sucedidas, atualiza a matriz `Board` com `PossibleValue` utilizando `updateBoard/4` e retorna a `NewBoard` atualizada. Caso a posição não esteja zerada, retorna a própria `Board`.
 * 
 * Parâmetros:
 * - Board: A matriz onde as posições são preenchidas.
 * - RegionsBoard: O tabuleiro que mapeia as regiões.
 * - I: A coordenada da linha na matriz `[I, J]`.
 * - J: A coordenada da coluna na matriz `[I, J]`.
 * - NewBoard: A nova matriz de números gerada após o preenchimento da posição `[I, J]`.
 */
fillPosition(Board, RegionsBoard, I, J, NewBoard) :-
    verifyZeroed(Board, RegionsBoard, I, J) ->
    getPossibleForPosition(Board, RegionsBoard, I, J, PossibleValue),
    % Verifica validade para todas as posições adjacentes
    verifyUp(Board, RegionsBoard, I, J, PossibleValue),
    verifyDown(Board, RegionsBoard, I, J, PossibleValue),
    verifyLeft(Board, I, J, PossibleValue),
    verifyRight(Board, I, J, PossibleValue),
    % Atualiza o tabuleiro
    updateBoard(Board, I, J, PossibleValue, NewBoard);
    % Se a posição não estiver zerada, retorna o próprio Boaard
    NewBoard = Board.


% ==== RESOLUÇÃO DO PUZZLE ==========

/*
 * O predicado `solveKojun` é uma função auxiliar que resolve o que parece ser um quebra-cabeça ou jogo representado por `OriginalBoard` e `RegionsBoard`.
 * Ele itera sobre todos os elementos da matriz `OriginalBoard`, preenchendo cada posição com um valor válido utilizando o predicado `fillPosition/5`.
 * O processo continua até que todas as posições estejam preenchidas conforme as regras do jogo.
 * 
 * Parâmetros:
 * - OriginalBoard: A matriz que representa o quebra-cabeça a ser resolvido.
 * - RegionsBoard: O tabuleiro que mapeia as regiões da matriz.
 * - I: Índice da linha atual na matriz `OriginalBoard`.
 * - J: Índice da coluna atual na matriz `OriginalBoard`.
 * - NovaOriginalBoard: A nova matriz resultante após a resolução do quebra-cabeça.
 */
solveKojun(OriginalBoard, RegionsBoard, I, J, SolvedBoard) :-
    length(OriginalBoard, N),
    (I >= N -> SolvedBoard = OriginalBoard; % All positions are filled
    (J >= N -> NI is I + 1, NJ is 0, solveKojun(OriginalBoard, RegionsBoard, NI, NJ, SolvedBoard);  % Move to next row
    (fillPosition(OriginalBoard, RegionsBoard, I, J, UpdatedBoard),
    NJ is J + 1, % Goes to next col
    solveKojun(UpdatedBoard, RegionsBoard, I, NJ, SolvedBoard)))).  % Move to next column



/*
 * O predicado `kojun` resolve um quebra-cabeça identificado por `PuzzleID`. Ele obtém a matriz original do quebra-cabeça utilizando `originalBoard/2`, o tabuleiro de regiões utilizando `regionsBoard/2`,
 * e então chama o predicado `resolverKojun/5` para resolver o quebra-cabeça. Após resolver, imprime a matriz resolvida utilizando `printMatrix/1`.
 * 
 * Parâmetros:
 * - PuzzleID: Identificador único do quebra-cabeça a ser resolvido.
 */
kojun(PuzzleID) :-
    originalBoard(PuzzleID, OriginalBoard),
    regionsBoard(PuzzleID, RegionsBoard),
    solveKojun(OriginalBoard, RegionsBoard, 0, 0, SolvedMatrix),
    nl, printMatrix(SolvedMatrix), nl.