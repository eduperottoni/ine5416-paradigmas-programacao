% FATOS

colete(amarelo).
colete(azul).
colete(branco).
colete(verde).
colete(vermelho).

nome(bruce).
nome(felipe).
nome(ricardo).
nome(tiago).
nome(vicente).

segmento(bancos).
segmento(educacao).
segmento(energiaEletrica).
segmento(medicamentos).
segmento(papelCelulose).

retorno(20).
retorno(30).
retorno(40).
retorno(50).
retorno(60).

corretora(cerebrolCorretora).
corretora(rcInvestimentos).
corretora(cucaynvest).
corretora(brainInvestimentos).
corretora(cucaPactual).

idade(30).
idade(35).
idade(40).
idade(45).
idade(50).


% Predicados

% X está entre de X e Z
entre(X,Y,Z,Lista) :- aDireita(X,Y,Lista),
                      aEsquerda(X,Z,Lista).

% X está à esquerda de Y (em qualquer posição à esquerda)
aEsquerda(X,Y,Lista) :- nth0(IndexX,Lista,X), 
                        nth0(IndexY,Lista,Y), 
                        IndexX < IndexY.
                        
% X está à direita de Y (em qualquer posição à direita)
aDireita(X,Y,Lista) :- aEsquerda(Y,X,Lista).


% X está exatamente à esquerda de Y
exatAEsquerda(X,Y,Lista) :- nth0(IndexX,Lista,X), 
                            nth0(IndexY,Lista,Y),
                            IndexX + 1 =:= IndexY.

% X está exatamente à direita de Y
exatADireita(X,Y,Lista) :- exatAEsquerda(Y,X,Lista).

% X está ao lado de Y
aoLado(X,Y,Lista) :- exatAEsquerda(X,Y,Lista); exatADireita(X,Y,Lista).


% X está na ponta se ele é o primeiro ou o último da lista
naPonta(X,Lista) :- last(Lista,X).
naPonta(X,[X|_]).

todosDiferentes([]).
todosDiferentes([H|T]) :- not(member(H,T)), todosDiferentes(T).


solucao(ListaSolucao) :- 

    ListaSolucao = [
        analista(Colete1, Nome1, Segmento1, Retorno1, Corretora1, Idade1),
        analista(Colete2, Nome2, Segmento2, Retorno2, Corretora2, Idade2),
        analista(Colete3, Nome3, Segmento3, Retorno3, Corretora3, Idade3),
        analista(Colete4, Nome4, Segmento4, Retorno4, Corretora4, Idade4),
        analista(Colete5, Nome5, Segmento5, Retorno5, Corretora5, Idade5)
    ],

    % O analista de colete Azul está em algum lugar à esquerda do analista que tem conta na corretora CUCA Pactual.
    aEsquerda(analista(azul, _, _, _, _, _), analista(_, _, _, _, cucaPactual, _), ListaSolucao),
    % O homem de colete Amarelo está exatamente à direita do analista que acompanha o segmento dos Bancos.
    exatADireita(analista(amarelo, _, _, _, _, _), analista(_, _, bancos, _, _, _), ListaSolucao),
    % Em uma das pontas está o analista que investe através da RC Investimentos.
    naPonta(analista(_, _, _, _, rcInvestimentos, _), ListaSolucao),
    % O homem mais velho está em algum lugar à direita do homem de colete Verde.
    aDireita(analista(_, _, _, _, _, 50), analista(verde, _, _, _, _, _), ListaSolucao),
    % Tiago está exatamente à esquerda do analista de 35 anos.
    aEsquerda(analista(_, tiago, _, _, _, _), analista(_, _, _, _, _, 35), ListaSolucao),
    % Em uma das pontas está o analista que usa a Cerebrol Corretora para investir.
    naPonta(analista(_, _, _, _, cerebrolCorretora, _), ListaSolucao),
    % Na segunda posição está o homem mais novo.
    Idade2 = 30,
    % O homem de Azul está em algum lugar entre o Bruce e o homem de 35 anos, nessa ordem.
    entre(analista(azul, _, _, _, _, _), analista(_, bruce, _, _, _, _), analista(_, _, _, _, _, 35), ListaSolucao),
    % O analista de colete Verde está em algum lugar à esquerda do analista que vislumbra um retorno de 50% no segmento no qual é especialista.
    aEsquerda(analista(verde, _, _, _, _, _), analista(_, _, _, 50, _, _), ListaSolucao),
    % O analista de colete Azul está ao lado do analista que investe através da Cucaynvest.
    aoLado(analista(azul, _, _, _, _, _), analista(_, _, _, _, cucaynvest, _), ListaSolucao),
    % Na segunda posição está o analista que acredita em uma alta de 40% no setor no qual é especialista.
    Retorno2 = 40,
    % Felipe está exatamente à direita do analista de colete Verde.
    exatADireita(analista(_, felipe, _, _, _, _), analista(verde, _, _, _, _, _), ListaSolucao),
    % O analista do segmento de Papel e Celulose está em algum lugar entre o analista de Energia Elétrica e o analista de Bancos, nessa ordem.
    entre(analista(_, _, papelCelulose, _, _, _), analista(_, _, energiaEletrica, _, _, _), analista(_, _, bancos, _, _, 35), ListaSolucao),
    % O analista de colete Amarelo está em algum lugar à esquerda do analista que acredita em uma alta de 20% no segmento no qual é especialista.
    aEsquerda(analista(amarelo, _, _, _, _, _), analista(_, _, _, 20, _, _), ListaSolucao),
    % Ricardo tem 50 anos.
    member(analista(_, ricardo, _, _, _, 50), ListaSolucao),
    % % O analista de colete Verde está ao lado do analista que investe através da CUCA Pactual.
    aoLado(analista(verde, _, _, _, _, _), analista(_, _, _, _, cucaPactual, _), ListaSolucao),
    % % O analista que acredita no maior retorno está em algum lugar à direita do analista de colete Azul.
    aDireita(analista(_, _, _, 60, _, _), analista(azul, _, _, _, _, _), ListaSolucao),
    % % O segmento de Educação é estudado pelo analista que está em algum lugar à direita do homem de colete Verde.
    aDireita(analista(_, _, educacao, _, _, _), analista(verde, _, _, _, _, _), ListaSolucao),
    % % O analista que usa a RC Investimentos é especialista no segmento de Medicamentos.
    member(analista(_, _, medicamentos, _, rcInvestimentos, _), ListaSolucao),
    % % O homem de 45 anos está ao lado do homem de Verde.
    aoLado(analista(_, _, _, _, _, 45), analista(verde, _, _, _, _, _), ListaSolucao),
    % % O analista de colete Branco espera um retorno de 30% no segmento no qual é especialista.
    member(analista(branco, _, _, 30, _, _), ListaSolucao),

    % Teste de todas as possibilidades
    colete(Colete1), colete(Colete2), colete(Colete3), colete(Colete4), colete(Colete5),
    todosDiferentes([Colete1, Colete2, Colete3, Colete4, Colete5]),
    
    nome(Nome1), nome(Nome2), nome(Nome3), nome(Nome4), nome(Nome5),
    todosDiferentes([Nome1, Nome2, Nome3, Nome4, Nome5]),
    
    segmento(Segmento1), segmento(Segmento2), segmento(Segmento3), segmento(Segmento4), segmento(Segmento5),
    todosDiferentes([Segmento1, Segmento2, Segmento3, Segmento4, Segmento5]),
    
    corretora(Corretora1), corretora(Corretora2), corretora(Corretora3), corretora(Corretora4), corretora(Corretora5),
    todosDiferentes([Corretora1, Corretora2, Corretora3, Corretora4, Corretora5]),
    
    retorno(Retorno1), retorno(Retorno2), retorno(Retorno3), retorno(Retorno4), retorno(Retorno5),
    todosDiferentes([Retorno1, Retorno2, Retorno3, Retorno4, Retorno5]),

    idade(Idade1), idade(Idade2), idade(Idade3), idade(Idade4), idade(Idade5),
    todosDiferentes([Idade1, Idade2, Idade3, Idade4, Idade5]).