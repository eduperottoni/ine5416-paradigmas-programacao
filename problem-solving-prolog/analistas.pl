% FATOS

colete(amarelo)
colete(azul)
colete(branco)
colete(verde)
colete(vermelho)

nome(bruce)
nome(felipe)
nome(ricardo)
nome(tiago)
nome(vicente)

segmento(bancos)
segmento(educacao)
segmento(energiaEletrica)
segmento(medicamentos)
segmento(papelCelulose)

retorno(r20)
retorno(r30)
retorno(r40)
retorno(r50)
retorno(r60)

corretora(cerebrolCorretora)
corretora(rcInvestimentos)
corretora(cucaynvest)
corretora(brainInvestimentos)
corretora(cucaPactual)

idade(i30)
idade(i35)
idade(i40)
idade(i45)
idade(i50)


% Predicados

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
exatADireita(X,Y,Lista) :- nth0(IndexX,Lista,X), 
                           nth0(IndexY,Lista,Y),
                           IndexX - 1 =:= IndexY.


solucao(ListaSolucao) :- 

    ListaSolucao = [
        analista(Colete1, Nome1, Segmento1, Retorno1, Corretora1, Idade1),
        analista(Colete2, Nome2, Segmento2, Retorno2, Corretora2, Idade2),
        analista(Colete3, Nome3, Segmento3, Retorno3, Corretora3, Idade3),
        analista(Colete4, Nome4, Segmento4, Retorno4, Corretora4, Idade4),
        analista(Colete5, Nome5, Segmento5, Retorno5, Corretora5, Idade5)
    ],