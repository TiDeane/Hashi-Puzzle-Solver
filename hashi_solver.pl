% Tiago Deane Motta e Silva, 103811

%-------------------------------------------------------------------------------
% O predicado extrai_ilhas_linha(N_L, Linha, Ilhas) tem tres parametros: N_L e'
% um inteiro positivo correspondente ao numero de uma linha e Linha e' uma lista
% correspondente a uma linha de um puzzle. Significa que Ilhas corresponde a
% lista ordenada (ilhas da esquerda para a direita) cujos elementos sao as ilha
% da linha Linha.
%-------------------------------------------------------------------------------

extrai_ilhas_linha(N_L, Linha, Ilhas) :- extrai_ilhas_linha(N_L, Linha, 1, Ilhas, []).
extrai_ilhas_linha(_, Linha, Ind, Ilhas, Ilhas) :-
	length(Linha, Len),
	Ind > Len,
	!.
extrai_ilhas_linha(N_L, Linha, Ind, Ilhas, Acc) :-
	nth1(Ind, Linha, Pontes),
	Pontes == 0,
	New_Ind is Ind + 1,
	extrai_ilhas_linha(N_L, Linha, New_Ind, Ilhas, Acc).
extrai_ilhas_linha(N_L, Linha, Ind, Ilhas, Acc) :-
	nth1(Ind, Linha, Pontes),
	Pontes \== 0,
	append(Acc, [ilha(Pontes, (N_L, Ind))], New_Acc),
	New_Ind is Ind + 1,
	extrai_ilhas_linha(N_L, Linha, New_Ind, Ilhas, New_Acc).


%-------------------------------------------------------------------------------
% O predicado ilhas(Puz, Ilhas) tem dois parametros, onde Puz e' um puzzle, e
% significa que Ilhas corresponde a lista ordenada (ilhas da esquerda para a di-
% reita e de cima para baixo) cujos elementos sao as ilhas de Puz.
%-------------------------------------------------------------------------------

ilhas(Puz, Ilhas) :- ilhas(Puz, Ilhas, 1, []).
ilhas(Puz, Ilhas, Ind, Ilhas) :-
	length(Puz, Len),
	Ind > Len,
	!.
ilhas(Puz, Ilhas, Ind, Acc) :-
	nth1(Ind, Puz, Linha),
	extrai_ilhas_linha(Ind, Linha, LAux),
	append(Acc, LAux, New_Acc),
	New_Ind is Ind + 1,
	ilhas(Puz, Ilhas, New_Ind, New_Acc).


%-------------------------------------------------------------------------------
% - O predicado vizinhas(Ilhas, Ilha, Vizinhas) tem tres parametros, onde Ilhas
% e' a lista de ilhas de um puzzle e "Ilha" corresponde a uma dessas. Significa
% que Vizinhas e' lista ordenada (ilhas de cima para baixo e da esquerda para a
% direita) cujos elementos sao as ilhas vizinhas de "Ilha".
% - O predicado vizinhas_aux devolve as ilhas que estao mesma linha ou na mesma
% coluna que "Ilha".
% - O predicado remove_intermedios remove as ilhas das mesmas colunas ou linhas
% que tem outra ilha entre ela e "Ilha".
%-------------------------------------------------------------------------------

vizinhas(Ilhas, Ilha, Vizinhas) :-     
    subtract(Ilhas, [Ilha], Sem_repetidas),
    vizinhas_aux(Sem_repetidas, Ilha, Vizinhas_temp, 1, []),
    remove_intermedios(Vizinhas_temp, Ilha, Vizinhas).

vizinhas_aux(Sem_repetidas, _, Res, Ind, Res) :-
    length(Sem_repetidas, Len),
    Ind > Len.

vizinhas_aux(Sem_repetidas, ilha(_,(Linha1,Coluna1)), Vizinhas_temp, Ind, Acc) :-
    nth1(Ind, Sem_repetidas, ilha(Pontes,(Linha2,Coluna2))),
    (Linha1 == Linha2; Coluna1 == Coluna2),
    append(Acc, [ilha(Pontes,(Linha2,Coluna2))], New_Acc),
    New_Ind is Ind + 1,
    vizinhas_aux(Sem_repetidas, ilha(_,(Linha1,Coluna1)), Vizinhas_temp, New_Ind, New_Acc).
    
vizinhas_aux(Sem_repetidas, Ilha, Vizinhas_temp, Ind, Acc) :-
    New_Ind is Ind + 1,
    vizinhas_aux(Sem_repetidas, Ilha, Vizinhas_temp, New_Ind, Acc).

remove_intermedios(Vizinhas_temp, _, Vizinhas_temp) :-
	length(Vizinhas_temp, Len),
	Len < 2.

remove_intermedios(Vizinhas_temp, Ilha, Vizinhas) :-
	remove_cima(Vizinhas_temp, Ilha, Certo_cima, 1, [], Next_Ind),
    remove_esquerda(Vizinhas_temp, Ilha, Certo_esquerda, Next_Ind, Certo_cima, [], Next_Ind2),
    remove_direita(Vizinhas_temp, Ilha, Certo_direita, Next_Ind2, Certo_esquerda, Next_Ind3),
    remove_baixo(Vizinhas_temp, Ilha, Vizinhas, Next_Ind3, Certo_direita).

remove_cima(Vizinhas_temp, _, Anterior, Ind, Anterior, Next_Ind) :-
	length(Vizinhas_temp, Len),
	Next_Ind is Ind,
	Ind > Len.

remove_cima(Vizinhas_temp, ilha(_,(Linha1,Coluna1)), Anterior, Ind, Anterior, Next_Ind) :-
	nth1(Ind, Vizinhas_temp, ilha(_,(Linha2,Coluna2))),
	Next_Ind is Ind,
	(Coluna1 \== Coluna2; Linha2 > Linha1).

remove_cima(Vizinhas_temp, ilha(_,(Linha1,Coluna1)), Certo_cima, Ind, _, Next_Ind) :-
	nth1(Ind, Vizinhas_temp, ilha(Pontes,(Linha2,Coluna2))),
	Coluna1 == Coluna2,
	Linha2 < Linha1,
	New_Ind is Ind + 1,
	remove_cima(Vizinhas_temp, ilha(_,(Linha1,Coluna1)), Certo_cima, New_Ind, [ilha(Pontes,(Linha2,Coluna2))], Next_Ind).

remove_esquerda(Vizinhas_temp, _, Certo_esquerda, Ind, Acc, Anterior, Next_Ind) :-
	length(Vizinhas_temp, Len),
	Ind > Len,
	Next_Ind is Ind,
	append(Acc, Anterior, Certo_esquerda).

remove_esquerda(Vizinhas_temp, ilha(_,(Linha1,Coluna1)), Certo_esquerda, Ind, Acc, Anterior, Next_Ind) :-
	nth1(Ind, Vizinhas_temp, ilha(_,(Linha2,Coluna2))),
	(Coluna2 > Coluna1; Linha1 \== Linha2),
	Next_Ind is Ind,
	append(Acc, Anterior, Certo_esquerda).

remove_esquerda(Vizinhas_temp, ilha(_,(Linha1,Coluna1)), Certo_esquerda, Ind, Acc, _, Next_Ind) :-
	nth1(Ind, Vizinhas_temp, ilha(Pontes,(Linha2,Coluna2))),
	Coluna2 < Coluna1,
	Linha2 == Linha1,
	New_Ind is Ind + 1,
	remove_esquerda(Vizinhas_temp, ilha(_,(Linha1,Coluna1)), Certo_esquerda, New_Ind, Acc, [ilha(Pontes,(Linha2,Coluna2))], Next_Ind).

remove_direita(Vizinhas_temp, _, Certo_direita, Ind, Certo_direita, Next_Ind) :-
	length(Vizinhas_temp, Len),
	Next_Ind is Ind,
	Ind > Len.

remove_direita(Vizinhas_temp, ilha(_,(Linha1,Coluna1)), Certo_direita, Ind, Certo_direita, Next_Ind) :-
	nth1(Ind, Vizinhas_temp, ilha(_,(Linha2,Coluna2))),
	Next_Ind is Ind,
	(Coluna2 < Coluna1; Linha1 \== Linha2).

remove_direita(Vizinhas_temp, ilha(_,(Linha1,Coluna1)), Certo_direita, Ind, Acc, Next_Ind) :-
	nth1(Ind, Vizinhas_temp, ilha(Pontes,(Linha2,Coluna2))),
	Coluna2 > Coluna1,
	Linha1 == Linha2,
	Next_Ind is Ind + 1,
	!,
	append(Acc, [ilha(Pontes,(Linha2,Coluna2))], Certo_direita).

remove_baixo(Vizinhas_temp, _, Certo_baixo, Ind, Certo_baixo) :-
	length(Vizinhas_temp, Len),
	Ind > Len.

remove_baixo(Vizinhas_temp, ilha(_,(Linha1,Coluna1)), Certo_baixo, Ind, Certo_baixo) :-
	nth1(Ind, Vizinhas_temp, ilha(_,(Linha2,Coluna2))),
	(Coluna1 \== Coluna2; Linha2 < Linha1).

remove_baixo(Vizinhas_temp, ilha(_,(Linha1,Coluna1)), Certo_baixo, Ind, Acc) :-
	nth1(Ind, Vizinhas_temp, ilha(Pontes,(Linha2,Coluna2))),
	Coluna1 == Coluna2,
	Linha2 > Linha1,
	!,
	append(Acc, [ilha(Pontes,(Linha2,Coluna2))], Certo_baixo).

%-------------------------------------------------------------------------------
% O predicado estado(Ilhas, Estado) tem dois parametros, onde Ilhas e' a lista
% de ilhas de um puzzle, e significa que Estado e' a lista ordenada cujos ele-
% mentos sao as entradas referentes a cada uma das ilhas de Ilhas.
%-------------------------------------------------------------------------------

estado(Ilhas, Estado) :- estado(Ilhas, Estado, 1, []).
estado(Ilhas, Res, Ind, Res) :-
	length(Ilhas, Len),
	Ind > Len.
estado(Ilhas, Estado, Ind, Acc) :-
	nth1(Ind, Ilhas, Ilha),
	vizinhas(Ilhas, Ilha, Vizinhas),
	New_Ind is Ind + 1,
	append(Acc, [[Ilha, Vizinhas, []]], New_Acc),
	estado(Ilhas, Estado, New_Ind, New_Acc).

%-------------------------------------------------------------------------------
% O predicado posicoes_entre(Pos1, Pos2, Posicoes) recebe tres argumentos, onde
% Pos1 e Pos2 sao posicoes, e significa que Posicoes e' a lista ordenada de po-
% sicoes entre Pos1 e Pos2 (excluindo Pos1 e Pos2). Se Pos1 e Pos2 nao pertence-
% rem a mesma linha ou a mesma coluna, o resultado e' false.
%-------------------------------------------------------------------------------

posicoes_entre((X1,Y1), (X2,Y2), _) :-
	X1 \== X2,
	Y1 \== Y2,
	!,
	fail.

posicoes_entre((X1,Y1), (X2,Y2), Posicoes) :-
	X1 > X2,
	posicoes_entre((X2,Y2), (X1,Y1), Posicoes).
	
posicoes_entre((X1,Y1), (X2,Y2), Posicoes) :-
	(X1 = X2, Y1 > Y2),
	posicoes_entre((X2,Y2), (X1,Y1), Posicoes).

posicoes_entre((X1,Y1), (X2,Y2), Posicoes) :-
	X1 == X2 -> Y_1 is Y1 + 1,
	posicoes_entre_h((X1,Y1), (X2,Y2), Posicoes, Y_1, [])
	;
	X_1 is X1 + 1,
	posicoes_entre_v((X1,Y1), (X2,Y2), Posicoes, X_1, []).

posicoes_entre_h((_,_), (_,Y2), Posicoes, Y_1, Posicoes) :-
	Y_1 >= Y2, !.

posicoes_entre_h((X1,Y1), (_,Y2), Posicoes, Y_1, Acc) :-
	Y_1 < Y2,
	append(Acc, [(X1, Y_1)], New_Acc),
	New_Y is Y_1 + 1,
	posicoes_entre_h((X1,Y1), (_,Y2), Posicoes, New_Y, New_Acc).

posicoes_entre_v((_,_), (X2,_), Posicoes, X_1, Posicoes) :-
	X_1 >= X2, !.

posicoes_entre_v((X1,Y1), (X2,_), Posicoes, X_1, Acc) :-
	X_1 < X2,
	append(Acc, [(X_1, Y1)], New_Acc),
	New_X is X_1 + 1,
	posicoes_entre_v((X1,Y1), (X2,_), Posicoes, New_X, New_Acc).

%-------------------------------------------------------------------------------
% O predicado cria_ponte(Pos1, Pos2, Ponte) recebe tres argumentos, onde Pos1 e
% Pos2 sao 2 posicoes, significa que Ponte e' uma ponte entre essas 2 posicoes.
%-------------------------------------------------------------------------------

cria_ponte((X1,Y1), (X2,Y2), ponte((X2,Y2), (X1,Y1))) :-
	X1 > X2;
	(X1 = X2,
	Y1 > Y2).
cria_ponte(Pos1, Pos2, ponte(Pos1, Pos2)).

%-------------------------------------------------------------------------------
% O predicado caminho_livre(Pos1, Pos2, Posicoes, I, Vz), recebe cinco argumen-
% tos, onde Pos1 e Pos2 sao posicoes, Posicoes e' a lista ordenada de posicoes
% entre Pos1 e Pos2, I e' uma ilha, e Vz e' uma das suas vizinhas, e significa
% que a adicao da ponte ponte(Pos1, Pos2) nao faz com que I e Vz deixem de ser
% vizinhas.
%-------------------------------------------------------------------------------

caminho_livre((X1,Y1), (X2,Y2), _, ilha(_,(X1,Y1)), ilha(_,(X2,Y2))).
caminho_livre((X1,Y1), (X2,Y2), _, ilha(_,(X2,Y2)), ilha(_,(X1,Y1))).

caminho_livre(_, _, Posicoes, ilha(_,(X1,Y1)), ilha(_,(X2,Y2))) :-
	posicoes_entre((X1,Y1), (X2,Y2), Entre),
	findall(X, (member(X, Posicoes), member(X, Entre)), Aux),
	length(Aux, Len),
	Len > 0,
	!,
	fail.

caminho_livre(_, _, Posicoes, ilha(_,(X1,Y1)), ilha(_,(X2,Y2))) :-
	posicoes_entre((X1,Y1), (X2,Y2), Entre),
	findall(X, (member(X, Posicoes), member(X, Entre)), Aux),
	length(Aux, Len),
	Len = 0.

%-------------------------------------------------------------------------------
% - O predicado actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada,
% Nova_Entrada) recebe cinco argumentos, onde Pos1 e Pos2 sao as posicoes entre
% as quais vai ser adicionada uma ponte, Posicoes e' a lista ordenada de posi-
% coes entre Pos1 e Pos2, e Entrada e' uma entrada, e significa que Nova_Entrada
% e' igual a Entrada, exceto no que diz respeito a lista de ilhas vizinhas; esta
% deve ser actualizada, removendo as ilhas que deixaram de ser vizinhas depois
% da adicao da ponte.
% - O predicado replace_aux(O, R, L1, L2) recebe quatro argumentos, onde O e' o
% elemento "original", R (replacement) e' o elemento que se quer colocar no lu-
% gar de O e L1 e' uma lista, e significa que L2 e' a lista L1 depois de substi-
% tuir todas as ocorrencias de O por R.
%-------------------------------------------------------------------------------

replace_aux(_, _, [], []).
replace_aux(O, R, [O|T], [R|T2]) :- replace_aux(O, R, T, T2).
replace_aux(O, R, [H|T], [H|T2]) :- dif(H, O), replace_aux(O, R, T, T2).

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada) :-
	actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada, 1, []).

actualiza_vizinhas_entrada(_, _, _, Entrada, Nova_Entrada, Ind, Aux) :-
	nth1(2, Entrada, Vizinhas),
	length(Vizinhas, Len),
	Ind > Len,
	replace_aux(Vizinhas, Aux, Entrada, Nova_Entrada).

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada, Ind, Aux) :-
	nth1(1, Entrada, Ilha),
	nth1(2, Entrada, Vizinhas),
	nth1(Ind, Vizinhas, Vizinha),
	\+ caminho_livre(Pos1, Pos2, Posicoes, Ilha, Vizinha),
	New_Ind is Ind + 1,
	actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada, New_Ind, Aux).

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada, Ind, Aux) :-
	nth1(1, Entrada, Ilha),
	nth1(2, Entrada, Vizinhas),
	nth1(Ind, Vizinhas, Vizinha),
	caminho_livre(Pos1, Pos2, Posicoes, Ilha, Vizinha),
	append(Aux, [Vizinha], New_Aux),
	New_Ind is Ind + 1,
	actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada, New_Ind, New_Aux).

%-------------------------------------------------------------------------------
% O predicado actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado)
% tem quatro argumentos, onde Estado e' um estado, Pos1 e Pos2 sao as posicoes
% entre as quais foi adicionada uma ponte, significa que Novo_estado e' o estado
% que se obtem de Estado depois da actualizacao das ilhas vizinhas de cada uma
% das suas entradas.
%-------------------------------------------------------------------------------

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) :-
	posicoes_entre(Pos1, Pos2, Posicoes),
	actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Posicoes, Novo_estado, 1, []).

actualiza_vizinhas_apos_pontes(Estado, _, _, _, Novo_estado, Ind, Novo_estado) :-
	length(Estado, Len),
	Ind > Len.

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Posicoes, Novo_estado, Ind, Acc) :-
	nth1(Ind, Estado, Entrada),
	actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada),
	append(Acc, [Nova_Entrada], New_Acc),
	New_Ind is Ind + 1,
	actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Posicoes, Novo_estado, New_Ind, New_Acc).

%-------------------------------------------------------------------------------
% O predicado ilhas_terminadas(Estado, Ilhas_term) tem dois argumentos, onde Es-
% tado e' um estado, e significa que Ilhas_term e' a lista de ilhas que ja tem
% todas as pontes associadas, designadas por ilhas terminadas. Se a entrada re-
% ferente a uma ilha for [ilha(N_pontes, Pos), Vizinhas, Pontes], esta ilha esta
% terminada se N_pontes for diferente de 'X' (a razao para esta condicao ficara
% aparente mais a frente) e o comprimento da lista Pontes for N_pontes.
%-------------------------------------------------------------------------------

ilhas_terminadas(Estado, Ilhas_term) :- ilhas_terminadas(Estado, Ilhas_term, 1, []).

ilhas_terminadas(Estado, Ilhas_term, Ind, Ilhas_term) :-
	length(Estado, Len),
	Ind > Len.

ilhas_terminadas(Estado, Ilhas_term, Ind, Acc) :-
	nth1(Ind, Estado, Entrada),
	nth1(1, Entrada, ilha(N_Pontes,_)),
	nth1(3, Entrada, Pontes),
	length(Pontes, Len),
	(Len \== N_Pontes; N_Pontes == 'X'),
	New_Ind is Ind + 1,
	ilhas_terminadas(Estado, Ilhas_term, New_Ind, Acc).

ilhas_terminadas(Estado, Ilhas_term, Ind, Acc) :-
	nth1(Ind, Estado, Entrada),
	nth1(1, Entrada, ilha(N_Pontes,Pos)),
	nth1(3, Entrada, Pontes),
	length(Pontes, Len),
	Len == N_Pontes,
	N_Pontes \== 'X',
	append(Acc, [ilha(N_Pontes,Pos)], New_Acc),
	New_Ind is Ind + 1,
	ilhas_terminadas(Estado, Ilhas_term, New_Ind, New_Acc).

%-------------------------------------------------------------------------------
% O predicado tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada),
% recebe tres argumentos, onde Ilhas_term e' uma lista de ilhas terminadas e En-
% trada e' uma entrada, significa que Nova_entrada e' a entrada resultante de
% remover as ilhas de Ilhas_term da lista de ilhas vizinhas de entrada.
%-------------------------------------------------------------------------------

tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada) :-
	nth1(2, Entrada, Vizinhas),
	tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada, Vizinhas, 1, []).

tira_ilhas_terminadas_entrada(_, Entrada, Nova_entrada, Vizinhas, Ind, Acc) :-
	length(Vizinhas, Len),
	Ind > Len,
	replace_aux(Vizinhas, Acc, Entrada, Nova_entrada).

tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada, Vizinhas, Ind, Acc) :-
	nth1(Ind, Vizinhas, Vizinha),
	\+ member(Vizinha, Ilhas_term),
	append(Acc, [Vizinha], New_Acc),
	New_Ind is Ind + 1,
	tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada, Vizinhas, New_Ind, New_Acc).

tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada, Vizinhas, Ind, Acc) :-
	New_Ind is Ind + 1,
	tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada, Vizinhas, New_Ind, Acc).

%-------------------------------------------------------------------------------
% O predicado tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) recebe tres
% argumentos, onde Estado e' um estado e Ilhas_term e' uma lista de ilhas termi-
% nadas, significa que Novo_estado e' o estado resultante de aplicar o predicado
% tira_ilhas_terminadas_entrada a cada uma das entradas de Estado.
%-------------------------------------------------------------------------------

tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :- tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado, 1, []).

tira_ilhas_terminadas(Estado, _, Novo_estado, Ind, Novo_estado) :-
	length(Estado, Len),
	Ind > Len.

tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado, Ind, Acc) :-
	nth1(Ind, Estado, Entrada),
	tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada),
	append(Acc, [Nova_Entrada], New_Acc),
	New_Ind is Ind + 1,
	tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado, New_Ind, New_Acc).

%-------------------------------------------------------------------------------
% O predicado marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada),
% recebe tres argumentos, onde Ilhas_term e' uma lista de ilhas terminadas e En-
% trada e' uma entrada, e significa que Nova_entrada e' a entrada obtida de En-
% trada da seguinte forma: se a ilha de Entrada pertencer a Ilhas_term, o numero
% de pontes desta e' substituido por 'X'; em caso contrario Nova_entrada e'
% igual a Entrada.
%-------------------------------------------------------------------------------

marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada) :-
	nth1(1, Entrada, ilha(N_Pontes,Pos)),
	member(ilha(N_Pontes,Pos), Ilhas_term),
	replace_aux(ilha(N_Pontes,Pos), ilha('X',Pos), Entrada, Nova_entrada).

marca_ilhas_terminadas_entrada(_, Nova_entrada, Nova_entrada).

%-------------------------------------------------------------------------------
% O predicado marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) recebe
% tres argumentos, onde Estado e' um estado e Ilhas_term e' uma lista de ilhas
% terminadas, e significa que Novo_estado e' o estado resultante de aplicar o
% predicado marca_ilhas_terminadas_entrada a cada uma das entradas de Estado.
%-------------------------------------------------------------------------------

marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :- marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado, 1, []).

marca_ilhas_terminadas(Estado, _, Novo_estado, Ind, Novo_estado) :-
	length(Estado, Len),
	Ind > Len.

marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado, Ind, Acc) :-
	nth1(Ind, Estado, Entrada),
	marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada),
	append(Acc, [Nova_Entrada], New_Acc),
	New_Ind is Ind + 1,
	marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado, New_Ind, New_Acc).

%-------------------------------------------------------------------------------
% O predicado trata_ilhas_terminadas(Estado, Novo_estado) recebe dois argumen-
% tos, onde Estado e' um estado, e significa que Novo_estado e' o estado resul-
% tante de aplicar os predicados tira_ilhas_terminadas e marca_ilhas_terminadas
% a Estado.
%-------------------------------------------------------------------------------

trata_ilhas_terminadas(Estado, Novo_estado) :- 
	ilhas_terminadas(Estado, Ilhas_term),
	trata_ilhas_terminadas(Estado, Novo_estado, Ilhas_term).

trata_ilhas_terminadas(Estado, Novo_estado, Ilhas_term) :-
	tira_ilhas_terminadas(Estado, Ilhas_term, Aux),
	marca_ilhas_terminadas(Aux, Ilhas_term, Novo_estado).

%-------------------------------------------------------------------------------
% O predicado junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado) recebe
% cinco argumentos, onde Estado e' um estado e Ilha1 e Ilha2 sao 2 ilhas, e sig-
% nifica que Novo_estado e' o estado que se obtem de Estado por adicao de
% Num_pontes pontes entre Ilha1 e Ilha2.
%-------------------------------------------------------------------------------

junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado) :-
	junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado, 1, []).

junta_pontes(Estado, _, ilha(_,Pos1), ilha(_,Pos2), Novo_estado, Ind, Acc) :-
	length(Estado, Len),
	Ind > Len,
	actualiza_vizinhas_apos_pontes(Acc, Pos1, Pos2, Estado_Aux),
	trata_ilhas_terminadas(Estado_Aux, Novo_estado).
	
junta_pontes(Estado, Num_pontes, ilha(N_Pontes1,Pos1), ilha(N_Pontes2,Pos2), Novo_estado, Ind, Acc) :-
	Num_pontes == 1,
	nth1(Ind, Estado, Entrada),
	nth1(1, Entrada, Ilha_Aux),
	(Ilha_Aux == ilha(N_Pontes1,Pos1); Ilha_Aux == ilha(N_Pontes2,Pos2)),
	cria_ponte(Pos1, Pos2, Ponte_Nova),
	nth1(3, Entrada, Pontes),
	append(Pontes, [Ponte_Nova], New_Pontes),
	replace_aux(Pontes, New_Pontes, Entrada, New_Entrada),
	append(Acc, [New_Entrada], New_Acc),
	New_Ind is Ind + 1,
	junta_pontes(Estado, Num_pontes, ilha(N_Pontes1,Pos1), ilha(N_Pontes2,Pos2), Novo_estado, New_Ind, New_Acc).

junta_pontes(Estado, Num_pontes, ilha(N_Pontes1,Pos1), ilha(N_Pontes2,Pos2), Novo_estado, Ind, Acc) :-
	Num_pontes == 1,
	nth1(Ind, Estado, Entrada),
	append(Acc, [Entrada], New_Acc),
	New_Ind is Ind + 1,
	junta_pontes(Estado, Num_pontes, ilha(N_Pontes1,Pos1), ilha(N_Pontes2,Pos2), Novo_estado, New_Ind, New_Acc).


junta_pontes(Estado, Num_pontes, ilha(N_Pontes1,Pos1), ilha(N_Pontes2,Pos2), Novo_estado, Ind, Acc) :-
	Num_pontes == 2,
	nth1(Ind, Estado, Entrada),
	nth1(1, Entrada, Ilha_Aux),
	(Ilha_Aux == ilha(N_Pontes1,Pos1); Ilha_Aux == ilha(N_Pontes2,Pos2)),
	cria_ponte(Pos1, Pos2, Ponte_Nova),
	nth1(3, Entrada, Pontes),
	append(Pontes, [Ponte_Nova, Ponte_Nova], New_Pontes),
	replace_aux(Pontes, New_Pontes, Entrada, New_Entrada),
	append(Acc, [New_Entrada], New_Acc),
	New_Ind is Ind + 1,
	junta_pontes(Estado, Num_pontes, ilha(N_Pontes1,Pos1), ilha(N_Pontes2,Pos2), Novo_estado, New_Ind, New_Acc).

junta_pontes(Estado, Num_pontes, ilha(N_Pontes1,Pos1), ilha(N_Pontes2,Pos2), Novo_estado, Ind, Acc) :-
	Num_pontes == 2,
	nth1(Ind, Estado, Entrada),
	append(Acc, [Entrada], New_Acc),
	New_Ind is Ind + 1,
	junta_pontes(Estado, Num_pontes, ilha(N_Pontes1,Pos1), ilha(N_Pontes2,Pos2), Novo_estado, New_Ind, New_Acc).

