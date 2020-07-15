:- [exemplos_puzzles].

%propaga/3
%Recebe um puzzle e uma posicao, e devolve o conjunto de elementos do termemómetro ao qual o elemento pertence
propaga(Puz, Pos, Posicoes) :-
    tabuleiro(Puz, Tab),
    sub_member(Tab, Pos, Termometro),
    nth1(Indice, Termometro, Pos),
    slice(Termometro, Res, Indice),
    sort(Res, Posicoes), !.

%tabuleiro/2
%Recebe um puzzle e devolve os termómetros do puzzle
tabuleiro([Tab, _, _], Tab).

%sub_member/3
%Recebe um tabuleiro e uma posicao e devolve o termometro ao qual a posicao pertence
sub_member([P|_], Pos, P) :-
    member(Pos, P).

sub_member([_|R], Pos, X) :-
    sub_member(R, Pos, X).

%slice/3
%Recebe uma lista e um indice e devolve a lista desde o primeiro elemento ate esse indice
slice([P|R1], [P|R2], Y) :-
    Y > 1,
    NY is Y - 1,
    slice(R1, R2, NY).

slice([P|_], [P], Y) :-
    Y = 1.

%nao_altera_linhas_anteriores/3
%Recebe uma lista de posicoes, uma linha e uma lista de posicoes ja preenchidas e averigua
%se todas as posicoes dadas pertencentes a linhas anteriores estao nas ja preenchidas
nao_altera_linhas_anteriores([], _, _).
nao_altera_linhas_anteriores([P|R], L, Ja_Preenchidas) :-
    linha(P, Linha),
    Linha < L,
    member(P, Ja_Preenchidas),
    nao_altera_linhas_anteriores(R, L, Ja_Preenchidas), !.

nao_altera_linhas_anteriores([P|R], L, Ja_Preenchidas) :-
    linha(P, Linha),
    Linha >= L,
    nao_altera_linhas_anteriores(R, L, Ja_Preenchidas), !.

%linha/2
%Recebe um tuplo ou uma lista de tuplos e devolve a linha desse tuplo ou do primeiro tuplo da lista
linha((A, _), A).
linha([(A,_)|_], A).

%coluna/2
%Recebe um tuplo ou uma lista de tuplos e devolve a coluna desse tuplo ou do primeiro tuplo da lista
coluna((_,A), A).
coluna([(A,_)|_], A).


%verifica_parcial/4
%Recebe um puzzle, uma lista de posicoes ja preenchidas, uma dimensao e uma lista de posicoes
%Averigua se as posicoes nao excedem os limites de cada coluna tendo em conta as posicoes anteriores
verifica_parcial(Puz, Ja_Preenchidas, _, Poss) :-
    union(Ja_Preenchidas, Poss, Total),
    colunas(Puz, Colunas),
    reduz_valores(Colunas, Total), !.

%linhas/2
%Recebe um puzzle e devolve a lista dos limites das linhas desse puzzle
linhas([_,A,_], A).
%colunas/2
%Recebe um puzzle e devolve a lista dos limites de cada coluna desse puzzle
colunas([_,_,A], A).

%reduz_valores/2
%Recebe uma lista e uma lista de posicoes e reduz na lista das colunas o numero de elementos
reduz_valores(_, []).
reduz_valores(Colunas, [P|R]) :-
    coluna(P, Indice),
    reduz_valor(Colunas, Indice, NColunas),
    reduz_valores(NColunas, R).

%reduz_valor/3
%Recebe uma lista, um indice e retorna a lista alterada
%Reduz no indice dado da lista dada o valor, guardando na lista alterada
reduz_valor([], _, _).
reduz_valor([A|R], Indice, [NA|R]) :-
    Indice = 1,
    NA is A - 1,
    NA >= 0.

reduz_valor([A|R1], Indice, [A|R2]) :-
    Indice > 1,
    NI is Indice - 1,
    reduz_valor(R1, NI, R2).


%possibilidades_linha/5
%Recebe um puzzle, uma lista de posicoes da linha, um total, uma lista de posicoes ja_preenchidas
%Devolve a lista de posicoes possiveis para garantir o preenchimento correto da linha
possibilidades_linha(Puz, Posicoes_Linha, Total, Ja_Preenchidas, Possibilidades_L) :-
    findall(X, combinacao(Total, Posicoes_Linha, X), Combinacoes),
    linha(Posicoes_Linha, Linha),
    propaga_verifica(Puz, Combinacoes, [], Combinacoes_propagadas), !,
    limita_tamanho(Combinacoes_propagadas, Total, Linha, Combinacoes_Limitadas),!,
    limites_linha(Combinacoes_Limitadas, Total, Linha, Ja_Preenchidas, Combinacoes_Boas),
    parcial_verifica(Combinacoes_Boas, Ja_Preenchidas, Puz, Combinacoes_verificadas), !,
    anteriores_verifica(Ja_Preenchidas, Linha, Combinacoes_verificadas, Res_Desordenado), 
    sort(Res_Desordenado, Possibilidades_L).

%combinacao/3
%Recebe um indice e uma lista de elementos e retorna uma lista de combinacao desses elementos
combinacao(0, _, []).
combinacao(N, L, [E | C_L_E]) :-
    N > 0,
    append(_, [E | L_apos_E], L),
    N_1 is N - 1,
    combinacao(N_1, L_apos_E, C_L_E).
    
%parcial_verifica/4
%Recebe uma lista de listas, uma lista de posicoes ja_preenchidas e um puzzle
%Retorna a uma lista com as sublistas que verificam a condicao verifica_parcial
parcial_verifica([], _, _, []).
parcial_verifica([P|R1], Ja_Preenchidas, Puz, [P|R2]) :-
    verifica_parcial(Puz, Ja_Preenchidas, _, P),
    parcial_verifica(R1, Ja_Preenchidas, Puz, R2).

parcial_verifica([_|R1], Ja_Preenchidas, Puz, R2) :-
    parcial_verifica(R1, Ja_Preenchidas, Puz, R2).


%anteriores_verifica/4
%Recebe uma lista de posicoes ja preenchidas, uma linha, uma lista de posicoes para testar
%Devolve a lista com as posicoes que verificam a condicao nao_altera_linhas_anteriores
anteriores_verifica(_,_, [], []).
anteriores_verifica(Ja_Preenchidas, Linha, [P|R1], [P|R2]) :-
    nao_altera_linhas_anteriores(P, Linha, Ja_Preenchidas),
    anteriores_verifica(Ja_Preenchidas, Linha, R1, R2).

anteriores_verifica(Ja_Preenchidas, Linha, [_|R1], R2) :-
    anteriores_verifica(Ja_Preenchidas, Linha, R1, R2).

%propaga_verifica/4
%Recebe um puzzle, uma lista, uma lista de valores ja verificados e devolve o resultado de propagar 
%cada conjunto de posicoes separadamente
propaga_verifica(_, [], A, A).
propaga_verifica(Puz, [P|R1], Ac, Res) :-
    propaga_elementos(Puz, P, [], Ac2),
    sort(Ac2, Ac3),
    append(Ac, [Ac3], Novo_Ac),
    propaga_verifica(Puz, R1, Novo_Ac, Res).

%propaga_elementos/4
%Recebe um puzzle, uma lista de posicoes e uma lista de posicoes ja propagadas
%devolve a lista resultante de propagar cada posicoes juntamente com as ja propagadas
propaga_elementos(_, [], Ac, Ac).
propaga_elementos(Puz, [P|R1], Ac, Poss) :-
    propaga(Puz, P, Ac2),
    union(Ac, Ac2, Novo_Ac),
    propaga_elementos(Puz, R1, Novo_Ac, Poss).

%limita_tamanho/4
%Recebe uma lista, um total, e uma linha e devolve os elementos que contem elementos da linha = total
limita_tamanho([], _, _, []).
limita_tamanho([P|R1], T, Linha, [P|R2]) :-
    elementos_tamanho(P, Linha, N),
    N = T,
    limita_tamanho(R1, T, Linha, R2), !.
limita_tamanho([_|R], T, Linha, R2) :-
    limita_tamanho(R, T, Linha, R2).


%elementos_tamanho/3
%Recebe uma lista de elementos e uma linha e devolve o numero de elementos pertencentes a linha dada
elementos_tamanho(Lista, Linha, Res) :-
    elementos_tamanho(Lista, Linha, 0, Res).
elementos_tamanho([], _, Ac, Ac).
elementos_tamanho([P|R], Linha, Ac, Res) :-
    linha(P, Linha),
    Ac2 is Ac+1, !,
    elementos_tamanho(R, Linha, Ac2, Res).
elementos_tamanho([_|R], Linha, Ac, Res) :-
    elementos_tamanho(R, Linha, Ac, Res).

%limites_linha/5
%Recebe uma lista de listas, um total, uma linha, uma lista de posicoes ja preenchidas
%Devolve uma lista com as sublistas que verificam o limite de elementos da linha
limites_linha([], _, _, _, []).
limites_linha([P|R1], Total, Linha, Ja_Preenchidas, [P|R2]) :-
    union(P, Ja_Preenchidas, Temp),
    elementos_tamanho(Temp, Linha, Total2),
    Total = Total2,
    limites_linha(R1, Total, Linha, Ja_Preenchidas, R2).
limites_linha([_|R1], Total, Linha, Ja_Preenchidas, R2) :-
    limites_linha(R1, Total, Linha, Ja_Preenchidas, R2).
    
%resolve/2
%Recebe um puzzle e devolve a solucao do puzzle
resolve(Puz, Solucao) :-
        linhas(Puz, Linhas),
        length(Linhas, MaxLinha),
        resolve_aux(Puz, 1, MaxLinha, [], Solucao).

%resolve_aux/5
%Recebe um puzzle, uma linha, o maximo de elementos da linha a preencher, uma lista de posicoes ja preenchidas
%Devolve a solucao de todas as linhas desde a atual
resolve_aux(_, R, L, S, Sol) :-
    R is L + 1,
    sort(S, Sol).

resolve_aux(Puz, Linha, MaxLinha, Ja_Preenchidas, Solucao) :-
    gera_posicoes_linha(Linha, MaxLinha, Posicoes_Linha),
    linhas(Puz, Linhas),
    nth1(Linha, Linhas, N_Posicoes_Linha),
    possibilidades_linha(Puz, Posicoes_Linha, N_Posicoes_Linha, Ja_Preenchidas, 
        Possibilidades_Linha), !,
    Nova_Linha is Linha + 1,
    tenta_possibilidades(Puz, Nova_Linha, MaxLinha, Ja_Preenchidas, Possibilidades_Linha, Solucao).

%tenta_possibilidades/6
%Recebe um puzzle, uma linha, o numero maximo de elementos da linha, uma lista de posicoes ja preenchidas e um acumulador de posicoes (lista vazia standard)
%Devolve a solucao das linhas desde a atual, ou retorna falso caso seja impossivel
tenta_possibilidades(_, Linha_Atual, MaxLinha, Ja_Preenchidas, _, Solucao) :-
    Linha_Atual is MaxLinha + 2,
    sort(Ja_Preenchidas, Solucao).

tenta_possibilidades(Puz, Linha, MaxLinha, Ja_Preenchidas, [P|_], Solucao) :-
    union(Ja_Preenchidas, P, Tentativa),
    resolve_aux(Puz, Linha, MaxLinha, Tentativa, Solucao).

tenta_possibilidades(Puz, Linha, MaxLinha, Ja_Preenchidas, [_|R], Solucao) :-
    tenta_possibilidades(Puz, Linha, MaxLinha, Ja_Preenchidas, R, Solucao).


%gera_posicoes_linha/3
%Recebe uma linha e um tamanho
%Devolve uma lista de posicoes dessa linha desde a coluna 1 ate a coluna tamanho
gera_posicoes_linha(Linha, Tamanho, Lista) :-
    gera_posicoes_linha(Linha, Tamanho, 1, Lista).
gera_posicoes_linha(_, T, S, []) :-
    S is T + 1.
gera_posicoes_linha(Linha, Tamanho, Indice, [A|R]) :-
    A = (Linha, Indice),
    Novo_Indice is Indice + 1,
    gera_posicoes_linha(Linha, Tamanho, Novo_Indice, R).
