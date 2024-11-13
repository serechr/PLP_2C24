juntar([],L,L).
juntar([X|L1],L2,[X|L3]) :- juntar(L1,L2,L3).

last(L,X) :- append(_,[X],L).

% reverse(+L, -L1)
reverse([X],[X]).
reverse([H|T], L) :- append(T1, [H], L), reverse(T,T1).

prefijo(P,L) :- append(P,_,L).

sufijo(S,L) :- append(_,S,L).

% sublista(?S, +L)
sublista(Q,L) :- prefijo(P,L), sufijo(S,L), append(P,Q,L1), append(L1,S,L).

% pertenece(?X, +L)
%pertenece(X,[X|_]).
%pertenece(X,[_|T]) :- pertenece(X,T).

pertenece(X,L) :- append(_,[X|_],L).

% aplanar(+Xs, -Ys)
%aplanar([X],[X]).
%aplanar([H|_], L) :- member(H1,L), aplanar(H,H1).

%intersección(+L1, +L2, -L3)
interseccion([H|_],[H|_],L3) :- member(H,L3).
interseccion([H|_],[_|T],L3) :- member(H,T), member(H,L3).
interseccion([_|T1],[_|T2],L3) :- interseccion(T1,T2,L3).


%interseccion([],_,[]).
%interseccion([H|T],L2,[H|L3]) :- member(H,L2), interseccion(T,L2,L3).
%interseccion([H|T],L2,L3) :- not(member(H,L2)), not(member(H,L3)), interseccion(T,L2,L3).

% partir(N, L, L1, L2)

partir(0,L,[],L).
partir(N,L,L1,L2) :- N > 0, N1 is N - 1, partir(N1,L,L3,[H|L2]), append(L3,[H],L1).

% borrar(+ListaOriginal, +X, -ListaSinXs)
borrar([],_,[]).
borrar([X|T],X,L) :- borrar(T,X,L).
borrar([Y|T],X,[Y|L]) :- X \= Y, borrar(T,X,L).

% sacarDuplicados(+L1, -L2)
sacarDuplicados1([],[]).
sacarDuplicados1([X|T],T1) :- member(X,T), sacarDuplicados1(T,T1).
sacarDuplicados1([X|T],[X|T1]) :- not(member(X,T)), sacarDuplicados1(T,T1).

% sacarDuplicados(+L1, -L2)
sacarDuplicados([],[]).
sacarDuplicados([X|T],L) :- sacarDuplicados(T,Ts), borrar(Ts,X,Ls), append([X],Ls,L).


borrar1([],_,[]).
borrar1([X|T],X,T).
borrar1([Y|T],X,[Y|L]) :- X \= Y, borrar(T,X,L).

% permutación(+L1, ?L2)
permutacion([X],[X]).
permutacion([H|T],L) :- member(H,L), permutacion(T,L1), borrar1(L,H,L1).


%% FUNCIONES PARA EL TP %%
% borrar(+ListaOriginal, +X, -ListaSinXs)
borrar([],_,[]).
borrar([X|T],X,L) :- borrar(T,X,L).
borrar([Y|T],X,[Y|L]) :- X \= Y, borrar(T,X,L).

% sacarDuplicados(+L1, -L2)
sacarDuplicados([],[]).
sacarDuplicados([X|T],L) :- sacarDuplicados(T,Ts), borrar(Ts,X,Ls),append([X],Ls,L).

% partir(+N,?L,?L1,?L2)
partir(0,L,[],L).
partir(N,L,L1,L2) :- N>0, N1 is N-1, partir(N1,L,L3,[H|L2]), append(L3,[H],L1).

% entre(+I,+S,-V)
entre(X,Y,X) :- X =< Y.
entre(X,Y,Z) :- X =< Y, X1 is X+1, entre(X1,Y,Z).

% long(+XS,-L)
long([],0).
long([_|T],L) :- long(T,L1), L is L1+1.






