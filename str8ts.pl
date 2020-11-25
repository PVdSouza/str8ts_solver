tabuleiro([[0,_,_,0,0,3],
           [0,_,_,1,_,_],
           [0,6,_,_,_,_],
           [0,_,3,_,1,_],
           [3,2,0,6,_,0],
           [5,0,0,_,_,0]]).


n(1).
n(2).
n(3).
n(4).
n(5).
n(6).

todosDiferentes([]).
todosDiferentes([H|T]) :- not(member(H,T)), todosDiferentes(T).

% removeZeros([],[]).
% removeZeros([0],[]).
% removeZeros([X],[X]).
% removeZeros([H|T],XL) :-
%     (H = 0 -> removeZeros(T,XL); removeZeros(T,L), XL = [H|L]).

%Tento atribuir valores faltantes e verificar se todos são diferentes
completa([X1, X2, X3, X4, X5, X6]) :-
    n(X1), n(X2), n(X3), n(X4), n(X5), n(X6),
    todosDiferentes([X1, X2, X3, X4, X5, X6]).

solucao(TabuleiroSolucao) :-
    TabuleiroSolucao = tabuleiro([
            [X11, X12, X13, X14, X15, X16],
            [X21, X22, X23, X24, X25, X26],
            [X31, X32, X33, X34, X35, X36],
            [X41, X42, X43, X44, X45, X46],
            [X51, X52, X53, X54, X55, X56],
            [X61, X62, X63, X64, X65, X66]
          ]),

    %linhas
    completa([X11, X12, X13, X14, X15, X16]),
    completa([X21, X22, X23, X24, X25, X26]),
    completa([X31, X32, X33, X34, X35, X36]),
    completa([X41, X42, X43, X44, X45, X46]),
    completa([X51, X52, X53, X54, X55, X56]),
    completa([X61, X62, X63, X64, X65, X66]),

    %colunas
    completa([X11, X21, X31, X41, X51, X61]),
    completa([X12, X22, X32, X42, X52, X62]),
    completa([X13, X23, X33, X43, X53, X63]),
    completa([X14, X24, X34, X44, X54, X64]),
    completa([X15, X25, X35, X45, X55, X65]),
    completa([X16, X26, X36, X46, X56, X66]),

    %quadrados menores
    completa([X11, X12, X13, X21, X22, X23, X31, X32, X33]),
    completa([X41, X42, X43, X51, X52, X53, X61, X62, X63]),
    completa([X14, X15, X16, X24, X25, X26, X34, X35, X36]),
    completa([X44, X45, X46, X54, X55, X56, X64, X65, X66]).
