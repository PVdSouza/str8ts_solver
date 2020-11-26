:- use_module(library(clpfd)).

tabuleiro([[0,_,_,0,0,3],
           [0,_,_,1,_,_],
           [0,6,_,_,_,_],
           [0,_,3,_,1,_],
           [3,2,0,6,_,0],
           [5,0,0,_,_,0]]).

cores([[false,true,true,false,false,false],
       [false,true,true,false,true,true],
       [false,true,true,true,true,true],
       [false,true,true,true,true,true],
       [true,true,false,true,true,false],
       [false,false,false,true,true,false]]).

n(0).
n(1).
n(2).
n(3).
n(4).
n(5).
n(6).

todosDiferentes([]).
todosDiferentes([H|T]) :- not(member(H,T)), todosDiferentes(T).

get_sequences([],[],[]).
get_sequences(Number,Color,Result) :-
    get_one_sequence(Number,Color,L),
    remove_used_true(Number,Color,XNumber),
    remove_used_true_colors(Number,Color,XColor),
    get_sequences(XNumber,XColor,Tail),
    Result = [L|Tail].

get_one_sequence([],[],[]).
get_one_sequence(_,false,[]).
get_one_sequence(number,true,number).
get_one_sequence(_,[false|_],[]).
get_one_sequence([H|Number],[C|Color],L) :-
    C,
    get_one_sequence(Number,Color,XL),
    L=[H|XL].

remove_used_true([],[],[]).
remove_used_true([N|Number],[C|Color],Result) :-
    (C = true ->
        remove_used_true(Number,Color,Result)
    ;
        remove_used_false([N|Number],[C|Color],Result)
    ).

remove_used_false([],[],[]).
remove_used_false(Number,true,Number).
remove_used_false(Number,[true|_],Number).
remove_used_false([N|Number],[C|Color],Result) :-
    (C = false ->
        remove_used_false(Number,Color,Result)
    ;
        Result = [N|Number]
    ).

remove_used_true_colors([],[],[]).
remove_used_true_colors([N|Number],[C|Color],Result) :-
    (C ->
        remove_used_true_colors(Number,Color,Result)
    ;
        remove_used_false_colors([N|Number],[C|Color],Result)
    ).

remove_used_false_colors([],[],[]).
remove_used_false_colors(_,true,true).
remove_used_false_colors(_,[true|Color],[true|Color]).
remove_used_false_colors([_|Number],[C|Color],Result) :-
    (C = false ->
        remove_used_false_colors(Number,Color,Result)
    ;
        Result = [C|Color]
    ).

are_sequences([]).
are_sequences([H|T]) :-
    is_sequence(H),
    are_sequences(T).

is_sequence([]).
is_sequence([_]).
is_sequence(L) :- sort(L,XL), [First|_] = XL, last(XL,Last), length(L,Len), Len - 1 =:= Last - First.

set_domain([],[]).
set_domain([[N]|Tail],[[C]|TCor]) :-
    (C -> N in 1..6; N in 0..6),
    set_domain(Tail,TCor).
set_domain([[N|Number]|Tail],[[H|Color]|TCor]) :-
    (H = true -> N in 1..6; N in 0..6),
    set_domain([Number|Tail],[Color|TCor]).

removeZeros([],[]).
removeZeros([0],[]).
removeZeros([X],[X]).
removeZeros([H|T],XL) :-
    (H = 0 -> removeZeros(T,XL); removeZeros(T,L), XL = [H|L]).

%Tento atribuir valores faltantes e verificar se todos são diferentes
completa([X1, X2, X3, X4, X5, X6],Colors) :-
    n(X1), n(X2), n(X3), n(X4), n(X5), n(X6),
    removeZeros([X1, X2, X3, X4, X5, X6],Result),
    todosDiferentes(Result),
    get_sequences([X1, X2, X3, X4, X5, X6],Colors,Sequences),
    are_sequences(Sequences).

solucao(TabuleiroSolucao) :-
    TabuleiroSolucao = tabuleiro([
            [X11, X12, X13, X14, X15, X16],
            [X21, X22, X23, X24, X25, X26],
            [X31, X32, X33, X34, X35, X36],
            [X41, X42, X43, X44, X45, X46],
            [X51, X52, X53, X54, X55, X56],
            [X61, X62, X63, X64, X65, X66]
          ]),

    cores([
            [C11, C12, C13, C14, C15, C16],
            [C21, C22, C23, C24, C25, C26],
            [C31, C32, C33, C34, C35, C36],
            [C41, C42, C43, C44, C45, C46],
            [C51, C52, C53, C54, C55, C56],
            [C61, C62, C63, C64, C65, C66]
    ]),

    %seta dominio
    set_domain(tabuleiro,cores),

    %linhas
    completa([X11, X12, X13, X14, X15, X16],[C11, C12, C13, C14, C15, C16]),
    completa([X21, X22, X23, X24, X25, X26],[C21, C22, C23, C24, C25, C26]),
    completa([X31, X32, X33, X34, X35, X36],[C31, C32, C33, C34, C35, C36]),
    completa([X41, X42, X43, X44, X45, X46],[C41, C42, C43, C44, C45, C46]),
    completa([X51, X52, X53, X54, X55, X56],[C51, C52, C53, C54, C55, C56]),
    completa([X61, X62, X63, X64, X65, X66],[C61, C62, C63, C64, C65, C66]),

    %colunas
    completa([X11, X21, X31, X41, X51, X61],[C11, C21, C31, C41, C51, C61]),
    completa([X12, X22, X32, X42, X52, X62],[C12, C22, C32, C42, C52, C62]),
    completa([X13, X23, X33, X43, X53, X63],[C13, C23, C33, C43, C53, C63]),
    completa([X14, X24, X34, X44, X54, X64],[C14, C24, C34, C44, C54, C64]),
    completa([X15, X25, X35, X45, X55, X65],[C15, C25, C35, C45, C55, C65]),
    completa([X16, X26, X36, X46, X56, X66],[C16, C26, C36, C46, C56, C66]).