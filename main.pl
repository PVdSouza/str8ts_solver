:- use_module(library(clpfd)).

% Zeros representam células pretas vazias
tabuleiro([[5,0,1,_,_,0],
           [0,2,_,_,_,_],
           [4,_,_,0,_,_],
           [_,_,0,_,_,3],
           [_,_,_,_,_,1],
           [0,_,_,3,0,0]]).

% True representam células brancas
cores([[false,false,true,true,true,false],
       [false,true,true,true,true,true],
       [true,true,true,false,true,true],
       [true,true,false,true,true,true],
       [true,true,true,true,true,false],
       [false,true,true,true,false,false]]).

tabuleiro1([[3,0,_,4,_,0],
           [0,_,_,_,_,0],
           [_,_,0,0,_,2],
           [4,_,0,0,_,3],
           [0,_,_,_,_,5],
           [0,_,3,_,1,0]]).

cores1([[false,false,true,true,true,false],
       [false,true,true,true,true,false],
       [true,true,false,false,true,true],
       [true,true,false,false,true,true],
       [false,true,true,true,true,false],
       [false,true,true,true,true,false]]).

tabuleiro2([[0,0,0,_,_,_],
           [_,2,0,_,_,_],
           [_,_,_,_,_,6],
           [1,_,_,_,_,_],
           [_,_,_,0,6,_],
           [_,_,5,0,0,1]]).

cores2([[false,false,false,true,true,true],
       [true,true,false,true,true,true],
       [true,true,true,true,true,true],
       [true,true,true,true,true,true],
       [true,true,true,false,true,true],
       [true,true,true,false,false,false]]).

% Números possíveis de serem encontrados
n(0).
n(1).
n(2).
n(3).
n(4).
n(5).
n(6).

teste_n([[0,1,3,4],
         [1,2,1,3]]).
teste_c([[false,false,true,false],
        [true,false, true, true]]).

% Resolvedor
str8ts(Rows,Colors) :-
    set_domain(Rows,Colors),
    before_sequences(Rows,Colors,Sequences),
    transpose(Rows, Columns),
    transpose(Colors, TColors),
    before_sequences(Columns,TColors,TSequences),
    all_diferent(Columns, ToCompareColumns),
    maplist(all_distinct,ToCompareColumns),
    completa(Rows),
    %writeln(Rows),
    are_sequences(Sequences),
    are_sequences(TSequences),
    maplist(label, Rows).

% Procedure que completa o tabuleiro
completa([]).
completa([[X1,X2,X3,X4,X5,X6]|Tail]) :-
    n(X1), n(X2), n(X3), n(X4), n(X5), n(X6),
    remove_zeros([X1,X2,X3,X4,X5,X6],Result),
    todos_diferentes(Result),
    completa(Tail).

% Fução recursiva que retorna se uma lista possui todos os elementos diferentes
todos_diferentes([]).
todos_diferentes([H|T]) :- not(member(H,T)), todos_diferentes(T).

all_diferent([],[]).
all_diferent([S|Sequences],[SX|Result]) :-
    remove_zeros(S,SX),
    all_diferent(Sequences,Result).

% Remove todas as ocorrências de zero em uma lita.
remove_zeros([],[]).
remove_zeros([0],[]).
remove_zeros([X],[X]).
remove_zeros([H|T],XL) :-
    (H = 0 -> remove_zeros(T,XL); remove_zeros(T,L), XL = [H|L]).

% Recebe uma lista de listas e verifica se as listas que a compoem formam sequencias
are_sequences([]).
are_sequences([H|T]) :-
    is_sequence(H),
    are_sequences(T).

% Seta o domínio de valores possiveis das células do tabuleiro
% Células pretas podem ter valores de zero a 6,
% e células brancas podem ter valores de 1 a 6.
set_domain([],[]).
set_domain([[N]|Tail],[[C]|TCor]) :-
    (C -> N in 1..6; N in 0..6),
    set_domain(Tail,TCor).
set_domain([[N|Number]|Tail],[[H|Color]|TCor]) :-
    (H = true -> N in 1..6; N in 0..6),
    set_domain([Number|Tail],[Color|TCor]).

% Verifica se uma lista constitui uma sequencia,
% quando seus elementos são ordenados
is_sequence([]).
is_sequence([_]).
is_sequence(L) :- sort(L,XL), [First|_] = XL, last(XL,Last), length(L,Len), Len - 1 =:= Last - First.

% verifica se a lista não começa com false.
before_sequences(Number,Color,Result) :-
    verify(Color,L),
    (L = false ->
        remove_false_list(Number,Color,XNumber),
        remove_false_colors_list(Number,Color,XColor),
        get_seq(XNumber,XColor,Result)
    ;
        get_seq(Number,Color,Result)
    ).

% Retorna todas as sequencias do tabuleiro
get_sequences([],[],[]).
get_sequences(Number,Color,Result) :-
    get_one_sequence(Number,Color,L),
    remove_used_true(Number,Color,XNumber),
    remove_used_true_colors(Number,Color,XColor),
    get_sequences(XNumber,XColor,Tail),
    Result = [L|Tail].

verify([],false).
verify([[X|_]|_],X).

get_seq([],[],[]).
get_seq([N|Number],[C|Color],XL) :-
    get_sequences(N,C,L),
    get_seq(Number,Color,Tail),
    append(L,Tail,XL).

% chama remove_used_true para cada uma das sublistas da matriz
remove_true_list([],[],[]).
remove_true_list([N|Number],[C|Color],[R|Result]) :-
    remove_used_true(N,C,R),
    remove_true_list(Number,Color, Result).

% remove numeros utilizados para celulas true
remove_used_true([],[],[]).
remove_used_true([N|Number],[C|Color],Result) :-
    (C = true ->
        remove_used_true(Number,Color,Result)
    ;
        remove_used_false([N|Number],[C|Color],Result)
    ).

remove_false_list([],[],[]).
remove_false_list([N|Number],[C|Color],[R|Result]) :-
    remove_used_false(N,C,R),
    remove_false_list(Number, Color, Result).

remove_used_false([],[],[]).
remove_used_false(Number,true,Number).
remove_used_false(Number,[true|_],Number).
remove_used_false([N|Number],[C|Color],Result) :-
    (C = false ->
        remove_used_false(Number,Color,Result)
    ;
        Result = [N|Number]
    ).

remove_true_colors_list([],[],[]).
remove_true_colors_list([N|Number],[C|Color],[R|Result]) :-
    remove_used_true_colors(N,C,R),
    remove_true_colors_list(Number,Color, Result).

remove_used_true_colors([],[],[]).
remove_used_true_colors([N|Number],[C|Color],Result) :-
    (C ->
        remove_used_true_colors(Number,Color,Result)
    ;
        remove_used_false_colors([N|Number],[C|Color],Result)
    ).


remove_false_colors_list([],[],[]).
remove_false_colors_list([N|Number],[C|Color],[R|Result]) :-
    remove_used_false_colors(N,C,R),
    remove_false_colors_list(Number, Color, Result).

remove_used_false_colors([],[],[]).
remove_used_false_colors(_,true,true).
remove_used_false_colors(_,[true|Color],[true|Color]).
remove_used_false_colors([_|Number],[C|Color],Result) :-
    (C = false ->
        remove_used_false_colors(Number,Color,Result)
    ;
        Result = [C|Color]
    ).

% Retorna uma sequência de números.
get_one_sequence([],[],[]).
get_one_sequence(_,false,[]).
get_one_sequence(number,true,number).
get_one_sequence(_,[false|_],[]).
get_one_sequence([H|Number],[C|Color],L) :-
    C,
    get_one_sequence(Number,Color,XL),
    L=[H|XL].
