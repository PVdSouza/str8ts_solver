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


teste_n([[0,1,3,4],
         [1,2,1,3]]).
teste_c([[false,false,true,false],
        [true,false, true, true]]).

str8ts(Rows) :-
    cores(Colors),
    set_domain(Rows,Colors),
    before_sequences(Rows,Colors,Sequences),
    maplist(is_sequence, Sequences),
    transpose(Rows, Columns),
    transpose(Colors, TColors),
    before_sequences(Columns,TColors,TSequences),
    maplist(is_sequence, TSequences),
    maplist(label, Rows).

set_domain([],[]).
set_domain([[]|Tail],[[]|TCor]) :- set_domain(Tail,TCor).
set_domain([[N]|Tail],[[C]|TCor]) :- (C -> N in 1..6; N in 0..6), set_domain([Number]|Tail,[Color]|TCor).
set_domain([[N|Number]|Tail],[[H|Color]|TCor]) :-
    (H = true -> N in 1..6; N in 0..6),
    set_domain([Number|Tail],[Color|TCor]).

is_sequence([]).
is_sequence([_]).
is_sequence(L) :- sort(L,XL), [First|_] = XL, my_last(XL,Last), length(L,Len), Len - 1 = Last - First.

my_last([],-1).
my_last([X],X).
my_last([_|T],L) :- my_last(T,L).

% verifica se a lista não começa com false.
before_sequences(Number,Color,Result) :-
    verify(Color,L),
    ( L = false -> remove_used_false(Number,Color,XNumber),
                remove_used_false_colors(Number,Color,XColor),
                get_sequences(XNumber,XColor,Result);
                get_sequences(Number,Color,Result)).

get_sequences([],[],[]).
get_sequences(Number,Color,[L | Tail]) :-
    get_one_sequence(Number,Color,L),
    remove_used_true(Number,Color,XNumber),
    remove_used_true_colors(Number,Color,XColor),
    get_sequences(XNumber,XColor,Tail).

verify([],false).
verify([[X|_]|_],X).

remove_true_list([],[],[]).
remove_true_list([N|Number],[C|Color],[R|Result]) :-
    remove_used_true(N,C,R),
    remove_true_list(Number,Color, Result).

remove_used_true([],[],[]).
remove_used_true([N|Number],[C|Color],Result) :-
    (C = true -> remove_used_true(Number,Color,Result);remove_used_false([N|Number],[C|Color],Result)).

remove_false_list([],[],[]).
remove_false_list([N|Number],[C|Color],[R|Result]) :-
    remove_used_false(N,C,R),
    remove_false_list(Number, Color, Result).


remove_used_false([],[],[]).
remove_used_false(Number,true,Number).
remove_used_false(Number,[true|_],Number).
remove_used_false([N|Number],[C|Color],Result) :-
    (C = false -> remove_used_false(Number,Color,Result);Result is [N|Number]).


remove_true_colors_list([],[],[]).
remove_true_colors_list([N|Number],[C|Color],[R|Result]) :-
    remove_used_true_colors(N,C,R),
    remove_true_colors_list(Number,Color, Result).

remove_used_true_colors([],[],[]).
remove_used_true_colors([N|Number],[C|Color],Result) :-
    (C = true -> remove_used_true_colors(Number,Color,Result);remove_used_false_colors([N|Number],[C|Color],Result)).


remove_false_colors_list([],[],[]).
remove_false_colors_list([N|Number],[C|Color],[R|Result]) :-
    remove_used_false_colors(N,C,R),
    remove_false_colors_list(Number, Color, Result).

remove_used_false_colors([],[],[]).
remove_used_false_colors(_,true,true).
remove_used_false_colors(_,[true|Color],[true|Color]).
remove_used_false_colors([_|Number],[C|Color],Result) :-
    (C = false -> remove_used_false_colors(Number,Color,Result);Result is [C|Color]).

get_one_sequence([],[],[]).
get_one_sequence(_,false,[]).
get_one_sequence(number,true,number).
get_one_sequence(_,[false|_],[]).
get_one_sequence([H|Number],[C|Color],L) :-
    (C = true -> get_one_sequence(Number,Color,XL), append([H],XL,L)).
