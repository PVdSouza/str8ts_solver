:- use_module(library(clpfd)).

tabuleiro([[_,_,_,_,_,3],
           [_,_,_,1,_,_],
           [_,6,_,_,_,_],
           [_,_,3,_,1,_],
           [3,2,_,6,_,_],
           [5,_,_,_,_,_]]).

cores([[false,true,true,false,false,false],
       [false,true,true,false,true,true],
       [false,true,true,true,true,true],
       [true,true,true,true,true,false],
       [true,true,false,true,true,false],
       [false,false,false,true,true,false]]).

str8ts(Rows) :-
    cores(Color),
    append(Rows, Vs), Vs ins 1..6,
    before_sequences(Rows,Colors,Sequences),
    maplist(is_sequence, Sequences),
    transpose(Rows, Columns),
    transpose(Colors, TColors),
    before_sequences(Columns,TColors,TSequences),
    maplist(is_sequence, TSequences),
    maplist(label, Rows).

is_sequence([]).
is_sequence([L]).
is_sequence(L) :- sort(L,XL), [First|_] = XL, my_last(XL,Last), length(L,Len), Len - 1 = Last - First.

my_last([],-1).
my_last([X],X).
my_last([H|T],L) :- my_last(T,L).

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
verify([false|Color],false).
verify([true|Color],true).

remove_used_true([],[],[]).
remove_used_true([N|Number],[C|Color],Result) :-
    (C = true -> remove_used_true(Number,Color,Result);remove_used_false([N|Number],[C|Color],Result)).

remove_used_false([],[],[]).
remove_used_false(Number,true,Number).
remove_used_false(Number,[true|_],Number).
remove_used_false([N|Number],[C|Color],Result) :-
    (C = false -> remove_used_false(Number,Color,Result);Result is [N|Number]).

remove_used_true_colors([],[],[]).
remove_used_true_colors([N|Number],[C|Color],Result) :-
    (C = true -> remove_used_true_colors(Number,Color,Result);remove_used_false_colors([N|Number],[C|Color],Result)).

remove_used_false_colors([],[],[]).
remove_used_false_colors(_,true,true).
remove_used_false_colors(_,[true|Color],[true|Color]).
remove_used_false_colors([N|Number],[C|Color],Result) :-
    (C = false -> remove_used_false_colors(Number,Color,Result);Result is [C|Color]).

get_one_sequence([],[],[]).
get_one_sequence(_,false,[]).
get_one_sequence(number,true,number).
get_one_sequence(_,[false|Color],[]).
get_one_sequence([H|Number],[C|Color],L) :-
    (C = true -> get_one_sequence(Number,Color,XL), append([H],XL,L)).


%   [
%   [false,true,true,false,false,false],
%   [false,true,true,false,true,true],
%   [false,true,true,true,true,true],
%   [true,true,true,true,true,false],
%   [true,true,false,true,true,false],
%   [false,false,false,true,true,false],