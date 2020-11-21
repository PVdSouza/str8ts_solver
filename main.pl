:- use_module(library(clpfd)).

%str8ts(Rows,Colors) :-
%  append(Rows, Vs), Vs ins 1..6,
%  maplist(is_sequence, Rows),
%  transpose(Rows, Columns),
%  maplist(is_sequence, Columns),
%  Rows = [A,B,C,D,E,F],
% blocks(A, B, C), blocks(D, E, F), blocks(G, H, I),
%  maplist(label, Rows).

is_sequence([]).
is_sequence([L]).
is_sequence(L) :- sort(L,XL), [First|_] = XL, my_last(XL,Last), length(L,Len), Len - 1 = Last - First.

my_last([],-1).
my_last([X],X).
my_last([H|T],L) :- my_last(T,L).


%não ta funcionando \/
get_sequence([],[],[]).
get_sequence([H|List],[C|Color],L) :- get_sequence(List,Color,XL), C, L is [H|XL].

% Puzzle = [
%   [_,_,_,_,_,3],
%   [_,_,_,1,_,_],
%   [_,6,_,_,_,_],
%   [_,_,3,_,1,_],
%   [3,2,_,6,_,_],
%   [5,_,_,_,_,_],
%   ],
%   [
%   [false,true,true,false,false,false],
%   [false,true,true,false,true,true],
%   [false,true,true,true,true,true],
%   [true,true,true,true,true,false],
%   [true,true,false,true,true,false],
%   [false,false,false,true,true,false],
%   ],
%   [_,5,4,_,_,3],
%   [_,3,2,1,4,5],
%   [_,6,5,3,2,4],
%   [2,4,3,5,1,_],
%   [3,2,_,6,5,_],
%   [5,_,_,4,3,_],
%   Puzzle = [A,B,C,D,E,F,G,H,I],
%   sudoku([A,B,C,D,E,F,G,H,I]).