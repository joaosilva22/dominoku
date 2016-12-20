:- use_module(library(clpfd)).
:- use_module(library(lists)).

peca(1, [0, 0]).
peca(2, [0, 1]).
peca(3, [0, 2]).
peca(4, [0, 3]).
peca(5, [0, 4]).
peca(6, [1, 1]).
peca(7, [1, 2]).
peca(8, [1, 3]).
peca(9, [1, 4]).
peca(10, [2, 2]).
peca(11, [2, 3]).
peca(12, [2, 4]).

board([0, 3, 0, 2, 4, 2, 0, 0, 4, 0, 1, 4, 3, 1, 0, 2, 2, 3, 1, 1, 1, 1, 2, 2]).

% Indices começam a 1
getAdjacent(Index, Vars, BoardWidth, Adjacent) :-
    length(Vars, BoardLength),
    NIndex is Index+BoardWidth,
    NIndex =< BoardLength,
    nth1(NIndex, Vars, Adjacent).
getAdjacent(Index, Vars, BoardWidth, Adjacent) :-
    NIndex is Index-BoardWidth,
    NIndex > 0,
    nth1(NIndex, Vars, Adjacent).
getAdjacent(Index, Vars, BoardWidth, Adjacent) :-
    NIndex is Index-1,
    Div1 is (Index-1)//BoardWidth,
    Div2 is (NIndex-1)//BoardWidth,
    Div1 == Div2,
    nth1(NIndex, Vars, Adjacent).
getAdjacent(Index, Vars, BoardWidth, Adjacent) :-
    NIndex is Index+1,
    Div1 is (Index-1)//BoardWidth,
    Div2 is (NIndex-1)//BoardWidth,
    Div1 == Div2,
    nth1(NIndex, Vars, Adjacent).

getAllAdjacent(Index, Vars, BoardWidth, Adjacent) :-
    bagof(Adj, getAdjacent(Index, Vars, BoardWidth, Adj), Adjacent).

% Devolve todas as listas de adjacencias pela ordem inversa
% Ou seja de CMax a CMin
getAllAdjacentList(0, _, _, []).
getAllAdjacentList(BoardLength, Vars, BoardWidth, [Adjacent|Adjacents]) :-
    getAllAdjacent(BoardLength, Vars, BoardWidth, Adjacent),
    Index is BoardLength-1,
    getAllAdjacentList(Index, Vars, BoardWidth, Adjacents).

restrainAdjacencies(0, _, _).
restrainAdjacencies(BoardLength, Vars, [Adjacent|Adjacencies]) :-
    nth1(BoardLength, Vars, Current),
    contaIguais(Adjacent, Current, Num),
    Num #= 1,
    Index is BoardLength-1,
    restrainAdjacencies(Index, Vars, Adjacencies).

contaIguais([], _, 0).
contaIguais([Var1|Mais], Var, N) :-
    Var1 #= Var,
    contaIguais(Mais, Var, N1),
    N is N1+1.
contaIguais([Var1|Mais], Var, N) :-
    Var1 #\= Var,
    contaIguais(Mais, Var, N).

match(Index, Var) :-
    board(Board),
    peca(Var, Piece),
    nth1(Index, Board, Num),
    nth0(0, Piece, Value),
    Value #= Num.
match(Index, Var) :-
    board(Board),
    peca(Var, Piece),
    nth1(Index, Board, Num),
    nth0(1, Piece, Value),
    Value #= Num.

restrainMatch(_, 0).
restrainMatch(Vars, BoardLength) :-
    nth1(BoardLength, Vars, Var),
    match(BoardLength, Var),
    Index #= BoardLength-1,
    restrainMatch(Vars, Index).

solve_dominoku(Vars, BoardWidth) :-
    length(Vars, BoardLength),
    NumberOfPieces is BoardLength//2,
    domain(Vars, 1, NumberOfPieces),
    getAllAdjacentList(BoardLength, Vars, BoardWidth, Adjacencies),
    restrainAdjacencies(BoardLength, Vars, Adjacencies),
    restrainMatch(Vars, BoardLength),
    statistics(walltime, _),
    labeling([], Vars),
    fd_statistics.

print_board(Board, Width) :-
    print_full_separator(Width, 1),
    print_lines(Board, Width, 1),
    print_full_separator(Width, 1).

print_lines(Board, Width, Line) :-
    length(Board, Length),
    NumberOfLines is Length//Width,
    Line == NumberOfLines,
    print_line(Board, Width, Line, 1).
print_lines(Board, Width, Line) :-
    print_line(Board, Width, Line, 1),
    print_separator(Width, 1),
    NLine is Line+1,
    print_lines(Board, Width, NLine).

print_line(Board, Width, Line, Width) :-
    Index is (Line-1)*Width+Width,
    nth1(Index, Board, Value),
    write(Value), write('|'), nl.
print_line(Board, Width, Line, 1) :-
    Index is (Line-1)*Width+1,
    nth1(Index, Board, Value),
    write('|'), write(Value), write(' '),
    print_line(Board, Width, Line, 2).
print_line(Board, Width, Line, Column) :-
    Index is (Line-1)*Width+Column,
    nth1(Index, Board, Value),
    write(Value), write(' '),
    NColumn is Column+1,
    print_line(Board, Width, Line, NColumn).

print_separator(Width, Width) :-
    write('x x'), nl.
print_separator(Width, Current) :-
    write('x '),
    NCurrent is Current+1,
    print_separator(Width, NCurrent).

print_full_separator(Width, Width) :-
    write('x-x'), nl.
print_full_separator(Width, Current) :-
    write('x-'),
    NCurrent is Current+1,
    print_full_separator(Width, NCurrent).
    
