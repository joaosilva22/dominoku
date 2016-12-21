:- use_module(library(clpfd)).
:- use_module(library(lists)).

% Pecas: vao desde [0,0] a [4,4]
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
peca(13, [3, 3]).
peca(14, [3, 4]).
peca(15, [4, 4]).

% Tabuleiros: board(BoardId, Board)
board(1, [0, 0, 1, 0, 2, 0]). % 3x2
board(2, [2, 0, 0, 3, 1, 0, 0, 0, 0, 1, 1, 4]). % 4x3
board(3, [0, 0, 4, 0, 2, 1, 1, 0, 1, 2, 0, 3, 1, 3, 0, 1]). % 4x4
board(4, [0, 3, 0, 2, 4, 0, 0, 4, 0, 1, 3, 1, 0, 2, 2, 1, 1, 1, 1, 2]). % 5x4
board(5, [3, 1, 2, 2, 4, 1, 1, 2, 0, 2, 0, 0, 2, 4, 0, 1, 3, 2, 4, 0, 1, 1, 3, 0]). % 6x4
board(6, [2, 0, 0, 2, 2, 3, 2, 0, 1, 1, 0, 0, 1, 1, 4, 4, 4, 3, 2, 1, 3, 2, 3, 3, 1, 0, 3, 4, 4, 4]). % 6x5

% getAdjacent: devolve uma celula adjacente a celula em index
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

% getAllAdjacent: devolve todas as celulas adjacentes a celula em index
getAllAdjacent(Index, Vars, BoardWidth, Adjacent) :-
    bagof(Adj, getAdjacent(Index, Vars, BoardWidth, Adj), Adjacent).

% getAllAdjacentList: devolve todas as listas de adjacencias pela ordem inversa
% Ou seja de CMax a CMin
getAllAdjacentList(0, _, _, []).
getAllAdjacentList(BoardLength, Vars, BoardWidth, [Adjacent|Adjacents]) :-
    getAllAdjacent(BoardLength, Vars, BoardWidth, Adjacent),
    Index is BoardLength-1,
    getAllAdjacentList(Index, Vars, BoardWidth, Adjacents).

% restrainAdjacencies: aplica as restricoes relativas a adjacencia
% cada celula apenas pode ter um vizinho com o mesmo valor
restrainAdjacencies(0, _, _).
restrainAdjacencies(BoardLength, Vars, [Adjacent|Adjacencies]) :-
    nth1(BoardLength, Vars, Current),
    contaIguais(Adjacent, Current, Num),
    Num #= 1,
    Index is BoardLength-1,
    restrainAdjacencies(Index, Vars, Adjacencies).

% contaIguais: conta o numero de celulas iguais a var na lista
contaIguais([], _, 0).
contaIguais([Var1|Mais], Var, N) :-
    Var1 #= Var,
    contaIguais(Mais, Var, N1),
    N is N1+1.
contaIguais([Var1|Mais], Var, N) :-
    Var1 #\= Var,
    contaIguais(Mais, Var, N).

% match: verifica se o valor de uma celula corresponde ao esperado
match(BoardId, Index, Var) :-
    board(BoardId, Board),
    peca(Var, Piece),
    nth1(Index, Board, Num),
    nth0(0, Piece, Value),
    Value #= Num.
match(BoardId, Index, Var) :-
    board(BoardId, Board),
    peca(Var, Piece),
    nth1(Index, Board, Num),
    nth0(1, Piece, Value),
    Value #= Num.

% restrainMatch: aplica as restricoes relacionadas com correspondencias
% o valor de uma celula deve corresponder ao valor encontrado no tabuleiro
restrainMatch(_, _, 0).
restrainMatch(BoardId, Vars, BoardLength) :-
    nth1(BoardLength, Vars, Var),
    match(BoardId, BoardLength, Var),
    Index #= BoardLength-1,
    restrainMatch(BoardId, Vars, Index).

% solve_dominoku: resolve o puzzle para o boardid
solve_dominoku(BoardId, Vars, BoardWidth) :-
    length(Vars, BoardLength),
    NumberOfPieces is BoardLength//2,
    domain(Vars, 1, NumberOfPieces),
    getAllAdjacentList(BoardLength, Vars, BoardWidth, Adjacencies),
    restrainAdjacencies(BoardLength, Vars, Adjacencies),
    restrainMatch(BoardId, Vars, BoardLength),
    statistics(walltime, _),
    labeling([], Vars),
    fd_statistics.

% print_board: imprime o tabuleiro inicial
print_board(Board, Width) :-
    print_full_separator(Width, 1),
    print_lines(Board, Width, 1),
    print_full_separator(Width, 1).

% print_lines: imprime as linhas do tabuleiro inicial
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

% print_line: imprime uma linha do tabuleiro inicial
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

% print_separator: imprime o separador horizontal
% entre as linhas do tabuleiro
print_separator(Width, Width) :-
    write('x x'), nl.
print_separator(Width, Current) :-
    write('x '),
    NCurrent is Current+1,
    print_separator(Width, NCurrent).

% print_full_separator: imprime o separador horizontal
% antes da primeira linha e depois da ultima
print_full_separator(Width, Width) :-
    write('x-x'), nl.
print_full_separator(Width, Current) :-
    write('x-'),
    NCurrent is Current+1,
    print_full_separator(Width, NCurrent).

% print_vertical_border: imprime a borda vertical entre duas pecas
print_vertical_border(Solution, Index) :-
    Behind is Index-1,
    nth1(Index, Solution, AtIndex),
    nth1(Behind, Solution, AtBehind),
    AtIndex == AtBehind,
    write(' ').
print_vertical_border(Solution, Index) :-
    Behind is Index-1,
    nth1(Index, Solution, AtIndex),
    nth1(Behind, Solution, AtBehind),
    AtIndex \= AtBehind,
    write('|').

% print_solved_line: imprime uma linha do tabuleiro resolvido
print_solved_line(Board, Width, Solution, Line, Width) :-
    Index is (Line-1)*Width+Width,
    print_vertical_border(Solution, Index),
    nth1(Index, Board, Value),
    write(Value), write('|'), nl.
print_solved_line(Board, Width, Solution, Line, 1) :-
    Index is (Line-1)*Width+1,
    nth1(Index, Board, Value),
    write('|'), write(Value),
    print_solved_line(Board, Width, Solution, Line, 2).
print_solved_line(Board, Width, Solution, Line, Column) :-
    Index is (Line-1)*Width+Column,
    print_vertical_border(Solution, Index),
    nth1(Index, Board, Value),
    write(Value),
    NColumn is Column+1,
    print_solved_line(Board, Width, Solution, Line, NColumn).

% print_horizontal_border: imprime a borda horizontal entre duas pecas
print_horizontal_border(Solution, Width, Line, Column) :-
    Index is (Line-1)*Width+Column,
    Below is Index+Width,
    nth1(Index, Solution, AtIndex),
    nth1(Below, Solution, AtBelow),
    AtIndex == AtBelow,
    write(' ').
print_horizontal_border(Solution, Width, Line, Column) :-
    Index is (Line-1)*Width+Column,
    Below is Index+Width,
    nth1(Index, Solution, AtIndex),
    nth1(Below, Solution, AtBelow),
    AtIndex \= AtBelow,
    write('-').

% print_solved_separator: imprime o separador horizontal
% entre as linhas do tabuleiro resolvido
print_solved_separator(Solution, Width, Line, Width) :-
    write('x'),
    print_horizontal_border(Solution, Width, Line, Width),
    write('x'), nl.
print_solved_separator(Solution, Width, Line, Column) :-
    write('x'),
    print_horizontal_border(Solution, Width, Line, Column),
    NColumn is Column+1,
    print_solved_separator(Solution, Width, Line, NColumn).

% print_solved_lines: imprime as linhas do tabuleiro resolvido
print_solved_lines(Board, Width, Solution, Line) :-
    length(Board, Length),
    NumberOfLines is Length//Width,
    Line == NumberOfLines,
    print_solved_line(Board, Width, Solution, Line, 1).
print_solved_lines(Board, Width, Solution, Line) :-
    print_solved_line(Board, Width, Solution, Line, 1),
    print_solved_separator(Solution, Width, Line, 1),
    NLine is Line+1,
    print_solved_lines(Board, Width, Solution, NLine).

% print_solve_board: imprime o tabuleiro resolvido
print_solved_board(Board, Width, Solution) :-
    print_full_separator(Width, 1),
    print_solved_lines(Board, Width, Solution, 1),
    print_full_separator(Width, 1).

% get_solution: imprime a solucao de um tabuleiro boardid
% Tabuleiro de 3x2
get_solution(1) :-
    board(1, Board),
    write('Tabuleiro inicial:\n'),
    print_board(Board, 3),
    solve_dominoku(1, [S1, S2, S3, S4, S5, S6], 3),
    write('Solucao:\n'),
    print_solved_board(Board, 3, [S1, S2, S3, S4, S5, S6]).
% Tabuleiro de 4x3
get_solution(2) :-
    board(2, Board),
    write('Tabuleiro inicial:\n'),
    print_board(Board, 4),
    solve_dominoku(2, [S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12], 4),
    write('Solucao:\n'),
    print_solved_board(Board, 4, [S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12]).
% Tabuleiro de 4x4
get_solution(3) :-
    board(3, Board),
    write('Tabuleiro inicial:\n'),
    print_board(Board, 4),
    solve_dominoku(3, [S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16], 4),
    write('Solucao:\n'),
    print_solved_board(Board, 4, [S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16]).
% Tabuleiro de 5x4
get_solution(4) :-
    board(4, Board),
    write('Tabuleiro inicial:\n'),
    print_board(Board, 5),
    solve_dominoku(4, [S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20], 5),
    write('Solucao:\n'),
    print_solved_board(Board, 5, [S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20]).
% Tabuleiro de 6x4
get_solution(5) :-
    board(5, Board),
    write('Tabuleiro inicial:\n'),
    print_board(Board, 5),
    solve_dominoku(5, [S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21,S22,S23,S24], 6),
    write('Solucao:\n'),
    print_solved_board(Board, 6, [S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21,S22,S23,S24]).
% Tabuleiro de 6x5
get_solution(6) :-
    board(6, Board),
    write('Tabuleiro inicial:\n'),
    print_board(Board, 6),
    solve_dominoku(6, [S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21,S22,S23,S24,S25,S26,S27,S28,S29,S30], 6),
    write('Solucao:\n'),
    print_solved_board(Board, 6, [S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17,S18,S19,S20,S21,S22,S23,S24,S25,S26,S27,S28,S29,S30]).

% Inicio do programa
start :-
    write('Tamanho do tabuleiro a resolver:\n'),
    write('1 - 3x2\n'),
    write('2 - 4x3\n'),
    write('3 - 4x4\n'),
    write('4 - 5x4\n'),
    write('5 - 6x4\n'),
    write('6 - 6x5\n'),
    read(Choice),
    get_solution(Choice).
