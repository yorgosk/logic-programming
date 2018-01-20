/*
Author: Georgios Kamaras
Date: 24/05/2017
*/
% My implementation uses the ic Constraint Programming library
:- lib(ic).
:- lib(branch_and_bound).

% the "main" declaration of our problem, the Tents are what we want
tents(RowTents, ColumnTents, Trees, Tents) :-
  def_vars(RowTents, NR, ColumnTents, NC, Solution),
  def_constrs(RowTents, NR, ColumnTents, NC, Trees, Solution),
  Cost #= sum(Solution), !,
  generate_min_sol(Cost, Solution), % generate the minimum-cost solution using branch-and-bound
  L3 is NR*NC,
  length(Solution1, L3),
  Solution1 #:: 0..1,
  def_constrs(RowTents, NR, ColumnTents, NC, Trees, Solution1),
  Cost #= sum(Solution1), !,
  generate_more_sol(Solution1),     % generate more solutions with minimum-cost
  translate_to_coord(Solution1, NC, Tents).

% define our problem's variables and their fields
def_vars(RowTents, NR, ColumnTents, NC, Solution) :-
  length(RowTents, NR),
  length(ColumnTents, NC),
  L3 is NR*NC,
  length(Solution, L3),
  Solution #:: 0..1.

% define our problem's constraints
def_constrs(RowTents, NR, ColumnTents, NC, Trees, Solution) :-
  row_constr(RowTents, NC, 1, Solution),
  column_constr(ColumnTents, NR, NC, 1, Solution),
  tree_neighbor_constr(NR, NC, Trees, Solution),
  tent_neighbor_constr(NR, NC, 1, Solution, Solution).

% our row-constraint, each row can have up to a certain number of tents
row_constr([], _, _, _).
row_constr([X|Xs], NC, I, Solution) :-
  X >= 0,       % if this is true, this column is constrained
  gather_row_elems(I, NC, Solution, Row),
  X #>= sum(Row),
  I1 is I+1,
  row_constr(Xs, NC, I1, Solution).
row_constr([_|Xs], NC, I, Solution) :-
  I1 is I+1,
  row_constr(Xs, NC, I1, Solution).

% gather the elements of a certain row
gather_row_elems(R, NC, Solution, Row) :-
  Start is (R-1)*NC+1,
  End is Start+(NC-1),
  gather_row_elems(Start, End, Start, Solution, Row).
% where the element-gathering takes place
gather_row_elems(_, End, End, Solution, [Elem]) :-
  n_th(End, Solution, Elem).
gather_row_elems(Start, End, Index, Solution, [Elem|Elems]) :-
  n_th(Index, Solution, Elem),
  Index1 is Index+1,
  gather_row_elems(Start, End, Index1, Solution, Elems).

% our column-constraint, each column can have up to a certain number of tents
column_constr([], _, _, _, _).
column_constr([X|Xs], NR, NC, I, Solution) :-
  X >= 0,     % if this is true, this column is constrained
  gather_column_elems(I, NR, NC, Solution, Column),
  X #>= sum(Column),
  I1 is I+1,
  column_constr(Xs, NR, NC, I1, Solution).
column_constr([_|Xs], NR, NC, I, Solution) :-
  I1 is I+1,
  column_constr(Xs, NR, NC, I1, Solution).

% gather the elements of a certain column
gather_column_elems(C, NR, NC, Solution, Column) :-
  Start is C,
  End is Start+(NR-1)*NC,
  gather_column_elems(NC, Start, End, Start, Solution, Column).
% where the element-gathering takes place
gather_column_elems(_, _, End, End, Solution, [Elem]) :-
  n_th(End, Solution, Elem).
gather_column_elems(NC, Start, End, Index, Solution, [Elem|Elems]) :-
  n_th(Index, Solution, Elem),
  Index1 is Index+NC,
  gather_column_elems(NC, Start, End, Index1, Solution, Elems).

% our tent-neighbors constraint, each tent must not have any other tent in a neighbor-position
tent_neighbor_constr(_, _, _, [_], _).  % if we are in our last board element, stop
tent_neighbor_constr(NR, NC, I, [X|Xs], Solution) :-
  gather_tent_neighbor_elements(I, NR, NC, Solution, Neighbors),
  1 #>= sum(Neighbors)+X,
  I1 is I+1,
  tent_neighbor_constr(NR, NC, I1, Xs, Solution).

% gather the neighbors of a possible tent-position in a 'Neighbors' list
gather_tent_neighbor_elements(I, NR, NC, Solution, Neighbors) :-
  add_right_neighbor(I, NC, Solution, [], N1),
  add_lower_right_neighbor(I, NR, NC, Solution, N1, N2),
  add_lower_neighbor(I, NR, NC, Solution, N2, Neighbors).

% our tree-neighbors constraint, each tree must have at least one tent at one of his neighbor-positions
tree_neighbor_constr(_, _, [], _).
tree_neighbor_constr(NR, NC, [A - B|Ts], Solution) :-
  I is NC*(A-1)+B,  % the position of the tree in the list-representation of the map
  n_th(I, Solution, Elem),
  Elem #= 0,        % because it is a tree
  gather_tree_neighbor_elements(I, NR, NC, Solution, Neighbors),
  1 #=< sum(Neighbors),
  tree_neighbor_constr(NR, NC, Ts, Solution).

% gather the neighbors of a certain tree-position in a 'Neighbors' list
gather_tree_neighbor_elements(I, NR, NC, Solution, Neighbors) :-
  add_upper_left_neighbor(I, NC, Solution, [], N1),
  add_upper_neighbor(I, NC, Solution, N1, N2),
  add_upper_right_neighbor(I, NC, Solution, N2, N3),
  add_right_neighbor(I, NC, Solution, N3, N4),
  add_lower_right_neighbor(I, NR, NC, Solution, N4, N5),
  add_lower_neighbor(I, NR, NC, Solution, N5, N6),
  add_lower_left_neighbor(I, NR, NC, Solution, N6, N7),
  add_left_neighbor(I, NC, Solution, N7, Neighbors).

% get the upper-left neighbor of the Index's element
add_upper_left_neighbor(Index, NC, _, Neighs, Neighs) :-
  Index =< NC.  % first row's elements don't have upper-left neighbors
add_upper_left_neighbor(Index, NC, _, Neighs, Neighs) :-
  1 is mod(Index, NC).  % first column's elements don't have upper-left neighbors
add_upper_left_neighbor(Index, NC, Solution, Neighs, Neighs1) :-
  Ind is Index-NC-1,
  n_th(Ind, Solution, Elem),
  append(Neighs, [Elem], Neighs1).

% get the upper neighbor of the Index's element
add_upper_neighbor(Index, NC, _, Neighs, Neighs) :-
  Index =< NC.  % first row's elements don't have upper neighbors
add_upper_neighbor(Index, NC, Solution, Neighs, Neighs1) :-
  Ind is Index-NC,
  n_th(Ind, Solution, Elem),
  append(Neighs, [Elem], Neighs1).

% get the upper-right neighbor of the Index's element
add_upper_right_neighbor(Index, NC, _, Neighs, Neighs) :-
  Index =< NC.  % first row's elements don't have upper-rigth neighbors
add_upper_right_neighbor(Index, NC, _, Neighs, Neighs) :-
  0 is mod(Index, NC).  % last column's elements don't have upper-right neighbors
add_upper_right_neighbor(Index, NC, Solution, Neighs, Neighs1) :-
  Ind is Index-NC+1,
  n_th(Ind, Solution, Elem),
  append(Neighs, [Elem], Neighs1).

% get the right neighbor of the Index's element
add_right_neighbor(Index, NC, _, Neighs, Neighs) :-
  0 is mod(Index, NC).  % last column's elements don't have right neighbors
add_right_neighbor(Index, _, Solution, Neighs, Neighs1) :-
  Ind is Index+1,
  n_th(Ind, Solution, Elem),
  append(Neighs, [Elem], Neighs1).

% get the lower-right neighbor of the Index's element
add_lower_right_neighbor(Index, NR, NC, _, Neighs, Neighs) :-
  Index > NR*NC-NC.  % last row's elements don't have lower-right neighbors
add_lower_right_neighbor(Index, _, NC, _, Neighs, Neighs) :-
  0 is mod(Index, NC).  % last column's elements don't have lower-right neighbors
add_lower_right_neighbor(Index, _, NC, Solution, Neighs, Neighs1) :-
  Ind is Index+NC+1,
  n_th(Ind, Solution, Elem),
  append(Neighs, [Elem], Neighs1).

% get the lower neighbor of the Index's element
add_lower_neighbor(Index, NR, NC, _, Neighs, Neighs) :-
  Index > NR*NC-NC.  % last row's elements don't have lower neighbors
add_lower_neighbor(Index, _, NC, Solution, Neighs, Neighs1) :-
  Ind is Index+NC,
  n_th(Ind, Solution, Elem),
  append(Neighs, [Elem], Neighs1).

% get the lower-left neighbor of the Index's element
add_lower_left_neighbor(Index, _, NC, _, Neighs, Neighs) :-
  1 is mod(Index, NC).  % first column's elements don't have lower-left neighbors
add_lower_left_neighbor(Index, NR, NC, _, Neighs, Neighs) :-
  Index > NR*NC-NC.  % last row's elements don't have lower-left neighbors
add_lower_left_neighbor(Index, _, NC, Solution, Neighs, Neighs1) :-
  Ind is Index+NC-1,
  n_th(Ind, Solution, Elem),
  append(Neighs, [Elem], Neighs1).

% get the left neighbor of the Index's element
add_left_neighbor(Index, NC, _, Neighs, Neighs) :-
  1 is mod(Index, NC).  % first column's elements don't have left neighbors
add_left_neighbor(Index, _, Solution, Neighs, Neighs1) :-
  Ind is Index-1,
  n_th(Ind, Solution, Elem),
  append(Neighs, [Elem], Neighs1).

% generate the minimum-cost solution using branch-and-bound
generate_min_sol(Cost, Solution) :-
  bb_min(search(Solution, 0, first_fail, indomain, complete, []), Cost, bb_options{}).

% generate more solution with minimum-cost
generate_more_sol(Solution1) :-
  search(Solution1, 0, first_fail, indomain, complete, []).

% "translate" the list-representation of the problem's field, to a board-representation
translate_to_coord(Solution, NC, Tents) :-
  translate_to_coord(Solution, 1, NC, Tents).
% where the main "translation" takes place
translate_to_coord([], _, _, []).
translate_to_coord([X|Xs], Index, NC, Tents) :-
  X is 1,
  R is div(Index-1, NC)+1,
  C is mod(Index-1, NC)+1,
  Index1 is Index+1,
  translate_to_coord(Xs, Index1, NC, Rest),
  append([R - C], Rest, Tents).
translate_to_coord([X|Xs], Index, NC, Tents) :-
  X is 0,
  Index1 is Index+1,
  translate_to_coord(Xs, Index1, NC, Tents).

% locate the n-th element of a list
n_th(1, [Elem| _], Elem).
n_th(N, [_|Elems], Elem) :-
  N \= 1,
  N1 is N - 1,
  n_th(N1, Elems, Elem).
