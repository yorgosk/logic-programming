/*
Author: Georgios Kamaras
Date: 30/04/2017
*/
% My implementation uses the ic Constraint Programming library
:- lib(ic).
:- lib(branch_and_bound).

% from graph.pl -- start
create_graph(NNodes, Density, Graph) :-
   cr_gr(1, 2, NNodes, Density, [], Graph).

cr_gr(NNodes, _, NNodes, _, Graph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 > NNodes,
   NN1 is N1 + 1,
   NN2 is NN1 + 1,
   cr_gr(NN1, NN2, NNodes, Density, SoFarGraph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 =< NNodes,
   rand(1, 100, Rand),
   (Rand =< Density ->
      append(SoFarGraph, [N1 - N2], NewSoFarGraph) ;
      NewSoFarGraph = SoFarGraph),
   NN2 is N2 + 1,
   cr_gr(N1, NN2, NNodes, Density, NewSoFarGraph, Graph).

rand(N1, N2, R) :-
   random(R1),
   R is R1 mod (N2 - N1 + 1) + N1.
% from graph.pl -- end

% Clique and Size is what we want
maxclq(N, D, Clique, Size) :-
  create_graph(N, D, G), write("Graph = "), write(G), write("\n"),
  def_vars(N, Solution),
  state_constrs(Solution, G),
  Cost #= N - sum(Solution),
  bb_min(search(Solution, 0, first_fail, indomain, complete, []), Cost, bb_options{strategy:restart}),
  produce_clique(Solution, Clique),
  length(Clique, Size).

% define our variables and fields
def_vars(N, Solution) :-
  length(Solution, N),
  Solution #:: [0,1].

% my constraint statement
state_constrs(Solution, G) :-
  constraint(Solution, 1, G, Solution).

constraint([], _, _, _).
constraint([_|Xs], I, G, Solution) :-
  I1 is I+1,
  check(I, Xs, I1, G, Solution),
  constraint(Xs, I1, G, Solution).

% check constraint satisfaction
check(_, [], _, _, _).
check(I, [_|Ys], J, G, Solution) :-
  member(I - J, G),                 % if there is such an edge in our graph
  J1 is J+1,
  check(I, Ys, J1, G, Solution).
check(I, [_|Ys], J, G, Solution) :-
  n_th(I, Solution, Node1),
  n_th(J, Solution, Node2),
  Node1+Node2 #=< 1,
  J1 is J+1,
  check(I, Ys, J1, G, Solution).

% locate the n-th element of a list
n_th(1, [Node| _], Node).
n_th(N, [_| Nodes], Node) :-
  N \= 1,
  N1 is N - 1,
  n_th(N1, Nodes, Node).

% Clique is the members of the Solution that are '1'
produce_clique(Solution, Clique) :-
  produce_clique(Solution, 1, Clique), write(Clique), write('\n').

produce_clique([], _, []).
produce_clique([X|Xs], Num, Clique) :-
  X is 1,
  Num1 is Num+1,
  produce_clique(Xs, Num1, Clq),
  append([Num], Clq, Clique).
produce_clique([_|Xs], Num, Clique) :-
  Num1 is Num+1,
  produce_clique(Xs, Num1, Clique).
