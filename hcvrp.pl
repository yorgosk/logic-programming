/*
Author: Georgios Kamaras
Date: 29/06/2017
*/
% My implementation uses the ic Constraint Programming library
:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).
% our data -- start
vehicles([35, 40, 55, 15, 45, 25, 85, 55]).

clients([c(15,  77,  97), c(23, -28,  64), c(14,  77, -39),
         c(13,  32,  33), c(18,  32,   8), c(18, -42,  92),
         c(19,  -8,  -3), c(10,   7,  14), c(18,  82, -17),
         c(20, -48, -13), c(15,  53,  82), c(19,  39, -27),
         c(17, -48, -13), c(12,  53,  82), c(11,  39, -27),
         c(15, -48, -13), c(25,  53,  82), c(14, -39,   7),
         c(22,  17,   8), c(23, -38,  -7)]).
% our data -- end
% the "main" declaration of our problem, Solution, Cost and Time is what we need to find
hcvrp(NCl, NVe, Timeout, Sol, Cost, Time) :-
  cputime(T1),
  def_vars(NCl, NVe, Ve, Solution, DistMatrix, Cap),
  def_constrs(NCl, Ve, Cap, Solution),
  calculate_cost(Solution, NCl, DistMatrix, CostList),
  Cost #= sum(CostList), !,
  gen_sol(Solution, Cost, Timeout),
  cputime(T2),
  Time is T2-T1,
  clear_zeros(Solution, Sol).

% define our problem's variables and their fields
def_vars(NCl, NVe, Ve, Solution, DistMatrix, Cap) :-
  clients(Cl), vehicles(Ve),
  length(Solution, NVe),
  initialize_solution(Solution, NCl),
  append([c(0, 0, 0)], Cl, AllDist),
  build_distance_matrix(AllDist, AllDist, -1, NCl, DistMatrix),
  get_capacities(AllDist, Cap).

% initialize the solution's representation list
initialize_solution([], _).
initialize_solution([X|L], NCl) :-
  length(X, NCl),
  X #:: 0..NCl,
  initialize_solution(L, NCl).

% build a distance matrix between the clients and the clients and the warehouse
build_distance_matrix([], _, _, _, []) :- !.
build_distance_matrix(_, _, I, I, []) :- !.
build_distance_matrix([H|T], L, I, NCl, DistMatrix) :-
  calc_dists(H, L, -1, NCl, Dists),
  I1 is I+1,
  build_distance_matrix(T, L, I1, NCl, DM),
  append(Dists, DM, DistMatrix).
calc_dists(_, [], _, _, []) :- !.
calc_dists(_, _, I, I, []) :- !.
calc_dists(c(_, X1, Y1), [c(_, X2, Y2)|T], I, NCl, Dists) :-
  E is integer(round((sqrt( abs(X1-X2) * abs(X1-X2) + abs(Y1-Y2) * abs(Y1-Y2) )) * 1000)),
  I1 is I+1,
  calc_dists(c(_, X1, Y1), T, I1, NCl, Ds),
  append([E], Ds, Dists).

% get capacities out of a c(cap, _, _) elements' list, to assist us above
get_capacities([], []).
get_capacities([c(C, _, _)|T], [C|Cap]) :-
  get_capacities(T, Cap).

% define the constraints that we are going to take into consideration
def_constrs(NCl, Ve, Cap, Solution) :-
  occurrences_constraint(NCl, Solution),
  capacity_constraint(NCl, Ve, Cap, Solution),
  warehouse_constr(Solution).

% occurences constraint -- all clients must exist only once in the context of
% all of the solution's sublist
occurrences_constraint(NCl, Solution) :-
  flatten(Solution, Sol),
  occurrences_constraint(NCl, NCl, Sol).
occurrences_constraint(1, _, Sol) :- occurrences(1, Sol, 1), !.
occurrences_constraint(I, NCl, Sol) :-
  occurrences(I, Sol, 1),
  I1 is I-1,
  occurrences_constraint(I1, NCl, Sol).

% capacity constraint -- each truck has a certain capacity, in which the sum of the
% volumes of the orders that it serves must comply
capacity_constraint(_, _, _, []).
capacity_constraint(NCl, [HVe|TVe], Cap, [HSol|TSol]) :-
  iterate_clients(NCl, HSol, Cap, CapL),
  sum(CapL) #=< HVe,
  capacity_constraint(NCl, TVe, Cap, TSol).
iterate_clients(_, [], _, []).
iterate_clients(NCl, [H|T], Cap, [V|L]) :-
  I #= H+1,
  element(I, Cap, V),
  iterate_clients(NCl, T, Cap, L).

% warehouse constraint -- a truck should only return to the warehouse once it is
% done delivering to clients
warehouse_constr([]).
warehouse_constr([H|T]) :-
  zero_constr(H),
  %symm_constr(H), % new
  warehouse_constr(T).
zero_constr([H|T]) :- zero_constr(T, H).
zero_constr([], _).
zero_constr([H|T], Prev) :-
  H #= (Prev #\= 0)*H,
  zero_constr(T, H).

% symmetry constraint -- for optimization, with a goal not to check symmetrical solutions
symmetry_constraint([]).
symmetry_constraint([H|T]) :-
  symm_constr(H),
  symmetry_constraint(T).
symm_constr([H|T]) :- symm_constr(H, [H|T]).
symm_constr(_, [_]).
symm_constr(E, [A,B|T]) :-
  B #= 0 => E #>= A,
  symm_constr(E, [B|T]).

% calculate the cost of our solution -- cost is defined as the total distance covered by the trucks
calculate_cost([], _, _, []).
calculate_cost([HS|TS], NCl, DistMatrix, [C|RCL]) :-
  append([0], HS, L),   % we start from the warehouse and we return to the warehouse
  calc_truck_cost(L, 1, NCl, DistMatrix, TCL),
  C #= sum(TCL),
  calculate_cost(TS, NCl, DistMatrix, RCL).
calc_truck_cost([E|_], Ind, _, _, []) :- \+ E #\= 0, Ind > 1, !.
calc_truck_cost([E], _, NCl, DistMatrix, [C]) :- I #= E*(NCl+1)+1, element(I, DistMatrix, C).
calc_truck_cost([A,B|T], Ind, NCl, DistMatrix, [C1|CL]) :-
  I #= A*(NCl+1) + (B+1),
  element(I, DistMatrix, C1),
  Ind1 is Ind+1,
  calc_truck_cost([B|T], Ind1, NCl, DistMatrix, CL).

% generate solution
gen_sol(Solution, Cost, Timeout) :-
  flatten(Solution, Sol),
  bb_min(search(Sol, 0, occurrence, indomain, complete, []),
          Cost, bb_options{timeout:Timeout}).

% remove the zeros from our solution's representation
clear_zeros([], []).
clear_zeros([HS|TS], [L1|L2]) :-
  clear_from_zeros(HS, L1),
  clear_zeros(TS, L2).
clear_from_zeros([], []).
clear_from_zeros([0|_], []).
clear_from_zeros([H|T], [H|L]) :-
  clear_from_zeros(T, L).
