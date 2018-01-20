/*
Author: Georgios Kamaras
Date: 8/05/2017
*/
% My implementation uses the ic Constraint Programming library
:- lib(ic).

% the "main" declaration of our problem, the Liars are what we want
liars_csp(Lin, Liars) :-
  def_vars(Lin, Liars),
  def_constrs(Lin, Liars),
  generate(Liars).

% define our variables and fields
def_vars(Lin, Solution) :-
  length(Lin, N),
  length(Solution, N),
  Solution #:: [0,1].

% define our constraints
def_constrs(Lin, Liars) :-
  S #= sum(Liars),
  constraint(Lin, Liars, S).

% my constraint statement
constraint([], [], _).
constraint([X|Xs], [Y|Ys], S) :-
  Y #= (S #< X),          % If we have a liar Y <- 1, then the SumOfTheList will be less than his estimation
  constraint(Xs, Ys, S).

% generate our solution
generate(Sol) :-
  search(Sol, 0, input_order, indomain, complete, []).
% I use input order, because I just care about the order that the variables are listed,
% nothin more special

% generate random sequences
genrand(N, List) :-
  length(List, N),
  make_list(N, List).

make_list(_, []).
make_list(N, [X|List]) :-
  random(R),
  X is R mod (N+1),
  make_list(N, List).
