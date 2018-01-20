/*
Author: Georgios Kamaras
Date: 9/04/2017
*/

% our facts -- start
dominos( [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),
                (1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
                      (2,2),(2,3),(2,4),(2,5),(2,6),
                            (3,3),(3,4),(3,5),(3,6),
                                  (4,4),(4,5),(4,6),
                                        (5,5),(5,6),
                                             (6,6)]).

frame( [[3,1,2,6,6,1,2,2],
        [3,4,1,5,3,0,3,6],
        [5,6,6,1,2,4,5,0],
        [5,6,4,1,3,3,0,0],
        [6,1,0,6,3,2,4,0],
        [4,1,5,2,4,3,5,5],
        [4,1,0,2,4,5,2,0]]).

% dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9),(0,a),
%                 (1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,a),
%                       (2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9),(2,a),
%                             (3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9),(3,a),
%                                   (4,4),(4,5),(4,6),(4,7),(4,8),(4,9),(4,a),
%                                         (5,5),(5,6),(5,7),(5,8),(5,9),(5,a),
%                                               (6,6),(6,7),(6,8),(6,9),(6,a),
%                                                     (7,7),(7,8),(7,9),(7,a),
%                                                           (8,8),(8,9),(8,a),
%                                                                 (9,9),(9,a),
%                                                                     (a,a)]).
%
% frame([[6,5,0,5,5,3,3,1,1,4,6],
%         [1,2,2,a,a,5,7,0,1,0,7],
%         [5,8,6,0,8,0,9,7,7,4,2],
%         [4,0,9,0,7,7,9,9,8,8,0],
%         [1,a,3,8,8,5,a,8,0,0,3],
%         [9,2,3,5,7,6,9,1,6,3,9],
%         [2,2,2,5,8,6,0,4,6,a,a],
%         [9,4,2,1,7,9,5,4,a,4,a],
%         [9,a,4,9,5,5,6,6,0,a,2],
%         [1,a,1,2,1,1,8,2,2,7,8],
%         [7,7,3,3,4,3,6,6,4,3,1],
%         [5,9,6,3,3,a,7,4,4,8,8]]).

% our facts -- end

/* "roadmap" to solution

1. for each domino construct a list of the coordinates of the positions in frame where it fits
a. for each domino, find where it fits to the frame horizontally
b. for each domino, find where it fits to the frame vertically

2. place the dominos, giving priority to the ones with the least possible positions
a. pick the domino with the least possible positions and put it in the first of them, then remove it from the list
b. update the possible positions of the other dominos, so that the don't conflict with the above decision
c. if at any time we find a domino with no possible positions, then we can't have a solution, so we backtrack

*/

% solution body -- start

fit_in_row(_, _, [], _, _, []).
fit_in_row(_, _, [_], _, _, []).
fit_in_row(X, Y, [X|[Y|T]], Xc, Yc, L) :-
      Yc1 is Yc+1,
      fit_in_row(X, Y, [Y|T], Xc, Yc1, L1),
      append([[(Xc,Yc),(Xc,Yc1)]], L1, L).
fit_in_row(X, Y, [Y|[X|T]], Xc, Yc, L) :-   % fits reversed horizontally
      Yc1 is Yc+1,
      fit_in_row(X, Y, [X|T], Xc, Yc1, L1),
      append([[(Xc,Yc),(Xc,Yc1)]], L1, L).
fit_in_row(X, Y, [_|T], Xc, Yc, L) :-
      Yc1 is Yc+1,
      fit_in_row(X, Y, T, Xc, Yc1, L).

fits_in_rows(_, _, [], _, []).
fits_in_rows(X, Y, [HL|TL], Xc, L) :-
      fit_in_row(X, Y, HL, Xc, 1, L1),
      Xc1 is Xc+1,
      fits_in_rows(X, Y, TL, Xc1, L2),
      append(L1, L2, L).

fit_in_column(_, _, [], [], _, _, []).
fit_in_column(X, Y, [X|T1], [Y|T2], Xc, Yc, L) :-
      Xc1 is Xc+1,
      Yc1 is Yc+1,
      fit_in_column(X, Y, T1, T2, Xc, Yc1, L1),
      append([[(Xc, Yc),(Xc1, Yc)]], L1, L).
fit_in_column(X, Y, [Y|T1], [X|T2], Xc, Yc, L) :-   % fits reversed vertically
      Xc1 is Xc+1,
      Yc1 is Yc+1,
      fit_in_column(X, Y, T1, T2, Xc, Yc1, L1),
      append([[(Xc, Yc),(Xc1, Yc)]], L1, L).
fit_in_column(X, Y, [_|T1], [_|T2], Xc, Yc, L) :-
      Yc1 is Yc+1,
      fit_in_column(X, Y, T1, T2, Xc, Yc1, L).

fits_in_columns(_, _, [], _, []).
fits_in_columns(_, _, [_], _, []).
fits_in_columns(X, Y, [R1|[R2|T]], Xc, L) :-
      fit_in_column(X, Y, R1, R2, Xc, 1, L1),
      Xc1 is Xc+1,
      fits_in_columns(X, Y, [R2|T], Xc1, L2),
      append(L1, L2, L).

find_fits([], []).
find_fits([(X,Y)|T], L) :-
      frame(F),
      fits_in_rows(X, Y, F, 1, L11),
      fits_in_columns(X, Y, F, 1, L12),
      append(L11, L12, L1),
      find_fits(T, L2),
      append([L1], L2, L).

partition([], _, [], _, [], [], [], []).
partition([HD|TD], PHD, [HL|TL], PHL, [HD|L1], R1, [HL|L2], R2) :-
      length(HL, X),
      length(PHL, Y),
      X =< Y,
      partition(TD, PHD, TL, PHL, L1, R1, L2, R2).
partition([HD|TD], PHD, [HL|TL], PHL, L1, [HD|R1], L2, [HL|R2]) :-
      length(HL, X),
      length(PHL, Y),
      X > Y,
      partition(TD, PHD, TL, PHL, L1, R1, L2, R2).

sort_on_fits([], [], [], []).
sort_on_fits([HD|TD], [HL|TL], Y1, Y2) :-
      partition(TD, HD, TL, HL, Left1, Right1, Left2, Right2),
      sort_on_fits(Left1, Left2, L1, L2),
      sort_on_fits(Right1, Right2, R1, R2),
      append(L1, [HD|R1], Y1),
      append(L2, [HL|R2], Y2).

remove_conflicts_from_one(_, [], []).
remove_conflicts_from_one(E, [H|T], L) :-
      E == H,
      remove_conflicts_from_one(E, T, L).
remove_conflicts_from_one([(X1,Y1),(X2,Y2)], [[(X1,Y1),(_,_)]|T], L) :-
      !,remove_conflicts_from_one([(X1,Y1),(X2,Y2)], T, L).
remove_conflicts_from_one([(X1,Y1),(X2,Y2)], [[(_,_),(X1,Y1)]|T], L) :-
      !,remove_conflicts_from_one([(X1,Y1),(X2,Y2)], T, L).
remove_conflicts_from_one([(X1,Y1),(X2,Y2)], [[(_,_),(X2,Y2)]|T], L) :-
      !,remove_conflicts_from_one([(X1,Y1),(X2,Y2)], T, L).
remove_conflicts_from_one([(X1,Y1),(X2,Y2)], [[(X2,Y2),(_,_)]|T], L) :-
      !,remove_conflicts_from_one([(X1,Y1),(X2,Y2)], T, L).
remove_conflicts_from_one(E, [H|T], L) :-  % no conflict here
      E \== H,
      !,remove_conflicts_from_one(E, T, L1),
      append([H], L1, L).

remove_conflicts(_, [], []).
remove_conflicts(E, [H|T], L) :-
      remove_conflicts_from_one(E, H, H1),
      length(H1, X),
      X > 0,
      remove_conflicts(E, T, L1),
      append([H1], L1, L).

solve([], []).
solve([[H|_]|TL], Sol) :-
      remove_conflicts(H, TL, L),
      solve(L, Sol1),
      append([H], Sol1, Sol).
solve([[_|T]|TL], Sol) :-   % head 'H' can't be the solution
      solve([T|TL], Sol).

check([], _) :- false.          % failure (I use 'false' because it is more declarative than 'fail')
check([H|_], E) :- H == E.      % success
check([_|T], E) :- check(T, E).

% I use my own delete variant, so that my program works both at ECLiPSe and SWI Prolog environments
mydelete(E, [E|Rem], Rem).
mydelete(E, [H|T], [H|Rest]) :-
mydelete(E, T, Rest).

print_row([], _, _, Sol, Sol) :- write('\n').
print_row([E], R, C, Sol, S) :-
      write(E),
      C1 is C+1,
      print_row([], R, C1, Sol, S).
print_row([A,B|T], R, C, Sol, S) :-
      C1 is C+1,
      check(Sol, [(R,C),(R,C1)]),
      mydelete([(R,C),(R,C1)], Sol, S1),
      write(A), write('-'),
      print_row([B|T], R, C1, S1, S).
print_row([A,B|T], R, C, Sol, S) :-
      write(A), write(' '),
      C1 is C+1,
      print_row([B|T], R, C1, Sol, S).

print_midrow([], _, _, Sol, Sol) :- write('\n').
print_midrow([_|T], R, C, Sol, S) :-
      R1 is R+1,
      check(Sol, [(R,C),(R1,C)]),
      mydelete([(R,C),(R1,C)], Sol, S1),
      write('|'), write(' '),
      C1 is C+1,
      print_midrow(T, R, C1, S1, S).
print_midrow([_|T], R, C, Sol, S) :-
      write(' '), write(' '),
      C1 is C+1,
      print_midrow(T, R, C1, Sol, S).

print([], _, _).
print([H|T], R, Sol) :-
      print_row(H, R, 1, Sol, S1),    % in the rows we can see the dominos that have been placed horizontally
      print_midrow(H, R, 1, S1, S2),  % in the mid-rows we can see the dominos that have been placed vertically
      R1 is R+1,
      print(T, R1, S2).

put_dominos :-
      dominos(D),
      find_fits(D, L),            % for each domino find his "fits", a list of all the frame positions where it can possibly fit
      sort_on_fits(D, L, _, SL),  % perform a quicksort variant based on the length of fits
      solve(SL, Sol),             % find a solution, where every domino is used to cover the frame
      frame(F),
      print(F, 1, Sol).           % print the solution, appropriately

% solution body -- end
