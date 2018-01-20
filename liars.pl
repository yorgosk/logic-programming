/*
Author: Georgios Kamaras
Date: 29/03/2017
*/

% I use my own "subset" statement, the one we saw in class
mysubset([], []).
mysubset([X|Set], [X|Subset]) :-
    mysubset(Set, Subset).
mysubset([_|Set], Subset) :-
    mysubset(Set, Subset).

evaluate([], [], _).
evaluate([Hin|Tin], [Hl|Tl], N) :-
    Hin =:= Hl,     % we have a supposed liar
    N < Hin,        % if we have less liars than supposed-liar tells us, then he may be lying, indeed
    evaluate(Tin, Tl, N).
evaluate([Hin|Tin], [], N) :-    % by construction, we are going to have a supposed truth-teller
    N >= Hin,       % if we have at least as many liars as supposed-truth-teller tells us
    evaluate(Tin, [], N).           % move with no liars left
evaluate([Hin|Tin], Liars, N) :-    % by construction, we are going to have a supposed truth-teller and there are still liars left
    N >= Hin,       % if we have at least as many liars as supposed-truth-teller tells us
    evaluate(Tin, Liars, N).        % move with liars left

spotliars([], [], []).
spotliars([Hin|Tin], [Hl|Tl], Lout) :-
    Hin =:= Hl,                 % we have a liar
    spotliars(Tin, Tl, L1),
    append([1], L1, Lout).
spotliars([_|Tin], Liars, Lout) :-  % by construction, we have a truth-teller
    spotliars(Tin, Liars, L1),
    append([0], L1, Lout).

liars(Lin, Liars) :-
    mysubset(Lin, Lout),
    length(Lout, N),
    evaluate(Lin, Lout, N),   % if evaluate finishes successfully, then we have ourselves a solution
    spotliars(Lin, Lout, Liars).    % translate above solution to 0s and 1s
