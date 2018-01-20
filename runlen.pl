/*
Author: Georgios Kamaras
Date: 23/03/2017
*/

unwrap((_,0),[]).       % base case, to stop recursion in case we initially had an element more than once
unwrap((E,N),[E|T]) :-  %  in case we initially have an element more than once
  N >= 1,
  N1 is N-1,
  unwrap((E,N1),T).
unwrap(A,[A]).          % in case we have an element only once

decode_rl([],[]).       % base case
decode_rl([Hin|Tin],L) :-
  unwrap(Hin,L1),       % unwrap head
  decode_rl(Tin,L2),    % decode the rest of the sequence
  append(L1,L2,L).      % append the lists with the "results"

wrap([],E,1,[E]).       % base case, for one element of a kind read so far
wrap([],E,N,[(E,N)]).   % base case, for more than one element of a kind read so far
wrap([E|T],E,N,L) :-    % if list's head and previously read element are the same symbolically (let E)...
  N1 is N+1,            % ...then count it
  wrap(T,E,N1,L).
wrap(L1,E,N,[E|L]) :-   % if list's head and previously read element E are NOT the same...
  N == 1,               % ...and we have seen the previous only once then we want to return it in a proper way,...
  encode_rl(L1,L).      % ...and yet, not "lose" the head in the recursion
wrap(L1,E,N,[(E,N)|L]) :-
  encode_rl(L1,L).

encode_rl([H|T],L) :-
  wrap(T,H,1,L).        % start wrapping the sequence

/*
  Based in my definitions, for the question:
?- encode_rl([p(3),p(X),q(X),q(Y),q(4)], L).
  I take the answer:
X = 3
Y = 3
L = [(p(3), 2), (q(3), 2), q(4)]
Yes (0.00s cpu, solution 1, maybe more)
  (in the ECLiPSe system).
  This happens, because when encoding I check if the current head of the list is symbolically the same
with the previously read element. In Prolog, the capitals 'X' and 'Y' are variables, so, when solving the
problem (encoding the sequence) according to the rules that I provided, Prolog will try to assign values
to them in order for the 1st element to be symbolically exactly the same with the 2nd and the 3rd element
to be exactly the same with the 4th. This happens for X = 3 and Y = 3. The 1st element is p(3), so in
the 2nd element p(X) X will become 3 and, as a result the 3rd element will become q(3), so in the 4th
element q(Y), Y will become 3. And because of these assignments for X and Y, the final grouping (encoding)
has the form of: [(p(3), 2), (q(3), 2), q(4)].
*/
