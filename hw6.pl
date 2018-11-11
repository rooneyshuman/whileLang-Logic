% Pretty Printer
pps(_, val(V)) :- write(V).     % Print a value
pps(_, vrb(V)) :- write(V).     % Print a variable
pps(_, bin(*, V1, V2)) :- write('('), pps(N,V1), write(' * '), pps(N,V2), write(') ').  
pps(_, bin(+, V1, V2)) :- write('('), pps(N,V1), write(' + '), pps(N,V2), write(') ').
pps(_, bin(-, V1, V2)) :- write('('), pps(N,V1), write(' - '), pps(N,V2), write(') ').
pps(_, bin(>, V1, V2)) :- write('('), pps(N,V1), write(' > '), pps(N,V2), write(') ').
pps(_, bin(<, V1, V2)) :- write('('), pps(N,V1), write(' < '), pps(N,V2), write(') ').
pps(N, assign(V,E)) :- tab(N), pps(N,V), write(' := '), pps(N,E), nl.
pps(N, if(E,S1,S2)) :- tab(N), write('if '), pps(N,E), write(' then'), nl, N1 is N+2, pps(N1, S1), tab(N), write('else '), nl, pps(N1, S2).
pps(N, while(E,S)) :- tab(N), write('while '), pps(N,E), write('do'), nl, N1 is N+2, pps(N1,S).
pps(N, out(V)) :- tab(N), write('output '), pps(N,V), nl.
pps(N, comp(L)) :- tab(N), write('begin'), nl, N1 is N+2, ppl(N1, L).
ppl(N, []) :- N1 is N-2, tab(N1), write('end'), nl.  %prints end of compound list
ppl(N, [H|T]) :- pps(N, H), ppl(N, T).               %prints compound list

% Interpret functionality
:- dynamic table/2.         % Set up dynamic table

eval(vrb(V), R) :- table(V, R). 
eval(val(V), R) :- R is V.
eval(bin(*, V1, V2), R) :- eval(V1, R1), eval(V2, R2), R is (R1 * R2).
eval(bin(+, V1, V2), R) :- eval(V1, R1), eval(V2, R2), R is (R1 + R2).
eval(bin(-, V1, V2), R) :- eval(V1, R1), eval(V2, R2), R is (R1 - R2).
eval(bin(>, V1, V2), R) :- eval(V1, R1), eval(V2, R2), R1 > R2 -> R is 1; R is 0.
eval(bin(<, V1, V2), R) :- eval(V1, R1), eval(V2, R2), R1 < R2 -> R is 1; R is 0.
getVar(vrb(V), R) :- R = V.   % Get variable's letter
intp(if(E, S1, S2)) :- eval(E, R), R = 1 -> intp(S1); intp(S2).        % If successful
intp(assign(V, E)) :- eval(E, R), getVar(V, V1), N is R, (table(V1,_) -> retract(table(V1,_));!), assert(table(V1,N)).
intp(out(V)) :- getVar(V, L), table(L,R), write(L), write(' = '), write(R), nl.
intp(while(E, S)) :- eval(E, R), (R = 1 -> intp(S), intp(while(E,S));!).
intp(comp(L)) :- intpl(L).              % Call compound list interpreter
intpl([]).                              % End of compound list
intpl([H|T]) :- intp(H), intpl(T).      % Interpret compound list elements
