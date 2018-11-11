%              TESTING PROGRAMS

factorial(F) :- 
    F = comp([
        assign(vrb(n),val(5)), 
        assign(vrb(r),val(1)), 
        while(bin(>,vrb(n),val(0)), 
        comp([
            assign(vrb(r), bin(*,vrb(r),vrb(n))), 
            assign(vrb(n), bin(-,vrb(n),val(1)))
        ])), out(vrb(r))]).

fibonacci(I) :-
    I = comp([
        assign(vrb(c),val(10)), 
        assign(vrb(n),val(0)), 
        assign(vrb(r),val(1)), 
        assign(vrb(i),val(0)), 
        while(bin(<,vrb(i),vrb(c)), 
            comp([
                out(vrb(n)),        
                assign(vrb(s),bin(+,vrb(n),vrb(r))), 
                assign(vrb(n),vrb(r)), 
                assign(vrb(r),vrb(s)), 
                assign(vrb(i),bin(+,vrb(i),val(1)))
            ]))
        ]).

test(T) :- 
    T = comp([
        assign(vrb(n), val(5)), 
        if(bin(>, vrb(n), val(6)),
        comp([
            assign(vrb(r), val(2)),
            assign(vrb(n), bin(-, vrb(n), val(1)))
        ]), assign(vrb(r), val(3))),
        out(vrb(r)),
        out(vrb(n))
    ]).

% TESTS
int(factorial) :- factorial(F), intp(F), !.
int(fibonacci) :- fibonacci(I), intp(I), !.
int(test) :- test(T), intp(T), !.
pp(test) :- test(T), pps(0, T), !.
pp(factorial) :- factorial(F), pps(0, F), !.
pp(fibonacci) :- fibonacci(I), pps(0, I), !.

interp :- intp(assign(vrb(x), val(3))), intp(out(vrb(x))).
ifstat :- intp(if(bin(>,val(1), val(2)), assign(vrb(x), val(5)), assign(vrb(x), val(6)))),
            intp(out(vrb(x))).

whiles :- intp(comp([assign(vrb(x), val(3)), while(bin(>,vrb(x),val(0)),
        assign(vrb(x), bin(-,vrb(x),val(1)))), out(vrb(x))])).

