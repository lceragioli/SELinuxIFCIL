:- use_module(library(clpfd)).

gen(0, empty).

gen(N, ruleset(type(a), R)) :-
    M is N-1,
    M >= 0,
    gen(M, R).

gen(N, ruleset(block(R1), R2)) :-
    N1 is N - 1,
    between(1, N1, M),
    P is N1 - M,
    gen(M, R1),
    gen(P, R2).
