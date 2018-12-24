:- dynamic fib_db/2.

fib(0,1) :- !.
fib(1,1) :- !.
fib(N,F) :- fib_db(N,F),!.
fib(N,F) :- N1 is N-1, fib(N1,F1),
			N2 is N-2, fib(N2,F2),
			F is F1+F2, asserta(fib_db(N,F)).
%0 1 2 3 4 5 6 7 8
%1 1 2 3 5 8 13 21 34

getRel(N1,N2,[N1, "is parent", N2]) :- parent_db(N1,N2),!.
getRel(N1,N2,[N1, "is parent", N2]) :- parent(N1,N2),assert(parent_db(N1,N2)).

getRel(N1,N2,[N1, "and", N2, "are married"]) :- marrieds(N1,N2).
getRel(N1,N2,[N1, "and", N2, "are married"]) :- marrieds(N1,N2).

getRel1(N1,N2,[N1, "is daughter", N2]) :- daughter_db(N1,N2),!.
getRel1(N1,N2,[N1, "is daughter", N2]) :- daughter(N1,N2),assert(daughter_db(N1,N2)).

getRel1(N1,N2,[N1, "is granddaughter", N2]) :- granddaughter_db(N1,N2),!.
getRel1(N1,N2,[N1, "is granddaughter", N2]) :- granddaughter(N1,N2),assert(granddaughter_db(N1,N2)).

getRel1(N1,N2,[N1, "is sister",N2]) :- sister_db(N1,N2),!.
getRel1(N1,N2,[N1, "is sister",N2]) :- sister(N1,N2),assert(sister_db(N1,N2)).


getRel1(N1,N2,[N1, "is aunt", N2]) :- aunt_db(N1,N2),!.
getRel1(N1,N2,[N1, "is aunt", N2]) :- aunt(N1,N2),assert(aunt_db(N1,N2)).

getRel1(N1,N2,[N1, "is grandmother",N2]) :- grandmother_db(N1,N2),!.
getRel1(N1,N2,[N1, "is grandmother",N2]) :- grandmother(N1,N2),assert(grandmother_db(N1,N2)).

getRel1(N1,N2,[N1, "is bride",N2]) :- bride_db(N1,N2),!.
getRel1(N1,N2,[N1, "is bride",N2]) :- bride(N1,N2),assert(bride_db(N1,N2)).



  getRel1(N1,N2,[N1, "is daughter", N2]) :- daughter(N1,N2).
getRel1(N1,N2,[N1, "is granddaughter", N2]) :- granddaughter(N1,N2).
getRel1(N1,N2,[N1, "is sister",N2]) :- sister(N1,N2).
getRel1(N1,N2,[N1, "is aunt", N2]) :- aunt(N1,N2).
getRel1(N1,N2,[N1, "is grandmother",N2]) :- grandmother(N1,N2).
getRel1(N1,N2,[N1, "is bride",N2]) :- bride(N1,N2).

