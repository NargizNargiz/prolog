getRel1(N1,N2,[N1, "is daughter", N2]) :- daughter(N1,N2).
getRel1(N1,N2,[N2, "is daughter", N1]) :- daughter(N2,N1).
getRel1(N1,N2,[N1, "is granddaughter", N2]) :- granddaughter(N1,N2).
getRel1(N1,N2,[N2, "is granddaughter", N1]) :- granddaughter(N2,N1).
getRel1(N1,N2,[N1, "is sister",N2]) :- sister(N1,N2).
getRel1(N1,N2,[N2, "is sister",N1]) :- sister(N2,N1).
getRel1(N1,N2,[N1, "is aunt", N2]) :- aunt(N1,N2).
getRel1(N1,N2,[N2, "is aunt", N1]) :- aunt(N2,N1).
getRel1(N1,N2,[N2, "is grandmother",N1]) :- grandmother(N2,N1).
getRel1(N1,N2,[N1, "is grandmother",N2]) :- grandmother(N1,N2).
getRel1(N1,N2,[N1, "is bride",N2]) :- bride(N1,N2).
getRel1(N1,N2,[N2, "is bride",N1]) :- bride(N2,N1).



getRel(N1,N2,[N1, "is son", N2]) :- son(N1,N2).
getRel(N1,N2,[N2, "is son", N1]) :- son(N2,N1).
getRel(N1,N2,[N1, "is grandson", N2]) :- grandson(N1,N2).
getRel(N1,N2,[N2, "is grandson", N1]) :- grandson(N2,N1).
getRel(N1,N2,[N1, "is brother",N2]) :- brother(N1,N2).
getRel(N1,N2,[N2, "is brother",N1]) :- brother(N2,N1).
getRel(N1,N2,[N1, "is uncle", N2]) :- uncle(N1,N2).
getRel(N1,N2,[N2, "is uncle", N1]) :- uncle(N2,N1).
getRel(N1,N2,[N1, "is grandfather",N2]) :- grandfather(N1,N2).
getRel(N1,N2,[N2, "is grandfather",N1]) :- grandfather(N2,N1).
getRel(N1,N2,[N1, "is groom",N2]) :- groom(N1,N2).
getRel(N1,N2,[N2, "is groom",N1]) :- groom(N2,N1).
getRel(N1,N2,[N1, "is daughter's husband",N2]) :- husb_daughter(N1,N2).
getRel(N1,N2,[N2, "is daughter's husband",N1]) :- husb_daughter(N2,N1).



%getRel1(N1,N2,[N1, "is daughter", N2]) 	 :- daughter(N1,N2),assert(daughter_db(N1,N2)).
%getRel1(N1,N2,[N1, "is sister",   N2]) 		 :- sister(N1,N2),assert(sister_db(N1,N2)).
%getRel1(N1,N2,[N1, "is granddaughter", N2]) :- granddaughter(N1,N2),assert(granddaughter_db(N1,N2)).
%getRel1(N1,N2,[N2, "is aunt",    N1]) 		:- aunt(N2,N1),assert(aunt_db(N1,N2)).
%getRel1(N1,N2,[N2, "is grandmother",N1]) 	:- grandmother(N2,N1), assert(grandmother_db(N1,N2)).
%getRel1(N1,N2,[N2, "is bride",N1]) 		:- bride(N2,N1), assert(bride_db(N1,N2)).


%getRel1(N1,N2) :- daughter(N1,N2).
%getRel1(N1,N2) :- granddaughter(N1,N2).
%getRel1(N1,N2) :- sister(N1,N2).
%getRel1(N1,N2) :- aunt(N1,N2).
%getRel1(N1,N2) :- grandmother(N1,N2).
%getRel1(N1,N2) :- bride(N1,N2).

