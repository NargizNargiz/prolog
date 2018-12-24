formatOutput(X,Y) :- not(list(X)), tab(Y),write(X).
formatOutput([H|T],Y) :- Z is Y+3, formatOutput(H,Z), printTab(T,Z).

printTab([],_).
printTab([H|T],I):- formatOutput(H,I), printTab(T,I).

list([]).
list([_|_]).

printNl([]):- nl.
printNl([H|T]):- write(H), nl, printNl(T).
%----------------------------
parent(mary,kate).
%----------------------------
getRel(N1,N2,[N1, "is parent", N2]) :- parent(N1,N2).
getRel(N1,N2,[N1, "is parent", N2]) :- parent(N2,N1).

checkInput(Ans) :- 	Ans is 1, printNl(["Enter the first name"]), 
					read(Name1),printNl(["Good, enter the second name"]),read(Name2),
					getRel(Name1,Name2,ProgramAns),printTab(ProgramAns,2).

checkInput(Ans) :- Ans is 2.
checkInput(Ans) :- Ans is 3, printNl(["Use the predicate assert/1 to add new member"]).
checkInput(Ans) :- Ans is 4, printNl(["Buy!","..."]).

runn :- printNl([	"Hello!", 
					"This is a program that can identify family ties between people.",
					"If you want to find a relationship between two people enter 1",
					"If you want to find all the relatives of a person enter 2",
					"If you want to add a new family member enter 3",
					"If you want to exit the program enter 4"]),
				read(Answer),checkInput(Answer).
%parent(D,S),phh([S])

