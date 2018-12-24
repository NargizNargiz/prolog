%большой вывод предложений описывающих отношения с остальными родств.- AnswerS, имена этих родственников - AnswerF
getRelAll(N,AnswerF,AnswerS) :- findall(pair(N2,Ans1),getRel1(N,N2,Ans1),Ans), 
								toSet(Ans,Answer1),
								getSecond(Answer1,AnswerS),
								getFirst(Answer1,AnswerF).
getGr(N,Rel,Ans) :- findall(Name,getRel2(Name,N,Rel),Ans).


getRel1(N1,N2,[N1, "is parent", N2]) :- parent(N1,N2).

getRel1(N1,N2,[N1, "and", N2, "are married"]) :- marrieds(N1,N2).

getRel1(N1,N2,[N1, "is daughter", N2]) 	:- daughter(N1,N2).
getRel1(N1,N2,[N1, "is sister",N2]) 	:- sister(N1,N2).
getRel1(N1,N2,[N1, "is granddaughter", N2]) :- granddaughter(N1,N2).
getRel1(N1,N2,[N1, "is aunt", N2]) 			:- aunt(N1,N2).
getRel1(N1,N2,[N1, "is grandmother",N2]) 	:- grandmother(N1,N2).
getRel1(N1,N2,[N1, "is bride",N2]) 			:- bride(N1,N2).

getRel1(N1,N2,[N1, "is son", N2]) :- son(N1,N2).
getRel1(N1,N2,[N1, "is brother",N2]) :- brother(N1,N2).
getRel1(N1,N2,[N1, "is grandson", N2]) :- grandson(N1,N2).
getRel1(N1,N2,[N1, "is uncle", N2]) :- uncle(N1,N2).
getRel1(N1,N2,[N1, "is grandfather",N2]) :- grandfather(N1,N2).
getRel1(N1,N2,[N1, "is groom",N2]) :- groom(N1,N2).

getRel1(N1,N2,[N1, "is svekor",N2]) :- svekor(N1,N2).
getRel1(N1,N2,[N1, "is svekrov",N2]) :- svekrov(N1,N2).
getRel1(N1,N2,[N1, "is tes",N2]) :- tes(N1,N2).
getRel1(N1,N2,[N1, "is teshcha",N2]) :- teshcha(N1,N2).
getRel1(N1,N2,[N1, "is cousin",N2]) :- cousin(N1,N2).
getRel1(N1,N2,[N1, "is cousine",N2]) :- cousine(N1,N2).

%Вспомогательные функции

getFirst([],[]).
getFirst([pair(F,S)|T],[F|Res]) :- getFirst(T,Res).
getSecond([],[]).
getSecond([pair(F,S)|T],[S|Res]):-getSecond(T,Res).

toSet(T,L) :- toSet(T,L,[]).
toSet([],Acc,Acc).
toSet([pair(F,S)|T],Res,Acc) :- not(memberP(F,Acc)), toSet(T,Res,[pair(F,S)|Acc]). 
toSet([pair(F,S)|T],Res,Acc) :- memberP(F,Acc), toSet(T,Res,Acc). 

memberP(E,[pair(E,S) |_]).
memberP(E,[pair(F,S) |T]) :- memberP(E,T).


listToSet(T,L) :- listToSet(T,L,[]).
listToSet([],Acc,Acc).
listToSet([F|T], Res, Acc):- not(member(F,Acc)), listToSet(T,Res,[F|Acc]).
listToSet([F|T],Res,Acc) :- member(F,Acc), listToSet(T,Res,Acc).




getRel2(N1,N2,R) :- R == parent, parent(N1,N2).

getRel2(N1,N2,R) :- R == marrieds, marrieds(N1,N2).

getRel2(N1,N2,R) :- R == daughter, daughter(N1,N2).
getRel2(N1,N2,R) :- R == sister, sister(N1,N2).
getRel2(N1,N2,R) :- R == granddaughter, granddaughter(N1,N2).
getRel2(N1,N2,R) :- R == aunt, aunt(N1,N2).
getRel2(N1,N2,R) :- R == grandmother, grandmother(N1,N2).
getRel2(N1,N2,R) :- R == bride,	 bride(N1,N2).

getRel2(N1,N2,R) :- R == son, son(N1,N2).
getRel2(N1,N2,R) :- R == brother, brother(N1,N2).
getRel2(N1,N2,R) :- R == grandson, grandson(N1,N2).
getRel2(N1,N2,R) :- R == uncle, uncle(N1,N2).
getRel2(N1,N2,R) :- R == grandfather, grandfather(N1,N2).
getRel2(N1,N2,R) :- R == groom, groom(N1,N2).

getRel2(N1,N2,R) :- R == svekor, svekor(N1,N2).
getRel2(N1,N2,R) :- R == svekrov, svekrov(N1,N2).
getRel2(N1,N2,R) :- R == tes, tes(N1,N2).
getRel2(N1,N2,R) :- R == teshcha, teshcha(N1,N2).
getRel2(N1,N2,R) :- R == cousin, cousin(N1,N2).
getRel2(N1,N2,R) :- R == cousine, cousine(N1,N2).
