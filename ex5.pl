listToSet(T,L) :- listToSet(T,L,[]).

listToSet([],Acc,Acc).
listToSet([F|T],Res,Acc) :- not(member(F,Acc)), listToSet(T,Res,[F|Acc]).
listToSet([F|T],Res,Acc) :- member(F,Acc), listToSet(T,Res,Acc).
