toset(T,L) :- toSet(T,L,[]).

toSet([],Acc,Acc).
toSet([pair(F,S)|T],Res,Acc) :- not(memberP(F,Acc)), toSet(T,Res,[pair(F,S)|Acc]). 
toSet([pair(F,S)|T],Res,Acc) :- memberP(F,Acc), toSet(T,Res,Acc). 

memberP(E,[pair(E,S) |_]).
memberP(E,[pair(F,S) |T]) :- memberP(E,T).

