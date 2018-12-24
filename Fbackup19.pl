:- dynamic 	parent_db/2,
			daughter_db/2,
			bride_db/2,
			aunt_db/2,
			grandmother_db/2,
			granddaughter_db/2,
			sister_db/2,
			%marrieds_db/2,
			name_db/1.
%--------------------------------------------

%Программа пролог - Родственники
%Базовые отношения--------------------------------

man(X):- man_db(X).
woman(X):- woman_db(X).

%Родитель, X is parent Y
parent(X,Y) :- parent_db(X,Y).


marrieds(X,Y) :- marrieds1(X,Y); marrieds1(Y,X).

marrieds1(X,Y) :- parent(X,R),parent(Y,R),man(X), woman(Y).
marrieds1(X,Y) :- parent(X,R),parent(Y,R),woman(X),man(Y).

% X is child Y
child(X,Y):- parent(Y,X).

%X is son Y
son(X,Y):- man(X), parent(Y,X).

%X is daughter Y
daughter(X,Y):- woman(X),parent(Y,X).


%grandson-внук
grandson(X,Y):- man(X), parent(V,X), parent(Y,V).

%granddaughter-внучк
granddaughter(X,Y):- woman(X), parent(V,X), parent(Y,V).

%внуки,внучки
grandchildren(X,Y):- grandson(X,Y).
grandchildren(X,Y):- granddaughter(X,Y).

%sister
sister(X,Y) :- woman(X),dif(X,Y),parent(V,X), parent(V,Y).

%brother
brother(X,Y) :- man(X),dif(X,Y),parent(V,X),parent(V,Y).

%siblings - братья, сестры
siblings(X,Y):- sister(X,Y).
siblings(X,Y):- brother(X,Y).

%siblings(X,Y):- brother(Y,X).
%siblings(X,Y):- sister(Y,X).

%uncle - дядя
uncle(X,Y):- brother(X,V), parent(V,Y).
uncle(X,Y):- man(X),marrieds(X,V), siblings(V,M), child(Y,M). 
uncle(X,Y):- man(X),marrieds(X,V), aunt(V,Y). 

%aunt - тетя
aunt(X,Y):- sister(X,V), parent(V,Y).
aunt(X,Y):- woman(X),marrieds(X,V), siblings(V,M), child(Y,M). 
%aunt(X,Y):- woman(X),marrieds(X,V), uncle(V,Y).

%grandfather
grandfather(X,Y):- man(X), grandchildren(Y,X).
grandfather(X,Y):- man(X), parent(X,V), parent(V,Y).
%grandfather(X,Y):- man(X), marrieds(X,V), grandmother(V,Y).

%grandmother
grandmother(X,Y):- woman(X), grandchildren(Y,X).
grandmother(X,Y):- woman(X), parent(X,V), parent(V,Y).
grandmother(X,Y):- woman(X), marrieds(X,V), grandfather(V,Y).

%bride - невеста
bride(X,Y):- woman(X),marrieds(X,Y).
%bride(X,Y):-groom(Y,X).
%groom - жених
groom(X,Y):- man(X), marrieds(X,Y).
groom(X,Y):-bride(Y,X).

%зять, Y - это родитель дочери
husb_daughter(X,Y) :- man(X),daughter(D,Y), marrieds(X,D).
husb_daughter(X,Y) :- man(X),parent(Y,D),woman(D),marrieds(X,D).

%-----------------INPUT-----------------------------------
formatOutput(X,Y) :- not(list(X)), tab(Y),write(X).
formatOutput([H|T],Y) :- Z is Y+3, formatOutput(H,Z), printTab(T,Z).

printTab([],_).
printTab([H|T],I):- formatOutput(H,I), printTab(T,I).

list([]).
list([_|_]).

printNl([]):- nl.
printNl([H|T]):- write(H), nl, printNl(T).

%person(name(),ListP)

getRel1(N1,N2,[N1, "is parent", N2]) :- parent(N1,N2).
getRel1(N1,N2,[N1, "is parent", N2]) :- parent(N1,N2),assert(parent_db(N1,N2)).

getRel1(N1,N2,[N1, "and", N2, "are married"]) :- marrieds(N1,N2).
getRel1(N1,N2,[N1, "and", N2, "are married"]) :- marrieds(N1,N2).

% в базе данных хранить для каждого человека список известных отношений в которых он состоит 

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


getRelAll(N,Answer) :- findall(pair(N2,Ans1),getRel1(N,N2,Ans1),Ans), toSet(Ans,Answer).
%getRelAll(N,Answer) :- findall(pair(N2,Ans1),getRel1(N,N2,Ans1),Ans),toSet(Ans,Answer).

run :- printNl([	"Hello!", 
					"This is a program that can identify family ties between people.",
					"If you want to find a relationship between two people enter 1",
					"If you want to find all the relatives of a person enter 2",
					"If you want to add a new family member enter 3",
					"If you want to exit the program enter 4"]),
				read(Answer),checkInput(Answer).

checkInput(Ans) :- 	Ans is 1, printNl(["Enter the first name"]), 
					read(Name1),printNl(["Good, enter the second name"]),read(Name2),
					getRel1(Name1,Name2,ProgramAns),printTab(ProgramAns,2).

checkInput(Ans) :- 	Ans is 2, printNl(["Enter name"]),read(Name), getRelAll(Name,Answer),printNl(Answer).
checkInput(Ans) :- 	Ans is 3, 
					printNl(["If you need add new relationship between exist person and new person, enter 5"]),
					read(Ans1), checkInput(Ans1).
checkInput(Ans) :- Ans is 4, printNl(["Buy!","..."]).

checkInput(Ans) :- Ans is 5, printNl(["Enter exist person name"]), read(Name),checkName(Name).
checkInput(Ans,Exist) :- 	Ans is 6, printNl(["Enter new person name"]), read(New),checkName(Exist,New).%Name1 имя которое уже есть в бд
checkInput(Ans,N1,N2) :- 	Ans is 7, assert(name_db(N2)), printNl(["Good, you entered 2 correct names"]), %N2 - new name 
							printNl(["Enter new person gender(man/woman)"]), read(G),gender(G,N2), checkInput(8,N1,N2).

checkInput(Ans,N1,N2) :- 	Ans is 8, printTab(["Enter relation between",N1, "and", N2],2),nl, read(Rel), checkRel(Rel,N1,N2), printNl(["End."]).
%----------------------------------------------
gender(Ans,N):- Ans == man,assert(man_db(N)).
gender(Ans,N):- Ans == woman,assert(woman_db(N)).


len([],0).
len([T|R], K1):- len(R,K), K1 is K + 1. 

checkName(Name):- name_db(Name), checkInput(6,Name).%все хорошо имя есть в базе данных.
checkName(Name):- not(name_db(Name)), checkInput(5).% имени нет

checkName(Exist,New) :- not(name_db(New)), checkInput(7,Exist,New).
checkName(Exist,New) :- name_db(New),checkInput(6,Exist).
%----------------------------------------------
countPar(N1,Res) :- findall(P,parent(P,N1),List), len(List,Res). 
%----------------------------------------------
%проверка на противоречивость.  R это новое отношение между Р1 и Р2; P1, P2 родственник и новый родственник;Ans ответ.				
%printNl(["Enter correct relationship"])
checkRel(R,N1,N2) :- R == son, not(man(N1)),% 								TESTED+ 
					 printTab(["Error:",N1, "gender is not man."],2),nl, 
					 printNl(["Enter correct relationship"]),
					 checkInput(8,N1,N2).

checkRel(R,N1,N2) :- R == son, man(N1),%									TESTED+
					 countPar(N1,0),
					 printTab(["Good",N1,"can be son",N2],2),nl,
					 assert(parent_db(N2,N1)).

checkRel(R,N1,N2) :- R == son, man(N1),%									TESTED+
					 countPar(N1,1),parent(V,N1),man(V),woman(N2),
					 printTab(["Good",N1,"can be son",N2],2),nl,
					 assert(parent_db(N2,N1)).
					 %Вставили в БД отношние родитель с новым членом

checkRel(R,N1,N2) :- R == son, man(N1),%									TESTED+
					 countPar(N1,1),parent(V,N1),woman(V),man(N2),
					 printTab(["Good",N1,"can be son",N2],2),nl,
					 assert(parent_db(N2,N1)).
					 %Вставили в БД отношние родитель с новым членом

checkRel(R,N1,N2) :- R == son, man(N1),%									 TESTED+
					 countPar(N1,2),printTab(["Error:", N1,"can not be son",N2],2),nl,
					 printNl(["He already has 2 parents"]),
					 checkInput(8,N1,N2).

checkRel(R,N1,N2) :- R == son, %											 TESTED+
					 man(N1),man(N2),countPar(N1,1),parent(V,N1),man(V), %у N1 уже есть папа
					 printTab(["Error:", N1,"can not be son",N2],2),
					 printNl(["He already has dad"]),
					 checkInput(8,N1,N2).
%-----------------------------------------------------------
checkRel(R,N1,N2) :- R == daughter, not(woman(N1)),%						 TESTED+
					 printTab(["Error:",N1, "gender is not woman."],2),nl,
					 printNl(["Enter correct relationship"]),
				     checkInput(8,N1,N2).

checkRel(R,N1,N2) :- R == daughter, woman(N1),%								 TESTED+
					 countPar(N1,0),
					 printTab(["Good",N1,"can be daughter",N2],2),nl,
					 assert(parent_db(N2,N1)).
					 
checkRel(R,N1,N2) :- R == daughter, woman(N1),%								 TESTED+
					 countPar(N1,1),parent(V,N1),man(V),woman(N2),
					 printTab(["Good",N1,"can be daughter",N2],2),nl,
					 assert(parent_db(N2,N1)).
					 

checkRel(R,N1,N2) :- R == daughter, woman(N1),%								 TESTED+
					 countPar(N1,1),parent(V,N1),woman(V),man(N2),
					 printTab(["Good",N1,"can be daughter",N2],2),nl,
					 assert(parent_db(N2,N1)).
					 %Вставили в БД отношние родитель с новым членом

checkRel(R,N1,N2) :- R == daughter, woman(N1),	%							 TESTED+
					 countPar(N1,2),
					 printTab(["Error:", N1,"can not be daughter",N2],2),nl,
					 printNl(["She already has 2 parents"]),
					 checkInput(8,N1,N2).

checkRel(R,N1,N2) :- R == daughter, woman(N1),woman(N2),	%                 TESTED+				  
					 countPar(N1,1),parent(V,N1),woman(V),% у N1 уже есть мама
					 printTab(["Error:",N1,"can not be daughter",N2],2),nl,
					 printNl(["She already has a mom"]),
					 checkInput(8,N1,N2).
%----------------------------------------------------------------------
%может быть такой вариант что у N1 нет родителей, тогда в базу не засунуть
checkRel(R,N1,N2) :- R == sister, countPar(N1,0),
					 printTab(["Error:",N1, "does not even have a parent"],2),nl,
					 printNl(["I can't add this relation to database"]),
					 checkInput(8,N1,N2).

checkRel(R,N1,N2) :- R == sister, not(woman(N1)),%             			TESTED+
					 printTab(["Error:",N1,"gender is not woman"],2),nl,
					 checkInput(8,N1,N2).

checkRel(R,N1,N2) :- R == sister, woman(N1),% %                			TESTED+
					 countPar(N1,PCount),PCount =\= 0,                
					 printTab(["Good," ,N1,"can be sister",N2],2),nl,
					 parent(V,N1),assert(parent_db(V,N2)).

checkRel(R,N1,N2) :- R == brother, %                 					TESTED+
					 countPar(N1,0),
					 printTab(["Error:",N1, "does not even have a parent."],2),nl,
					 printNl(["I can't add this relation to database"]),
					 checkInput(8,N1,N2).

checkRel(R,N1,N2) :- R == brother, not(man(N1)),%		                 TESTED+
					 printTab(["Error:", N1, "gender is not man"],2),nl,
					 checkInput(8,N1,N2).

checkRel(R,N1,N2) :- R == brother, man(N1), 
					 countPar(N1,PCount),PCount =\= 0, %                 TESTED+
					 printTab(["Good," ,N1,"can be brother",N2],2),nl,
					 parent(V,N1),assert(parent_db(V,N2)).

%-----------------------------------------------------------------------------
checkRel(R,N1,N2) :- R == bride,
					 findall(L,child(L,N1),Res),len(Res,0),
					 printTab(["Error:",N1,"does not even have a child"],2),nl,
				     printNl(["I can't add this relation to database"]),
				     checkInput(8,N1,N2).

checkRel(R,N1,N2) :- R == bride, woman(N1),man(N2), not(marrieds(N1,_)),%              TESTED+
					 printTab(["Good," ,N1,"can be bride",N2],2), nl,
					 addParent(N1,N2).
checkRel(R,N1,N2) :- R == bride, not(woman(N1)),						%              TESTED+
					 printTab(["Error:",N1,"can't be bride",N2],2),nl,
					 printTab(["Error:",N1, "gender is not woman"],2),nl,
					 checkInput(8,N1,N2).
checkRel(R,N1,N2) :- R == bride, woman(N1),man(N2), marrieds(N1,_), %                 TESTED+
					 printTab(["Error:",N1,"can't be bride",N2],2),nl,
					 printNl(["She is marrieds."]),
					 checkInput(8,N1,N2).

checkRel(R,N1,N2) :- R == groom,findall(L,child(L,N1),Res),len(Res,0),
					 printTab(["Error:",N1,"does not even have a child"],2),nl,
				     printNl(["I can't add this relation to database"]),
					 checkInput(8,N1,N2).

checkRel(R,N1,N2) :- R == groom, man(N1),woman(N2), not(marrieds(N1,_)),
					 printTab(["Good," ,N1,"can be groom",N2],2),nl,
					 addParent(N1,N2).
checkRel(R,N1,N2) :- R == groom, not(man(N1)),
					 printTab(["Error:",N1,"can't be groom",N2],2),nl,
					 printTab(["Error:",N1, "gender is not man"],2),nl,
					 checkInput(8,N1,N2).
checkRel(R,N1,N2) :- R == groom, man(N1),woman(N2), marrieds(N1,_),
					 printTab(["Error:",N1,"can't be groom",N2,".",N1,"marrieds"],2),nl,
					 checkInput(8,N1,N2).

%----------------------------------------------
checkRel(R,N1,N2) :- R == uncle, not(man(N1)),printNl([N1, "gender is not man","Error"]),checkInput(8,N1,N2).
checkRel(R,N1,N2) :- R == uncle, man(N1), not(siblings(N1,_)), printNl([N1,"does not have siblings"]),checkInput(8,N1,N2).
checkRel(R,N1,N2) :- R == uncle, man(N1), siblings(N1,_), findall(L, siblings(N1,L), List), 
					 printTab([N1,"have siblings:"],2),
					 printTab(List,2), nl,
					 printNl(["Who be parent",N2,"?"]),
					 read(P),member(P,List),printNl(["Good,", P,"can be parent",N2]),
					 assert(parent_db(P,N2)).

checkRel(R,N1,N2) :- R == uncle,man(N1), siblings(N1,_), findall(L, siblings(N1,L), List), 
					 printTab([N1,"have siblings:"],2),
					 printTab(List,2), nl,
					 printNl(["Who be parent",N2,"?"]),
					 read(P),not(member(P,List)),printNl([ P,"can't be parent",N2,".",P,"is not siblings",N1]),
					 printNl(["Try again"]),
					 checkRel(uncle,N1,N2).

%----------------------------------------------
checkRel(R,N1,N2) :- R == aunt, not(woman(N1)),printNl([N1, "gender is not woman","Error"]),checkInput(8,N1,N2).
checkRel(R,N1,N2) :- R == aunt, woman(N1), not(siblings(N1,_)), printNl([N1,"does not have siblings"]),checkInput(8,N1,N2).
checkRel(R,N1,N2) :- R == aunt, woman(N1), siblings(N1,_), findall(L, siblings(N1,L), List), 
					 printTab([N1,"have siblings:"],2),
					 printTab(List,2), nl,
					 printNl(["Who be parent",N2,"?"]),
					 read(P),member(P,List),printNl(["Good,", P,"can be parent",N2]),
					 assert(parent_db(P,N2)).

checkRel(R,N1,N2) :- R == aunt,woman(N1), siblings(N1,_), findall(L, siblings(N1,L), List), 
					 printTab([N1,"have siblings:"],2),
					 printTab(List,2), nl,
					 printNl(["Who be parent",N2,"?"]),
					 read(P),not(member(P,List)),printNl([ P,"can't be parent",N2,".",P,"is not siblings",N1]),
					 printNl(["Try again"]),
					 checkRel(aunt,N1,N2).

%----------------------------------------------
checkRel(R,N1,N2) :- R == grandfather, not(man(N1)),printNl([N1, "gender is not man","Error"]),checkInput(8,N1,N2).
checkRel(R,N1,N2) :- R == grandfather, man(N1), not(child(_,N1)), 
					 printNl([N1,"does not even have a child"]),
				     printNl(["I can't add this relation to database"]).
checkRel(R,N1,N2) :- R == grandfather, man(N1),child(_,N1),findall(L, child(L,N1),List),
					 printTab([N1,"have children:"],2),
					 printTab(List,2), nl,
					 printNl(["Who be parent",N2,"?"]),
					 read(Ch),member(Ch,List),printNl(["Good,", Ch,"can be parent",N2]),
					 assert(parent_db(Ch,N2)).

checkRel(R,N1,N2) :- R == grandfather, man(N1),child(_,N1),findall(L, child(L,N1),List),
					 printTab([N1,"have children:"],2),
					 printTab(List,2), nl,
					 printNl(["Who be parent",N2,"?"]),
					 read(Ch),not(member(Ch,List)),printNl([ Ch,"can't be parent",N2,".",Ch,"is not child",N1]),
					 printNl(["Try again"]),
					 checkRel(grandfather,N1,N2).
%----------------------------------------------
checkRel(R,N1,N2) :- R == grandmother, not(woman(N1)),printNl([N1, "gender is not woman","Error"]),checkInput(8,N1,N2).
checkRel(R,N1,N2) :- R == grandmother, woman(N1), not(child(_,N1)), 
					 printNl([N1,"does not even have a child"]),
				     printNl(["I can't add this relation to database"]).
checkRel(R,N1,N2) :- R == grandmother, woman(N1),child(_,N1),findall(L, child(L,N1),List),
					 printTab([N1,"have children:"],2),
					 printTab(List,2), nl,
					 printNl(["Who be parent",N2,"?"]),
					 read(Ch),member(Ch,List),printNl(["Good,", Ch,"can be parent",N2]),
					 assert(parent_db(Ch,N2)).

checkRel(R,N1,N2) :- R == grandmother, woman(N1),child(_,N1),findall(L, child(L,N1),List),
					 printTab([N1,"have children:"],2),
					 printTab(List,2), nl,
					 printNl(["Who be parent",N2,"?"]),
					 read(Ch),not(member(Ch,List)),printNl([ Ch,"can't be parent",N2,".",Ch,"is not child",N1]),
					 printNl(["Try again"]),
					 checkRel(grandmother,N1,N2).
%----------------------------------------------
% нет родителей у N1 
% у N1 есть 1 родитель(2го нет), у него уже есть 2 родителя, то есть у N1 есть уже бабушка и дедушка
% у N1 есть 1 родитель(2го нет), у него уже есть 1 родитель,
% ищем сразу нужного по полу партнера среди родителей родителя N1
checkRel(R,N1,N2) :- R == granddaughter, not(woman(N1)),printNl([N1, "gender is not woman","Error"]),checkInput(8,N1,N2).
					
checkRel(R,N1,N2) :- R == granddaughter, woman(N1), 
					 %если нет родителей у N1, мы не можем добавить дедушку\бабушку
					 not(parent(_,N1)),printNl([N1,"Have not parents, I can't add grandparent"]),checkInput(8,N1,N2).
%N2 это новый\ая дед или бабушка,
checkRel(R,N1,N2) :- R == granddaughter, woman(N1),man(N2), %добавляем дедушку
					 parent(P,N1),countPar(P,1), parent(Gm,P), woman(Gm),  
					 printNl([N1,"have parent: ",P,".",P,"have parent",Gf, "and I can add granddaughter relation"]),
					 assert(parent_db(N2,P)).
checkRel(R,N1,N2) :- R == granddaughter, woman(N1), woman(N2), %добавляем бабушку
					 parent(P,N1),countPar(P,1),parent(Gf,P),man(Gf),
					 printNl([N1,"have parent: ",P,".",P,"have parent",Gf,"and I can add granddaughter relation"]),
					 assert(parent_db(N2,P)).
checkRel(_,_,_) :- printNl(["I cant incorrect input"]).
%----------------------------------------------
checkRel(R,N1,N2) :- R == grandson, not(man(N1)),printNl([N1, "gender is not woman","Error"]),checkInput(8,N1,N2).					
checkRel(R,N1,N2) :- R == grandson, man(N1),
					 %если нет родителей у N1, мы не можем добавить дедушку\бабушку
					 not(parent(_,N1)),printNl([N1,"Have not parents, I can't add grandparent"]),checkInput(8,N1,N2).
%N2 это новый\ая дед или бабушка,
checkRel(R,N1,N2) :- R == grandson, man(N1),man(N2), %добавляем дедушку
					 parent(P,N1),countPar(P,1), parent(Gm,P), woman(Gm),  
					 printNl([N1,"have parent: ",P,".",P,"have parent",Gf, "and I can add granddaughter relation"]),
					 assert(parent_db(N2,P)).
checkRel(R,N1,N2) :- R == grandson, man(N1), woman(N2), %добавляем бабушку
					 parent(P,N1),countPar(P,1),parent(Gf,P),man(Gf),
					 printNl([N1,"have parent: ",P,".",P,"have parent",Gf,"and I can add granddaughter relation"]),
					 assert(parent_db(N2,P)).
checkRel(_,_,_) :- printNl(["I cant incorrect input"]).
%--------------------------------------------
%нужно всех детей N1 связать с N2
addParent(N1,N2):- findall(L,child(L,N1),Res),add(Res,N2).
add([],_).
add([CH|R],N) :- assert(parent_db(N,CH)),add(R,N).


toSet(T,L) :- toSet(T,L,[]).
toSet([],Acc,Acc).
toSet([pair(F,S)|T],Res,Acc) :- not(memberP(F,Acc)), toSet(T,Res,[pair(F,S)|Acc]). 
toSet([pair(F,S)|T],Res,Acc) :- memberP(F,Acc), toSet(T,Res,Acc). 

memberP(E,[pair(E,S) |_]).
memberP(E,[pair(F,S) |T]) :- memberP(E,T).

member(E,[E|_]).
member(E,[F|T]):- member(E,T).


%for testing
inp(N):-getRelAll(N,Ans),printNl(Ans).
inp2(N1,N2) :- getRel1(N1,N2,ProgramAns),printTab(ProgramAns,2),!.



%зять, Y - это родитель дочери
husb_daughter(X,Y) :- man(X),daughter(D,Y), marrieds(X,D).
husb_daughter(X,Y) :- man(X),parent(Y,D),woman(D),marrieds(X,D).