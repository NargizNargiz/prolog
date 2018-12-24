%-----------------INPUT-----------------------------------
formatOutput(X,Y) :- not(list(X)), tab(Y),write(X).
formatOutput([H|T],Y) :- Z is Y+3, formatOutput(H,Z), printTab(T,Z).

printTab([],_).
printTab([H|T],I):- formatOutput(H,I), printTab(T,I).
pr([]):-nl.
pr([T|R]):- printTab(T,2),nl,pr(R).
list([]).
list([_|_]).
printNl([]):- nl.
printNl([H|T]):- write(H), nl, printNl(T).
%---------------------------------------------------------
%+
run :- printNl([	"Hello!", 
					"This is a program that can identify family ties between people."]),
					run1.
%+
run1 :- printNl([	"If you want to find a relationship between two people enter 1",
					"If you want to find all the relatives of a person enter 2",
					"If you want to add a new family member enter 3",
					"If you want to exit the program enter 4"]),
					read(Answer),checkInput(Answer).

%--------------------------------------------------------------------------------------------------
checkInput(Ans) :- 	Ans is 1, printNl(["Enter the first name"]), 
					read(Name1),
					printNl(["Good, enter the second name"]),
					read(Name2),
					getter1(Name1,Name2),run1.

%checkInput(Ans) :- 	Ans is 2, printNl(["Enter name"]),read(Name), getter2(Name),run1.
checkInput(Ans) :- 	Ans is 2, printNl(["Enter name"]),read(Name),checkInput(5,Name) ,run1.
checkInput(Ans) :- Ans is 4, printNl(["Buy!","..."]).

checkInput(Ans) :- Ans is 3, printNl(["Enter exist person name"]),read(Name),checkName(Name).
checkInput(_) :- printNl("Incorrect information, try again"), run1.
checkInput(Ans,N1) :- Ans is 5, printNl(["Enter relation"]),read(Relation), getter3(N1,Relation).

checkInput(Ans,Exist) :- 	Ans is 6, printNl(["Enter new person name"]), read(New),checkName(Exist,New).%Name1 имя которое уже есть в бд
checkInput(Ans,N1,N2) :- 	Ans is 7, printNl(["Good, you entered 2 correct names"]), %N2 - new name 
							printNl(["Enter new person gender(man/woman)"]), read(G),gender(G,N2,N1), checkInput(8,N1,N2).

checkInput(Ans,N1,N2) :- 	Ans is 8, printTab(["Enter relation between",N1, "and", N2],2),nl, 
							read(Rel), checkRel(Rel,N1,N2),run1.

checkInput(_,_,_) :- run1.
%------------------------------------------------------------------------------------------------------------------------
getter1(N1,N2) :- name_db(N1), name_db(N2),getRel1(N1,N2,ProgramAns),printTab(ProgramAns,2),nl.
getter1(N1,N2) :- name_db(N1), name_db(N2),getRel1(N2,N1,ProgramAns),printTab(ProgramAns,2),nl.
getter1(N1,N2) :- not(name_db(N1)),
				 printNl(["Error:name/s are not in the database"]),
				 printNl(["Try again"]), checkInput(1).
getter1(N1,N2) :- not(name_db(N2)),
				 printNl(["Error:name/s are not in the database"]),
				 printNl(["Try again"]), checkInput(1).

getter2(N) :- name_db(N),getRelAll(N,Names,Answer),getterRecur(Names,Answer).
getter2(N) :- not(name_db(N)), 
			  printNl(["Error:name/s are not in the database"]),
			  printNl(["Try again"]), checkInput(2).

getter3(N,R) :- name_db(N), getGr(N,R,Ans), printTab(Ans,2), nl.
getter3(N,R) :- not(name_db(N)), 
				printNl(["Error:name/s are not in the database"]),
				printNl(["Try again"]), checkInput(2).


%Рекурсивно вызвать по Names
getterRecur([],_).
getterRecur(_,[]).
getterRecur([Name|T],[Ans|Answers]) :- getRelAll(Name,_,Answer),printTab(Ans,2),nl, pr(Answer),getterRecur(T,Answers).

%--------------------------------------------------------------------------------------------------------------
gender(Ans,New,_):- Ans == man,assert(man_db(New)).
gender(Ans,New,_):- Ans == woman,assert(woman_db(New)).
%gender(Ans,New,Member):- M = man, W = woman, Ans =\= M, Ans =\= W,checkInput(7,Member,New).

len([],0).
len([T|R], K1):- len(R,K), K1 is K + 1. 

checkName(Name):- name_db(Name), checkInput(6,Name).%все хорошо имя есть в базе данных.
checkName(Name):- not(name_db(Name)), checkInput(3).% имени нет

checkName(Exist,New) :- not(name_db(New)), checkInput(7,Exist,New).
checkName(Exist,New) :- name_db(New),checkInput(6,Exist).
countPar(N1,Res) :- findall(P,parent(P,N1),List), len(List,Res). 
%--------------------------------------------------------------------------------------------------------------------------
%проверка на противоречивость.  R это новое отношение между N1 и N2; N1, N2 - родственник и новый родственник.	
checkRel(R,N1,N2) :- R == parent, assert(parent_db(N1,N2)),assert(name_db(N2)).

checkRel(R,N1,N2) :- R == married, not(marrieds(N1,_)), assert(marrieds_db(N1,N2)), assert(name_db(N2)). 

checkRel(R,N1,N2) :- R == son, not(man(N1)),% 								TESTED+ 
					 printTab(["Error:",N1, "gender is not man."],2),nl, 
					 printNl(["Enter correct relationship"]),
					 checkInput(8,N1,N2).

checkRel(R,N1,N2) :- R == son, man(N1),%									TESTED+
					 countPar(N1,0),
					 printTab(["Good",N1,"can be son",N2],2),nl,
					 assert(name_db(N2)),
					 assert(parent_db(N2,N1)).

checkRel(R,N1,N2) :- R == son, man(N1),%									TESTED+
					 countPar(N1,1),parent(V,N1),man(V),woman(N2),
					 printTab(["Good",N1,"can be son",N2],2),nl,
					 assert(name_db(N2)),
					 assert(parent_db(N2,N1)).
					 %Вставили в БД отношние родитель с новым членом

checkRel(R,N1,N2) :- R == son, man(N1),%									TESTED+
					 countPar(N1,1),parent(V,N1),woman(V),man(N2),
					 printTab(["Good",N1,"can be son",N2],2),nl,
					 assert(name_db(N2)),
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
					 assert(name_db(N2)),
					 assert(parent_db(N2,N1)).
					 
checkRel(R,N1,N2) :- R == daughter, woman(N1),%								 TESTED+
					 countPar(N1,1),parent(V,N1),man(V),woman(N2),
					 printTab(["Good",N1,"can be daughter",N2],2),nl,
					 assert(name_db(N2)),
					 assert(parent_db(N2,N1)).
					 

checkRel(R,N1,N2) :- R == daughter, woman(N1),%								 TESTED+
					 countPar(N1,1),parent(V,N1),woman(V),man(N2),
					 printTab(["Good",N1,"can be daughter",N2],2),nl,
					 assert(parent_db(N2,N1)),
					 assert(name_db(N2)).
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
checkRel(R,N1,N2) :- R == sister, countPar(N1,0),%                 TESTED+	
					 printTab(["Error:",N1, "does not even have a parent"],2),nl,
					 printNl(["I can't add this relation to database"]),
					 checkInput(8,N1,N2).

checkRel(R,N1,N2) :- R == sister, not(woman(N1)),%             			TESTED+
					 printTab(["Error:",N1,"gender is not woman"],2),nl,
					 checkInput(8,N1,N2).

checkRel(R,N1,N2) :- R == sister, woman(N1),% %                			TESTED+
					 countPar(N1,PCount),PCount =\= 0,                
					 printTab(["Good," ,N1,"can be sister",N2],2),nl,
					 assert(name_db(N2)),
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
					 assert(name_db(N2)),
					 parent(V,N1),assert(parent_db(V,N2)).

%-----------------------------------------------------------------------------
checkRel(R,N1,N2) :- R == bride,marrieds(N1,_), %              TESTED+
					 printTab(["Error:",N1, "is married"],2),nl.

checkRel(R,N1,N2) :- R == bride, not(woman(N1)),						%              TESTED+
					 printTab(["Error:",N1,"can't be bride",N2],2),nl,
					 printTab(["Error:",N1, "gender is not woman"],2),nl,
					 checkInput(8,N1,N2).

checkRel(R,N1,N2) :- R == bride, woman(N1),man(N2), not(marrieds(N1,_)),%              TESTED+
					 printTab(["Good," ,N1,"can be bride",N2],2), nl,
					 addParent(N1,N2),
					 assert(name_db(N2)),
					 assert(marrieds_db(N1,N2)).

checkRel(R,N1,N2) :- R == groom, marrieds(N1,_), 		%              TESTED+
					 printTab(["Error:",N1, "is married"],2),nl.
checkRel(R,N1,N2) :- R == groom, not(man(N1)),			%              TESTED+
					 printTab(["Error:",N1,"can't be groom",N2],2),nl,
					 printTab(["Error:",N1, "gender is not man"],2),nl,
					 checkInput(8,N1,N2).

checkRel(R,N1,N2) :- R == groom, man(N1),woman(N2), not(marrieds(N1,_)), %              TESTED+
					 printTab(["Good," ,N1,"can be groom",N2],2),nl,
					 assert(name_db(N2)),
					 assert(marrieds_db(N1,N2)),
					 addParent(N1,N2).

%----------------------------------------------
checkRel(R,N1,N2) :- R == uncle, not(man(N1)), 												%             			TESTED+
					 printTab(["Error:",N1, "gender is not man"],2),nl,checkInput(8,N1,N2).
checkRel(R,N1,N2) :- R == uncle, man(N1), not(siblings(N1,_)), 								%             			TESTED+
					 printTab(["Error:",N1,"does not have siblings"],2),nl,checkInput(8,N1,N2).
					 %ФАЙНДОЛЛ ВЫВОДИТ С ПОВТОРЕНИЯМИ
checkRel(R,N1,N2) :- R == uncle, man(N1), siblings(N1,_), findall(L, siblings(N1,L), List0), 
					listToSet(List0,List),%             			TESTED+
					 printTab([N1,"has siblings:"],2),
					 printTab(List,2), nl,
					 printTab(["Who be parent",N2,"?"],2),nl,
					 read(P),member(P,List),printTab(["Good,", P,"can be parent",N2],2),nl,
					 assert(name_db(N2)),
					 assert(parent_db(P,N2)).

checkRel(R,N1,N2) :- R == uncle,man(N1), siblings(N1,_), findall(L, siblings(N1,L), List0), listToSet(List0,List),
					 printTab([N1,"has siblings:"],2),
					 printTab(List,2), nl,
					 printNl(["Who be parent",N2,"?"]),
					 read(P),not(member(P,List)),
					 printTab(["Error:", P,"can't be parent",N2],2),nl,
					 printTab([P,"is not siblings",N1],2),
					 printNl(["Try again"]),
					 checkRel(uncle,N1,N2).

%----------------------------------------------
checkRel(R,N1,N2) :- R == aunt, not(woman(N1)),%             			TESTED+
					 printTab(["Error:",N1, "gender is not woman"],2),nl,checkInput(8,N1,N2).
checkRel(R,N1,N2) :- R == aunt, woman(N1), not(siblings(N1,_)), 
					 printTab(["Error:",N1,"does not has siblings"],2),nl,checkInput(8,N1,N2).
checkRel(R,N1,N2) :- R == aunt, woman(N1), siblings(N1,_), findall(L, siblings(N1,L), List0), listToSet(List0,List),
					 printTab([N1,"has siblings:"],2), %             			TESTED+
					 printTab(List,2), nl,
					 printTab(["Who be parent",N2,"?"],2),nl,
					 read(P),member(P,List),printTab(["Good,", P,"can be parent",N2],2),nl,
					 assert(name_db(N2)),
					 assert(parent_db(P,N2)).

checkRel(R,N1,N2) :- R == aunt,woman(N1), siblings(N1,_), findall(L, siblings(N1,L), List0),listToSet(List0,List),
													 %             			TESTED+
					 printTab([N1,"has siblings:"],2),
					 printTab(List,2), nl,
					 printTab(["Who be parent",N2,"?"],2),
					 read(P),not(member(P,List)),
					 printTab(["Error:", P,"can't be parent",N2],2),nl,
					 printTab([".",P,"is not siblings",N1],2),
					 printNl(["Try again"]),
					 checkRel(aunt,N1,N2).

%----------------------------------------------
checkRel(R,N1,N2) :- R == grandfather, not(man(N1)),%             			TESTED+
					 printTab(["Error:",N1, "gender is not man"],2),nl,
					 checkInput(8,N1,N2).
checkRel(R,N1,N2) :- R == grandfather, man(N1), not(child(_,N1)), %             			TESTED+
					 printTab(["Error:",N1,"does not even have a child"],2),
				     printNl([" I can't add this relation to database"]),
				     checkInput(8,N1,N2).
				     %ЕСЛИ ОДИН РЕБЕНОК МОЖНО БЕЗ ПРЕДОСТАВЛЕНИЯ ВЫБОРА??
checkRel(R,N1,N2) :- R == grandfather, man(N1),child(_,N1),findall(L, child(L,N1),List0),listToSet(List0,List),
					 printTab([N1,"has children:"],2),%             			TESTED+
					 printTab(List,2), nl,
					 printTab(["Who be parent",N2,"?"],2),nl,
					 read(Ch),member(Ch,List),
					 printTab(["Good,", Ch,"can be parent",N2],2),nl,
					 assert(name_db(N2)),
					 assert(parent_db(Ch,N2)).

checkRel(R,N1,N2) :- R == grandfather, man(N1),child(_,N1),findall(L, child(L,N1),List0),listToSet(List0,List), 
					 printTab([N1,"has children:"],2),%             			TESTED+
					 printTab(List,2), nl,
					 printNl(["Who be parent",N2,"?"]),
					 read(Ch),not(member(Ch,List)),
					 printTab(["Error:", Ch,"can't be parent",N2,".",Ch,"is not child",N1],2),
					 printNl(["Try again"]),
					 checkRel(grandfather,N1,N2).
%----------------------------------------------
checkRel(R,N1,N2) :- R == grandmother, not(woman(N1)),%             			TESTED+
					 printTab(["Error: ",N1, "gender is not woman"],2),nl,
					 checkInput(8,N1,N2).
checkRel(R,N1,N2) :- R == grandmother, woman(N1), not(child(_,N1)),%             			TESTED+ 
					 printTab(["Error: ",N1,"does not even have a child"],2),nl,
				     printNl(["I can't add this relation to database"]),
				     checkInput(8,N1,N2).
checkRel(R,N1,N2) :- R == grandmother, woman(N1),child(_,N1),findall(L, child(L,N1),List0),listToSet(List0,List),
					 printTab([N1,"has children:"],2),%             			TESTED+
					 printTab(List,2), nl,
					 printTab(["Who be parent",N2,"?"],2),nl,
					 read(Ch),member(Ch,List),printTab(["Good,", Ch,"can be parent",N2],2),nl,
					 assert(name_db(N2)),
					 assert(parent_db(Ch,N2)).

checkRel(R,N1,N2) :- R == grandmother, woman(N1),child(_,N1),findall(L, child(L,N1),List0),listToSet(List0,List),
					 printTab([N1,"has children:"],2),%             			TESTED+
					 printTab(List,2), nl,
					 printNl(["Who be parent",N2,"?"]),
					 read(Ch),not(member(Ch,List)),printTab(["Error: ", Ch,"can't be parent",N2,".",Ch,"is not child",N1],2),
					 printNl(["Try again"]),
					 checkRel(grandmother,N1,N2).
%----------------------------------------------
checkRel(R,N1,N2) :- R == granddaughter, not(woman(N1)),
					 printTab(["Error: ",N1, "gender is not woman"],2),nl,
					 checkInput(8,N1,N2).%                    TESTED+
					
checkRel(R,N1,N2) :- R == granddaughter, woman(N1), %                    TESTED+
					 %если нет родителей у N1, мы не можем добавить дедушку\бабушку
					 not(parent(_,N1)),
					 printTab(["Error: ",N1,"Have no parents, I can't add grandparent"],2),nl,
					 checkInput(8,N1,N2).
%N2 это новый\ая дед или бабушка,
checkRel(R,N1,N2) :- R == granddaughter, woman(N1),man(N2), %добавляем дедушку
					 parent(P,N1),countPar(P,1), parent(Gf,P), woman(Gm),  
					 printTab([N1,"has parent: ",P,".",P,"has parent",Gf, "and I can add granddaughter relation"],2),nl,
					 assert(name_db(N2)),
					 assert(parent_db(N2,P)).
checkRel(R,N1,N2) :- R == granddaughter, woman(N1), woman(N2), %добавляем бабушку
					 parent(P,N1),countPar(P,1),parent(Gf,P),man(Gf),
					 printTab(["Error: ",N1,"has parent: ",P,".",P,"has parent",Gf,"and I can add granddaughter relation"],2),nl,
					 assert(name_db(N2)),
					 assert(parent_db(N2,P)).

%----------------------------------------------
checkRel(R,N1,N2) :- R == grandson, not(man(N1)),
					 printTab(["Error: ",N1, "gender is not woman"],2),nl,
					 checkInput(8,N1,N2).					
checkRel(R,N1,N2) :- R == grandson, man(N1),
					 %если нет родителей у N1, мы не можем добавить дедушку\бабушку
					 not(parent(_,N1)),
					 printTab(["Error: ",N1,"Have not parents, I can't add grandparent"],2),nl,
					 checkInput(8,N1,N2).
%N2 это новый\ая дед или бабушка,
checkRel(R,N1,N2) :- R == grandson, man(N1),man(N2), %добавляем дедушку
					 parent(P,N1),countPar(P,1), parent(Gm,P), woman(Gm),  
					 printTab([N1,"has parent: ",P],2),nl,
					 printTab([P,"has parent",Gf],2),nl,
					 printNl(["I can add granddaughter relation"]),
					 assert(name_db(N2)),
					 assert(parent_db(N2,P)).
checkRel(R,N1,N2) :- R == grandson, man(N1), woman(N2), %добавляем бабушку
					 parent(P,N1),countPar(P,1),parent(Gf,P),man(Gf),
					 printTab([N1,"have parent: ",P,".",P,"have parent",Gf,"and I can add granddaughter relation"],2),nl,
					 assert(name_db(N2)),
					 assert(parent_db(N2,P)).
%checkRel(_,_,_) :- printNl(["I cant incorrect input"]).
checkRel(_,_,_) :- printNl(["Incorrect input"]).

%--------------------------------------------
%нужно всех детей N1 связать с N2
addParent(N1,N2):- findall(L,child(L,N1),Res),add(Res,N2).
add([],_).
add([CH|R],N) :- assert(parent_db(N,CH)),add(R,N).
