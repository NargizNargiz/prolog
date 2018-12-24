:- dynamic 	parent_db/2,
			daughter_db/2,
			bride_db/2,
			aunt_db/2,
			grandmother_db/2,
			granddaughter_db/2,
			sister_db/2,
			marrieds_db/2,
			name_db/1.
%--------------------------------------------
%Программа пролог - Родственники
%Базовые отношения--------------------------------
man(X):- man_db(X).
woman(X):- woman_db(X).

%Родитель, X is parent Y
parent(X,Y) :- parent_db(X,Y).


marrieds(X,Y) :- marrieds1(X,Y); marrieds1(Y,X).

marrieds1(X,Y) :- marrieds_db(X,Y).

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

%uncle
uncle(X,Y):- brother(X,V), parent(V,Y).
uncle(X,Y):- man(X),marrieds(X,V), siblings(V,M), child(Y,M). 
uncle(X,Y):- man(X),marrieds(X,V), aunt(V,Y). 

%aunt 
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

%svekor - X свекор для Y ,отец мужа
svekor(X,Y) :- man(X),woman(Y), marrieds(Y,Z), son(Z,X).

%svekrov ,мать мужа
svekrov(X,Y) :- woman(X), woman(Y), marrieds(Y,Z), son(Z,X).

%tes отец жены
tes(X,Y) :-  man(X), man(Y), marrieds(Y,Z), daughter(Z,X).

%teshcha мама жены
teshcha(X,Y) :- woman(X), man(Y), marrieds(Y,Z), daughter(Z,X).

% cousin - двоюродный брат
cousin(X,Y) :- man(X),parent(Z,X), parent(W,Y), siblings(Z,W).

% cousine - двоюродная сестра
cousine(X,Y) :- woman(X), parent(Z,X), parent(W,Y), siblings(Z,W).

