%example 

%family1--------------------------------------
name_db(mary).
name_db(kate).
name_db(alisa).
name_db(alina).
name_db(dasha).
name_db(toma).
name_db(kara).
name_db(rita).
name_db(bob).
name_db(jon).
name_db(andrey).
name_db(danil).
name_db(artur).
name_db(roma).
name_db(ron).
name_db(shone).
name_db(gevorg).
name_db(pole).
name_db(nick).

marrieds_db(jon,toma).
marrieds_db(andrey,alina).
marrieds_db(bob,mary).

%womans-----
woman_db(mary).
woman_db(kate).
woman_db(alisa).

woman_db(alina).
woman_db(dasha).
woman_db(toma).
woman_db(kara).
woman_db(rita).


%mans-------
man_db(bob).
man_db(jon).
man_db(andrey).
man_db(danil).
man_db(artur).
man_db(roma).
man_db(ron).
man_db(shone).
man_db(gevorg).
man_db(pole).
man_db(nick).




parent_db(mary,kate).
parent_db(mary,jon).
parent_db(mary,alisa).
parent_db(bob,kate).
parent_db(bob,jon).
parent_db(bob,alisa).

parent_db(alina,dasha).
parent_db(alina, toma).
parent_db(alina,danil).
parent_db(andrey,dasha).
parent_db(andrey,toma).
parent_db(andrey,danil).

parent_db(toma,shone).
parent_db(toma,gevorg).
parent_db(toma,ron).
parent_db(toma,roma).
parent_db(toma,artur).
parent_db(jon,shone).
parent_db(jon,gevorg).
parent_db(jon,ron).
parent_db(jon,roma).
parent_db(jon,artur).


parent_db(danil,rita).
parent_db(dasha,pole).
parent_db(kate,nick).
parent_db(alisa,kara).