:- dynamic 	parent_db/2,
			ball/2,
			save/1.
menu1:- % очистка текущего окна
	write("1 - get ball  "),nl,
	write("2 - add new student "),nl,
	write("0 - exit"),nl,
	read(C), me(C).
me(M):-		M == 1,
			write("enter familia - "), nl,
			read(N), % ждём ввода фамилии
			ball(N, B), % поиск в базе данных фамилии и оценки
			write("ball: "),write(" "),write(B), % если есть, выводим на экран
			read(_), % ждём нажатия любой клавиши
			menu1. % выводим на экран меню
me(M):-		M == 2,
			write("enter famila"),nl,
			read(N), % ждём ввода фамилии
			write("ball"),nl,
			read(B), % ждём ввода оценки
			assert(ball(N,B)), % добавляем запись в базу данных
			menu1. % выводим на экран меню
me(M):-		M == 0,
			save("student.ddb"). % сохраняем базу данных
me(_):-		menu1. 

