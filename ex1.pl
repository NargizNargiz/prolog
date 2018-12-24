domains 
	char_list = char*
	str = string

predicates
	conver(str, char_list)
clauses
	conver(Str,[H|T]):- print(Str).