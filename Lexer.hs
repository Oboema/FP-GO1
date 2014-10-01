import Grammar

reserved::[([Char],TokenType)]
reserved	=[
	("var",		DefVar),
	("if",		If),
	("then",	Then),
	("else",	Else),
	("while",	While),
	("for",		For)]

keyword::Token->Token
keyword (Var, s)=(t',s)
	where
		l=lookup s reserved
		t'=case l of
			Just t	->t
			Nothing ->Var

lexer::[Char]->[Token]
lexer []	=[]
lexer (x:xs)	|isDigit x	=(Number,num):lexer rnum
				|isLetter x	=(Var,var):lexer rvar
				|x=='('		=(POpen,[x]):lexer xs
				|x==')'		=(PClose,[x]):lexer xs
				|x==' '		=lexer xs
				|otherwise	=error "token error: no matching token left"
				where
					(num,rnum)=span isDigit (x:xs)
					(var,rvar)=span isLetter (x:xs)