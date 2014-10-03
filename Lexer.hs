module Lexer(tokenize) where

import Grammar
import Data.Char


reserved::[([Char],TokenType)]
reserved	=[
	("var",		VarVar),
	("if",		If),
	("then",	Then),
	("else",	Else),
	("while",	While),
	("for",		For),
	("const",	ConstVar),
	("true", 	BTrue),
	("false",	BFalse)]

keyword::Token->Token
keyword (Var, s)=(t',s)
	where
		l	=lookup s reserved
		t'	=case l of
			Just t	->t
			Nothing ->Var
keyword t	=t

tokenize::[Char]->[Token]
tokenize []	=[]
tokenize (x:xs)	|isDigit x	=(Number, num)			:tokenize rnum
				|isLetter x	=(keyword (Var, var))	:tokenize rvar
				|x=='='		=case xs of
					'=':r	->(OpBool, x:"=")		:(tokenize $ tail xs)
					_		->(Assignment, [x])		:tokenize xs
				|x=='<'		=case xs of
					'=':r	->(OpBool, x:"=")		:(tokenize $ tail xs)
					_		->(OpBool, [x])			:tokenize xs
				|x=='>'		=case xs of
					'=':r	->(OpBool, x:"=")		:(tokenize $ tail xs)
					_		->(OpBool, [x])			:tokenize xs
				|x=='&'		=case xs of
					'&':r	->(OpBool, x:"&")		:(tokenize $ tail xs)
					_		->error "lexer error: unexpected character"
				|x=='|'		=case xs of
					'|':r	->(OpBool, x:"|")		:(tokenize $ tail xs)
					_		->error "lexer error: unexpected character"
				|x=='!'		=(Not, [x])				:tokenize xs
				|x=='('		=(POpen, [x])			:tokenize xs
				|x==')'		=(PClose, [x])			:tokenize xs
				|x=='{'		=(BOpen, [x])			:tokenize xs
				|x=='}'		=(BClose, [x])			:tokenize xs
				|x=='+'		=(Plus, [x])			:tokenize xs
				|x=='-'		=(Min, [x])				:tokenize xs
				|x=='*'		=(Mul, [x])				:tokenize xs
				|x=='/'		=(Div, [x])				:tokenize xs
				|x==';'		=(Semicolon, [x])		:tokenize xs
				|isSpace x	=tokenize xs
				|otherwise	=error ("lexer error: no matching token left for: "++(x:xs))
				where
					(num,rnum)=span isDigit (x:xs)
					(var,rvar)=span isLetter (x:xs)