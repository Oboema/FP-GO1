module Lexer where

import Grammar
import Data.Char


reserved::[([Char],TokenType)]
reserved	=[
	("var",		DefVar),
	("if",		If),
	("then",	Then),
	("else",	Else),
	("while",	While),
	("for",		For),
	("const",	Const)]

keyword::Token->Token
keyword (Var, s)=(t',s)
	where
		l	=lookup s reserved
		t'	=case l of
			Just t	->t
			Nothing ->Var
keyword t	=t

lexer::[Char]->[Token]
lexer []	=[]
lexer (x:xs)	|isDigit x	=(Number, num)			:lexer rnum
				|isLetter x	=(keyword (Var, var))	:lexer rvar
				|x=='='		=case nxt of
					'='	->(OpBool, [x,nxt])			:(lexer $ tail xs)
					_	->(Assignment, [x])			:lexer xs
				|x=='<'		=case nxt of
					'='	->(OpBool, [x,nxt])			:(lexer $ tail xs)
					_	->(OpBool, [x])				:lexer xs
				|x=='>'		=case nxt of
					'='	->(OpBool, [x,nxt])			:(lexer $ tail xs)
					_	->(OpBool, [x])				:lexer xs
				|x=='&'		=case nxt of
					'&'	->(OpBool, [x,nxt])			:(lexer $ tail xs)
					_	->error "unexpected character"
				|x=='|'		=case nxt of
					'|'	->(OpBool, [x,nxt])			:(lexer $ tail xs)
					_	->error "unexpected character"
				|x=='('		=(POpen, [x])			:lexer xs
				|x==')'		=(PClose, [x])			:lexer xs
				|x=='{'		=(BOpen, [x])			:lexer xs
				|x=='{'		=(BClose, [x])			:lexer xs
				|x=='+'		=(Plus, [x])			:lexer xs
				|x=='-'		=(Min, [x])				:lexer xs
				|x=='*'		=(Mul, [x])				:lexer xs
				|x=='/'		=(Div, [x])				:lexer xs
				|x==';'		=(Statement, [x])		:lexer xs
				|x==' '		=lexer xs
				|otherwise	=error "token error: no matching token left"
				where
					(num,rnum)=span isDigit (x:xs)
					(var,rvar)=span isLetter (x:xs)
					nxt=head xs