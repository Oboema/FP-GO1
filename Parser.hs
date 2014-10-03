module Parser(parseExpression, parseProgram) where

import Grammar

data NonTerminal	=Program
					|Stat
					|Block
					|ForDef
					|Print
					|NegExpr
					|BoolExpr
					|AddExpr
					|MulExpr
					|Operand deriving (Show)

consume::[TokenType]->[Token]->(Token,[Token])
consume []	_	=error "parse error: unexpected token"
consume (tt:tts) (x@(t,s):xs)		|tt==t		=(x,xs)
						|otherwise	=consume tts (x:xs)

parseExpression::[Token]->TokenTree						
parseExpression=fst.(parse NegExpr)

parseProgram::[Token]->TokenTree
parseProgram=fst.(parse Program)
					

parse::NonTerminal->[Token]->(TokenTree,[Token])

--Expression parsing

	--Possible Operands(Bool, Int and nested expressions)
parse Operand (t@(Var,_):ts)	=(TokenLeaf t,ts)
parse Operand (t@(Number,_):ts)	=(TokenLeaf t,ts)
parse Operand (t@(BTrue,_):ts)	=(TokenLeaf t,ts)
parse Operand (t@(BFalse,_):ts)	=(TokenLeaf t,ts)
parse Operand (t@(POpen,_):ts)	=(node,rest) where
	(node,r1)	=parse NegExpr ts
	(_,rest)	=consume [PClose] r1
	
	--Layers of operations
	
parse MulExpr (t:ts)			=(node,rest) where
	(tl, r1)	=parse Operand (t:ts)
	(tr, r2)	=parse MulExpr (tail r1)
	(node,rest)	=case r1 of
		((Mul,_):xs)	->(TokenNode (head r1) tl tr,r2)
		((Div,_):xs)	->(TokenNode (head r1) tl tr,r2)
		_			->(tl,r1)
parse AddExpr (t:ts)			=(node,rest) where
	(tl, r1)	=parse MulExpr (t:ts)
	(tr, r2)	=parse AddExpr (tail r1)
	(node, rest)=case r1 of
		((Plus,_):xs)	->(TokenNode (head r1) tl tr,r2)
		((Min,_):xs)		->(TokenNode (head r1) tl tr,r2)
		_			->(tl,r1)
parse BoolExpr (t:ts)			=(node,rest) where
	(tl, r1)	=parse AddExpr (t:ts)
	(tr, r2)	=parse AddExpr (tail r1)
	(node, rest)=case r1 of
		((OpBool,_):xs)	->(TokenNode (head r1) tl tr,r2)
		_			->(tl,r1)
parse NegExpr (t@(Not,_):ts)	=((TokenNode t Nop tr),r) where
	(tr,r)		=parse BoolExpr ts
parse NegExpr t	=parse BoolExpr t

--Statement parsing
parse Program []				=(Nop,[])
parse Program ts				=(TokenNode (Semicolon,";") tl tr,r) where
	(tl, r1)	=parse Stat ts
	(tr, r)		=parse Program r1

parse Block (t@(BClose,_):ts)	=(Nop,ts)
parse Block	ts					=(TokenNode (Semicolon,";") tl tr,r) where
	(tl, r1)	=parse Stat ts
	(tr, r)		=parse Block r1

parse Stat (t@(BOpen,_):ts)		=parse Block ts
	
parse Stat ts@((Semicolon,_):_)	=(Nop, ts)
	
parse Stat (t@(VarVar,_):ts)	=(TokenNode t (TokenLeaf tv) tr ,r) where
	(tv, r1)	=consume [Var] ts
	(tn, r2)	=consume [Assignment] r1
	(tr, r3)	=parse NegExpr r2
	(nt, r)		=consume [Semicolon] r3

parse Stat (t@(ConstVar,_):ts)	=(TokenNode t (TokenLeaf tv) tr ,r) where
	(tv, r1)	=consume [Var] ts
	(tn, r2)	=consume [Assignment] r1
	(tr, r3)	=parse NegExpr r2
	(nt, r)		=consume [Semicolon] r3
	
parse Stat (t@(Var,_):ts)		=(TokenNode tn (TokenLeaf t) tr,r) where
	(tn, r1)	=consume [Assignment] ts
	(tr, r2)	=parse NegExpr r1
	(nt, r)		=consume [Semicolon] r2

parse Stat (t@(While,_):ts)		=(TokenNode t tl tr, r) where
	(tl, r1)	=parse NegExpr ts
	(tr, r)		=parse Stat r1
	
parse Stat (t@(For,_):ts)		=(TokenNode t (TokenNode t tll tlr) (TokenNode t trl trr), r) where
	(tll, r1)	=parse ForDef ts
	(_, r2)		=consume [Semicolon] r1
	(tlr, r3)	=parse NegExpr r2
	(_, r4)		=consume [Semicolon] r3
	(trl, r5)	=parse NegExpr r4
	(trr, r)	=parse Stat r5

parse Stat (t@(If,_):ts)		=(TokenNode t tl (TokenNode tn trl trr), r) where
	(tl, r1)	=parse NegExpr ts
	(tn, r2)	=consume [Then] r1
	(trl, r3)	=parse Stat r2
	(trr, r)	=case r3 of
		((Else,_):xs)	->parse Stat xs
		_				->(Nop,r3)

parse ForDef (t@(VarVar,_):ts)	=(TokenNode t (TokenLeaf tv) tr ,r) where
	(tv, r1)	=consume [Var] ts
	(tn, r2)	=consume [Assignment] r1
	(tr, r)		=parse NegExpr r2

parse nt (t:ts)	=error ("not implemented: "++(show nt)++" on "++(show t))
parse nt []		=error ("not implemented: "++(show nt)++" on empty")