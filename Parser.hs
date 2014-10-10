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

consume::TokenType->[Token]->(Token,[Token])
consume a (x:xs)|a==(fst x)	=(x,xs)
				|otherwise		=error ("parser error: expected "++(show a)++" but found "++(show x))
				
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
	(_,rest)	=consume PClose r1
	
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
		((Min,_):xs)	->(TokenNode (head r1) tl tr,r2)
		_			->(tl,r1)
		
parse BoolExpr (t:ts)			=(node,rest) where
	(tl, r1)	=parse AddExpr (t:ts)
	(tr, r2)	=parse BoolExpr (tail r1)
	(node, rest)=case r1 of
		((OpBool,_):xs)	->(TokenNode (head r1) tl tr,r2)
		_			->(tl,r1)
		
parse NegExpr (t@(Not,_):ts)	=((TokenNode t tl Nop),r) where
	(tl,r)		=parse BoolExpr ts
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

parse Stat (t@(BOpen,_):ts)		=(TokenNode t tl Nop,r) where
	(tl, r)	=parse Block ts
	
parse Stat (t@(Semicolon,_):ts)	=parse Stat ts
	
parse Stat (t@(VarVar,_):ts)	=(TokenNode t (TokenLeaf tv) tr ,r) where
	(tv, r1)	=consume Var ts
	(tn, r2)	=consume Assignment r1
	(tr, r3)	=parse NegExpr r2
	(nt, r)		=consume Semicolon r3

parse Stat (t@(ConstVar,_):ts)	=(TokenNode t (TokenLeaf tv) tr ,r) where
	(tv, r1)	=consume Var ts
	(tn, r2)	=consume Assignment r1
	(tr, r3)	=parse NegExpr r2
	(nt, r)		=consume Semicolon r3
	
parse Stat (t@(Var,_):ts)		=(TokenNode tn (TokenLeaf t) tr,r) where
	(tn, r1)	=consume Assignment ts
	(tr, r2)	=parse NegExpr r1
	(nt, r)		=consume Semicolon r2

parse Stat (t@(While,_):ts)		=(TokenNode t tl (TokenNode (Semicolon,";") tr Nop), r) where
	(tl, r1)	=parse NegExpr ts
	(tr, r)		=parse Stat r1
	
{-
3e statement wordt impliciet in a gestopt.
2e statement moet Bool Expr zijn
1e statement moet var def en assignment zijn.
for var a = 3; b <4; d + 1 
{
    a=4;
}
-}
-- for loop maken was kut, we schrijven t gewoon om naar een while.
parse Stat (t@(For,_):ts)		=
	((TokenNode (BOpen,"{")
		(TokenNode (Semicolon,";")
			tll 
			(TokenNode (Semicolon, ";") 
				(TokenNode (While,"while") 
					tlr 
					(TokenNode (Semicolon,";") 
						trr
						(TokenNode (Assignment,"=") tvn trl) 
					)
				)
				Nop
			)
		)
		Nop
	),r)where

	(tll@(TokenNode _ tvn _), r1)	=parse ForDef ts
	(_, r2)		=consume Semicolon r1
	(tlr, r3)	=parse NegExpr r2
	(_, r4)		=consume Semicolon r3
	(trl, r5)	=parse NegExpr r4
	(trr, r)	=parse Stat r5

parse Stat (t@(If,_):ts)		=(TokenNode t tl (TokenNode tn (TokenNode (Semicolon,";") trl Nop) (TokenNode (Semicolon,";") trr Nop)), r) where
	(tl, r1)	=parse NegExpr ts
	(tn, r2)	=consume Then r1
	(trl, r3)	=parse Stat r2
	(trr, r)	=case r3 of
		((Else,_):xs)	->parse Stat xs
		_				->(Nop,r3)

parse ForDef (t@(VarVar,_):ts)	=(TokenNode t (TokenLeaf tv) tr ,r) where
	(tv, r1)	=consume Var ts
	(tn, r2)	=consume Assignment r1
	(tr, r)		=parse NegExpr r2

parse nt (t:ts)	=error ("parser error: "++(show nt)++" on "++(show t)++" is not implemented")
parse nt []		=error ("parser error: "++(show nt)++" on empty is not implemented")