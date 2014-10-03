module Grammar where

data TokenType	=Plus
				|Min
				|Mul
				|Div
				|OpBool
				|Not
				|Statement
				|Assignment
				|If
				|Then
				|Else
				|While
				|For
				|Number
				|Var
				|ConstVar
				|VarVar
				|POpen
				|PClose
				|BOpen
				|BClose
				|BTrue
				|BFalse
				|Nop deriving (Eq,Show)
				

type Token	=(TokenType,[Char])

data TokenTree 	=TokenLeaf Token
				|TokenNode Token TokenTree TokenTree deriving (Eq)
				
showTree::Int->TokenTree->[Char]
showTree level (TokenLeaf t)		=(concat $ replicate (level-1) "    ")++(show t)
showTree 0	t						=(showTree 1 t)++"\n"
showTree level (TokenNode t tl tr)	=(concat $ replicate (level-1) "    ")++(show t)++"\n"
										++(showTree (level+1) tl)++"\n"
										++(showTree (level+1) tr)
										
instance Show TokenTree where
	show t=showTree 0 t
 
printTree::TokenTree->IO ()
printTree t=putStr (show t)