module Grammar where

data TokenType	=Start
				|Plus
				|Min
				|Mul
				|Div
				|OpBool
				|Not
				|Semicolon
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
				|BFalse deriving (Eq,Show)
				

type Token	=(TokenType,[Char])

data TokenTree 	=TokenLeaf Token
				|Nop
				|TokenNode Token TokenTree TokenTree deriving (Eq)
				
spacing=replicate 4 ' '
				
showTree::Int->TokenTree->[Char]
showTree level (TokenLeaf t)		=(concat $ replicate (level-1) spacing)++(show t)
showTree level (Nop)				=(concat $ replicate (level-1) spacing)++"No operation"
showTree 0	t						=(showTree 1 t)++"\n"
showTree level (TokenNode t tl tr)	=(concat $ replicate (level-1) spacing)++(show t)++"\n"
										++(showTree (level+1) tl)++"\n"
										++(showTree (level+1) tr)
										
instance Show TokenTree where
	show t=showTree 0 t
 
printTree::TokenTree->IO ()
printTree t=putStr (show t)