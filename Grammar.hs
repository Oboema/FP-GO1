module Grammar where

data TokenType	=Plus
				|Min
				|Mul
				|Div
				|OpBool
				|Statement
				|DefVar
				|Assignment
				|If
				|Then
				|Else
				|While
				|For
				|Number
				|Var
				|Const
				|POpen
				|PClose
				|BOpen
				|BClose
				|Nop deriving (Eq,Show)
				

type Token	=(TokenType,[Char])

data TokenTree 	=TokenLeaf Token
				|TokenNode Token TokenTree TokenTree
			
			
ifstat=TokenNode (If,"if") 
	(TokenNode (OpBool,"==") 
		(TokenLeaf (Number, "3")) 
		(TokenLeaf (Var, "c")))
	
	(TokenNode (Then,"then") 
		(TokenNode (Statement, "" ) (TokenLeaf (Nop,"")) (TokenLeaf (Nop,""))) --thentree
		(TokenNode (Statement, "" ) (TokenLeaf (Nop,"")) (TokenLeaf (Nop,"")))) --elsetree