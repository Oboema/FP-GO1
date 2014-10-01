
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
				|POpen
				|PClose
				|BOpen
				|BClose
				|Nop
				

type Token	=(TokenType,[Char])

TokenTree 	=TokenLeaf Token
			|TokenNode Token TokenTree TokenTree
			
			
if	=TokenTree (If,"if") 
	(TokenTree (OpBool,"==") 
		(TokenLeaf (Number, "3")) 
		(TokenLeaf (Var, "c")))
	
	(TokenTree (Then,"then") 
		(TokenNode (Statement, "" ) (TokenLeaf (Nop,"") (TokenLeaf (Nop,"")))) --thentree
		(TokenNode (Statement, "" ) (TokenLeaf (Nop,"") (TokenLeaf (Nop,""))))) --elsetree