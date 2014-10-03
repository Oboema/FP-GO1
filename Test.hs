module Test where

import Grammar
import Lexer
import Parser

nonToken	=TokenLeaf (Nop,"")

ifTree=TokenNode (If,"if") 
	(TokenNode (OpBool,"==") 
		(TokenLeaf (Number, "3")) 
		(TokenLeaf (Var, "c")))
	
	(TokenNode (Then,"then") 
		(TokenNode (Statement, "" ) nonToken nonToken) --thentree
		(TokenNode (Statement, "" ) nonToken nonToken)) --elsetree
		
testMulExpr="2*3/4"
testAddExpr="2+3*4+3"
testNegExpr=""++testMulExpr++">="++testAddExpr

toTreeExpression=parseExpression.tokenize

toTree::[Char]->TokenTree
toTree =parseProgram.tokenize

readtf::IO [Char]
readtf=readFile "prog.sprkll"

tokens=do
	a<-readtf
	t<-return $ tokenize a
	return t
	
tree=do
	ts<-tokens
	return $ parseProgram ts