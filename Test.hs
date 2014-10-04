module Test where

import Grammar
import Lexer
import Parser
import Semantics

ifTree=TokenNode (If,"if") 
	(TokenNode (OpBool,"==") 
		(TokenLeaf (Number, "3")) 
		(TokenLeaf (Var, "c")))
	
	(TokenNode (Then,"then") 
		(TokenNode (Semicolon, "" ) Nop Nop) --thentree
		(TokenNode (Semicolon, "" ) Nop Nop)) --elsetree
		
testMulExpr="2*3/4"
testAddExpr="2+3*4+3"
testNegExpr=""++testMulExpr++">="++testAddExpr

toTreeExpression=parseExpression.tokenize

toTree::[Char]->TokenTree
toTree =parseProgram.tokenize

readtf::IO [Char]
readtf=readFile "prog.sprkll"

topLevel::TokenTree->[TokenTree]
topLevel (TokenLeaf _)					=error "Invalid parse tree"
topLevel (Nop)							=[]
topLevel (TokenNode (Semicolon,_) tl tr)=tl:(topLevel tr)
topLevel (TokenNode t tl tr)			=error "Invalid parse tree"

tokens=do
	a<-readtf
	t<-return $ tokenize a
	return t
	
tree=do
	ts<-tokens
	return $ parseProgram ts
	
trees=do
	t<-tree
	return $ topLevel t
	
tcheck=do
	t<-tree
	return $ verify t