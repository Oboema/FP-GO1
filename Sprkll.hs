module Main where

import System.Environment

import Exec--processor

import Lexer--compiler components
import Parser
import Semantics
import Compiler

lookAtRegs =([4,5],[0,1,2])

compileString a	|check		=instr
				|otherwise =error "unchecked semantic error occured" 
				where
					tokens	=tokenize a
					tree	=parseProgram tokens
					check	=verify tree
					instr	=compile tree
					
optTree a	=do
	p	<-readFile a
	putStr $ show $ parseProgram $ tokenize p

optEval::FilePath->IO ()
optEval a	=putStr $ show $ eval [[]] $ parseExpression $ tokenize a

optRun a=do
	p	<-readFile a
	sim lookAtRegs $ compileString p
	

optAssemble a=do
	p	<-readFile a
	showInstrs $ compileString p
	
optHelp a=do
	putStr $ concat $ map (\(o,(d,f))->(o++" : "++d++"\n")) options

	

options::[([Char],([Char],[Char]->IO ()))]
options=[("-r",("run the program in the file",optRun)),
	("-a",("show the assembly of the program in file",optAssemble)),
	("-t",("show the tree of the program in the file",optTree)),
	("-e",("evaluate the expression and show the result",optEval)),
	("-h",("show the help",optHelp))]



main=do
	(option:args)	<-getArgs
	let func=case (lookup option options) of
		Just (d,f)	->f
		Nothing		->error ("unknown option: "++(show option))
	func (head args)
