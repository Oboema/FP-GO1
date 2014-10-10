use .\make to generate an executable file with following commandline options
-r : run the program in the file
-a : show the assembly of the program in file
-t : show the tree of the program in the file
-e : evaluate the expression and show the result
-h : show the help

To run the compiler in ghci use the file Sprkll.hs and the opt* functions
optRun filename : open the file, compile the content and run with sim
optAssemble fn	: open the file, compile the content and show the result
optTree fn		: open the file and generate a tree. The tree will not be verified
optEval	expr	: tokenize,parse and evaluate the expression expr

The file Tests.hs contains some functions to debug a tree