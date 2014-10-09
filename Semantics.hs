module Semantics(verify) where

import Grammar
import Debug.Trace

type VarName	=[Char]

-- sprockel types, handig
data SprType	=SInt
				|SBool
				|SVoid deriving (Eq, Show)

type ConstFlag	=Bool

-- iets is of een binding van een var, of een resultaat van expressie. Result == type tussenresultaat
data ValueDef	=Bind VarName ConstFlag SprType
				|Result ConstFlag SprType deriving (Eq,Show)
				
type Scope		=[ValueDef]
type SymTab		=[Scope]

isConst::ValueDef->Bool
isConst (Bind _ f _)	=f
isConst (Result f _)	=f

inScope::Scope->VarName->Maybe ValueDef
inScope ((Result _ _):vs) name		=error "semantic error: unbound value in scope?"
inScope [] name						=Nothing
inScope (td@(Bind vn _ _):vs) name	|name==vn	=Just td
									|otherwise	=inScope vs name
									
-- checkt of shit bestaat in SymTab
getValueDef::SymTab->VarName->ValueDef
getValueDef [] name		=error ("semantic error: "++name++" not defined")
getValueDef (s:ss) name	=case (inScope s name) of
	Just vd	->vd
	Nothing	->getValueDef ss name
	
--  checkt of resultaten in rechter en linker boom legaal zijn, overeenomen met verwachting.
-- assert symtab, treelef, treerig, verwacht tl, verwacht tr, uiteindelijk tree resultaat
assert st tl tr vtl vtr vtres	|vtl==evtl && vtr==evtr	=Result (cfl&&cfr) vtres
								|otherwise				=error "semantic error: unexpected type for operation"
								where
									(Result cfl evtl)	=check st tl
									(Result cfr evtr)	=check st tr

verify::TokenTree->Bool
verify t	=(Result False SVoid)==(check [[]] t)

--change these functions to disable scope feature
openScope::SymTab->SymTab
openScope st=([]:st)

check::SymTab->TokenTree->ValueDef

check st (TokenLeaf (Number,_))					=Result True SInt
check st (TokenLeaf (BFalse,_))					=Result True SBool
check st (TokenLeaf (BTrue,_))					=Result True SBool
check [] (Nop)									=Result False SVoid	--ugly cheat to force checking of last statements
check st (Nop)									=Result False SVoid
check st (TokenLeaf (Var,vn))					=Result cf vdt where
	(Bind _ cf vdt)	=getValueDef st vn
	
-- vdt = value definition type, int of bool
check (cs:st) (TokenNode (VarVar,_) (TokenLeaf (Var,vn)) tr)	
												|(inScope cs vn)==Nothing	=Bind vn False vdt
												|otherwise					=error ("semantic error: "++vn++" already defined in this scope")
												where
													(Result _ vdt)	=check (cs:st) tr
	
check (cs:st) (TokenNode (ConstVar,_) (TokenLeaf (Var,vn)) tr)	
												|(inScope cs vn)==Nothing	=Bind vn True vdt
												|otherwise					=error ("semantic error: "++vn++" already defined in this scope")
												where
													vdt	=case (check (cs:st) tr) of
														(Result True vdt)	->vdt
														(Result False _)	->error ("semantic error: constant has to have a constant expression")

check st (TokenNode (BOpen,_) tl _)				=check (openScope st) tl
																		
check st@(s:ss) (TokenNode (Semicolon,_) tl tr)	=check st' tr where
	st'	=case (check st tl) of
		(Result _ _)	->st
		b@(Bind _ _ _)	->((b:s):ss)
		
check st (TokenNode (Assignment,_) tl tr)		|cfl==False && vtl==vtr	=Result cfr SVoid
												|cfl==True				=error "semantic error: cant assign to constant"
												|otherwise				=error "semantic error: types have to be the same for assignment"
												where
													(Result cfl vtl)	=check st tl
													(Result cfr vtr)	=check st tr
--checks for all operations
													
check st (TokenNode (Plus,_) tl tr)				=assert st tl tr SInt SInt SInt
check st (TokenNode (Min,_) tl tr)				=assert st tl tr SInt SInt SInt
check st (TokenNode (Mul,_) tl tr)				=assert st tl tr SInt SInt SInt
check st (TokenNode (Div,_) tl tr)				=assert st tl tr SInt SInt SInt
check st (TokenNode (OpBool,">=") tl tr)		=assert st tl tr SInt SInt SBool
check st (TokenNode (OpBool,"<=") tl tr)		=assert st tl tr SInt SInt SBool
check st (TokenNode (OpBool,">") tl tr)			=assert st tl tr SInt SInt SBool
check st (TokenNode (OpBool,"<") tl tr)			=assert st tl tr SInt SInt SBool
check st (TokenNode (OpBool,"&&") tl tr)		=assert st tl tr SBool SBool SBool
check st (TokenNode (OpBool,"||") tl tr)		=assert st tl tr SBool SBool SBool

check st (TokenNode (OpBool,"==") tl tr)		|(evtl==evtr)	=Result (cfl&&cfr) SBool
												|otherwise		=error "semantic error: operation requires the same types"
												where
													(Result cfl evtl)	=check st tl
													(Result cfr evtr)	=check st tr
													
check st (TokenNode (OpBool,"!=") tl tr)		|(evtl==evtr)	=Result (cfl&&cfr) SBool
												|otherwise		=error "semantic error: operation requires the same types"
												where
													(Result cfl evtl)	=check st tl
													(Result cfr evtr)	=check st tr
													
check st (TokenNode (Not,_) tl _)				|evtl==SBool			=Result cf evtl
												|otherwise				=error "semantic error: bool required"
												where
													(Result cf evtl)	=check st tl
													
--check if bla  bla bla in (if a<3 then {bla bla bla} is valid)
check st (TokenNode (If,_) tl (TokenNode (Then, _) trl trr))	
												|vdl==SBool	&& crl==crr	=crl
												|otherwise 				=error "semantic error: expression has to have the boolean type"
												where
													crl=check st trl
													crr=check st trr
													(Result cf vdl)	=check st tl

check st (TokenNode (While,_) tl tr)			|evtl==SBool	=check st tr
												|otherwise		=error "semantic error: while needs a boolean expression"
												where
													(Result cf evtl)	=check st tl


check st (TokenNode (tt,ts) tl tr)				=error ("semantic error: check not yet implemented for "++(show tt))