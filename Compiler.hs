module Compiler(compile,eval) where 

import Grammar
import Debug.Trace
import qualified TypesEtc as TE
import Exec

tkn n tl tr = TokenNode n tl tr 
tkl n = TokenLeaf n 


-- r1 and r3 are used for stuff, I'll just use 4-6 then.
-- disregard that, conditional jumps check regA == reg1 and jump if regA == 1.
-- So we should store all results of expressions in regA
r0=0
regA=1  -- don't want to write TE.regA everywhere. Stupid double type names :( )
r4=4
r5=5
r6=6

--we need to pop to avoid overloading the stack when doing a conditional jump
-- doesn't really matter as there is an unlimited stack, but this keeps the ouput
-- of sim readable for long running programs
pop = TE.Pop r0

--holy shit wat een typewerk, dat moet mooier kunnen. 
--Ik denk dat het definieren van Plus Minus OpBool While Assignment etc als 1 type TokenType een fout was.
--zo veel makkelijker als je TokenType = Op | Statement | NumVar ofzo hebt, met Op = Plus | Min | Mul, 
--Statement = For | While | Assignment etc etc
--Dit wordt ook in de parser gedaan, maar nadat je het daar zorgvuldig opbouwt flikker je het weg =( 


type Scop   = [(String, TE.Value)]
type SymTb  = [Scop]

bool2num :: Token -> Token
bool2num tok@(ttype, _) | ttype == BFalse   = (Number, "0")
                        | ttype == BTrue    = (Number, "1")
                        | otherwise         = tok

-- search and destroy bools and convert them to numbers
pp :: TokenTree -> TokenTree
pp (Nop)                = (Nop)
pp (TokenLeaf leaf)     = tkl $ bool2num leaf
pp (TokenNode n tl tr)  = (TokenNode n (pp tl) (pp tr))



addr2int :: TE.Value -> Int
addr2int (TE.Addr k)   = k 

inScop :: Scop -> String -> Maybe TE.Value
inScop [] vname                     = Nothing
inScop ((vname', addr):scs) vname   | vname == vname'   = Just addr
                                    | otherwise         = inScop scs vname

									
									

getAddr :: String -> SymTb -> TE.Value
getAddr vname []                        = error ("variable "++(show vname)++" not found")
getAddr vname (st:sts)  = case (inScop st vname) of
    Just varaddr    -> varaddr
    Nothing         -> getAddr vname sts 
                            -- reg

term2asm :: SymTb -> Token -> Int -> TE.Assembly
term2asm st (toktp, tokname) r  | toktp == Var      = TE.Load (getAddr tokname st ) r
                                | toktp == Number   = TE.Load (TE.Imm (read tokname :: Int))  r
                                | otherwise         = error ("got terminal "++(show (toktp,tokname))++" which was unexpected")

countScope::Scop->Int
countScope []		=0
countScope ((_,(TE.Addr _)):bs)	=1+(countScope bs)
countScope ((_,(TE.Imm _)):bs)		=0+(countScope bs)
								
currentAddress::SymTb->Int
currentAddress []		=0
currentAddress (s:ss)	=(countScope s)+(currentAddress ss)
								
toInt::Bool->Int
toInt True	=1
toInt False	=0

eval::SymTb->TokenTree->Int
eval mm (TokenLeaf (Number,s))		=read s::Int
eval mm (TokenLeaf (BFalse,_))			=0
eval mm (TokenLeaf (BTrue,_))			=1
eval mm (TokenLeaf (Var,vn))			=v' where
											v'=case (getAddr vn mm) of
												TE.Addr a	->error "Tried to evaluate a variable"
												TE.Imm i	->i
eval mm (TokenNode (Plus,_) tl tr)		=(eval mm tl) + (eval mm tr)
eval mm (TokenNode (Min,_) tl tr)		=(eval mm tl) - (eval mm tr)
eval mm (TokenNode (Grammar.Mul,_) tl tr)		=(eval mm tl) * (eval mm tr)
eval mm (TokenNode (Grammar.Div,_) tl tr)		=(eval mm tl) `div` (eval mm tr)
eval mm (TokenNode (OpBool, "<=") tl tr)=toInt ((eval mm tl) <= (eval mm tr))
eval mm (TokenNode (OpBool, ">=") tl tr)=toInt ((eval mm tl) >= (eval mm tr))
eval mm (TokenNode (OpBool, "<") tl tr)	=toInt ((eval mm tl) < (eval mm tr))
eval mm (TokenNode (OpBool, ">") tl tr)	=toInt ((eval mm tl) > (eval mm tr))
eval mm (TokenNode (OpBool, "&&") tl tr)=toInt (((eval mm tl) + (eval mm tr)) ==2)
eval mm (TokenNode (OpBool, "||") tl tr)=toInt (((eval mm tl) + (eval mm tr)) >=1)
eval mm (TokenNode (OpBool, "==") tl tr)=toInt ((eval mm tl) == (eval mm tr))
eval mm (TokenNode (OpBool, "!=") tl tr)=toInt ((eval mm tl) /= (eval mm tr))

eval mm (TokenNode (Grammar.Not,_) tl _)		=(1-(eval mm tl))
								
cmpE :: SymTb -> TokenTree -> [TE.Assembly]
cmpE st Nop                             = []
cmpE st (TokenLeaf tok)                 = [ term2asm st tok regA, TE.Push regA]

cmpE st (TokenNode op@(_, opStr) tl tr) =  opCodes where

--cmpE st (TokenNode (opType, opStr) (TokenLeaf (Var,vn1)) (TokenLeaf (Var, vn2))  ) =
    opCodes = case opStr of  
            "+"  -> doOp TE.Add --[Compute Add r4 r5 r6, Push r6]
            "-"  -> doOp TE.Sub --[Compute Sub r4 r5 r6, Push r6]
            "*"  -> doOp TE.Mul --[Compute Sub r4 r5 r6, Push r6]
            "/"  -> doOp TE.Div
            "==" -> doOp TE.Equal
            "!=" -> doOp TE.NEq
            ">"  -> doOp TE.Gt
            "<"  -> doOp TE.Lt
            "||" -> doOp TE.Or
            "&&" -> doOp TE.And
            ">=" -> cmpE st (TokenNode (OpBool, "||") (TokenNode (OpBool,">") tl tr) (TokenNode (OpBool,"==") tl tr) )
            "<=" -> cmpE st (TokenNode (OpBool, "||") (TokenNode (OpBool,"<") tl tr) (TokenNode (OpBool,"==") tl tr) )
            "!"  -> [TE.Pop r4, TE.Compute TE.Not r4 r4 regA ]--, TE.Push regA]
        where doOp op = (cmpE st tl) ++ (cmpE st tr) ++ [TE.Pop r5, TE.Pop r4, TE.Compute op r4 r5 regA, TE.Push regA]


                -- symbol table
cmpP :: Int -> SymTb -> TokenTree -> [TE.Assembly]
                        --node        |--                            tl                        --|  tr         
--cmpP nvar st (TokenNode (Semicolon,_) (TokenNode stmt (TokenNode sl@(sltp,slval)) sr@(srtp,srval))  tr )
cmpP nvar (st:sts) (TokenNode (Semicolon,_) tl@(TokenNode stmt subsl subsr) tr ) =
                                        
    case stmt of    
        (VarVar,"var")  -> 
            let
                TokenLeaf (Var, vname)  = subsl
                instr					= cmpE (st:sts) subsr
                st'                     = ((vname, (TE.Addr nvar)):st)

            in  instr++[TE.Pop 4, TE.Store (TE.Addr 4) nvar] ++ cmpP (nvar+1) (st':sts) tr

			
        (ConstVar,"const")  -> 
            let
                TokenLeaf (Var, vname)  = subsl
                n						= eval (st:sts) subsr
                st'                     = ((vname, (TE.Imm n)):st)

            in  [TE.Store (TE.Imm n) nvar] ++ cmpP (nvar+1) (st':sts) tr
			
        (Assignment,"=")->
            let
                TokenLeaf (Var, vname)  = subsl
                expr                    = subsr
                varaddr                 = (addr2int (getAddr vname (st:sts)) )
            in (cmpE (st:sts) expr) ++ [TE.Pop r4, TE.Store (TE.Addr r4) varaddr ] ++ cmpP nvar (st:sts) tr
        
                        --subsr is always Nop with Bopen  ||  tr is after the scope so we throw it away
        (BOpen, "{")    ->  (cmpP nvar ([]:st:sts) subsl) ++ cmpP nvar (st:sts) tr 

        (If, "if")      ->
            let
                (TokenNode (Then, "then") trueTree falseTree)   = subsr
                cond        = cmpE (st:sts) subsl
                trueStat    = cmpP nvar (st:sts) trueTree
                falseStat   = cmpP nvar (st:sts) falseTree
                falseJump   = [TE.Jump TE.UR (1 + (length trueStat))]
                trueJump    = [pop, TE.Jump TE.CR (1 + (length (falseStat++falseJump)))]
            in cond ++ trueJump ++ falseStat ++ falseJump ++ trueStat ++ cmpP nvar (st:sts) tr

        (While, "while")    ->
            let
                cond        = cmpE (st:sts) subsl
                whileStat   = cmpP nvar (st:sts) subsr
                jumpOut     = [TE.Compute TE.Not regA regA regA, pop, TE.Jump TE.CR (1+(length (whileStat++jumpBack)))]
                jumpBack    = [TE.Jump TE.UR (-(length(cond++jumpOut++whileStat)))]
            in cond ++ jumpOut ++ whileStat ++ jumpBack ++ cmpP nvar (st:sts) tr

-- hack for matching assignments it Then subtrees. 
-- Parser throws away parent Semicolons of Assignments in Then subtrees
-- which means they do not get matched by the above pattern.
-- there be dragons in the parser, so I thought this would save some time :)
{-}
cmpP nvar (st:sts) (TokenNode (Assignment, _) (TokenLeaf (Var, vname)) expr ) =
    let varaddr = (addr2int (getAddr vname (st:sts)) ) 
    in (cmpE (st:sts) expr) ++ [TE.Pop r4, TE.Store (TE.Addr r4) varaddr ]
        
-- same with Bopen, sometimes it pops up without a Semicolon parent.                            ||should always be Nop but ah well
cmpP nvar (st:sts) (TokenNode (BOpen, "{") subsl subsr )    =  (cmpP nvar ([]:st:sts) subsl) ++ cmpP nvar (st:sts) subsr
-}


cmpP nvar st (Nop)  = []

cmpP nvar st t      = trace ("not caught pattern:\n"++(show t)) []

compile::TokenTree->[TE.Assembly]
compile t	=(cmpP 0 [[]] t) ++ [TE.EndProg]

