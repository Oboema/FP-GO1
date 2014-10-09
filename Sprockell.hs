{-# LANGUAGE RecordWildCards #-}

module Sprockell where

import Debug.Trace
import TypesEtc
import Prelude

{-------------------------------------------------------------
| SPROCKELL: Simple PROCessor in hasKELL :-)
|
| j.kuper@utwente.nl
| October 28, 2012
-------------------------------------------------------------}



{-------------------------------------------------------------
| some constants
-------------------------------------------------------------}

dmemsize    = 128 :: Int	-- NOTE: memory sizes as yet unused, no "out of memory" yet
regbanksize =   7 :: Int


zeroreg	=  0 :: Int		-- the register that will always contain the value 0
regA    =  1 :: Int		-- for computational purposes;
				-- used in decode and pcUpd for Conditional Jumps.
regB    =  2 :: Int		-- 
jmpreg  =  3 :: Int             -- for jump instructions;
				-- used in decode and pcUpd, to remember instruction to jump back to from subroutine.
pcreg	=  regbanksize		-- pc is added at the end of the regbank => regbank0 (see sprockell)
				-- 	thus: index of program counter is the value of regbanksize

sp0	= 20 :: Int		-- TODO: get sp0 from compiler, add OS

{-------------------------------------------------------------
| some basic functions, used below
-------------------------------------------------------------}

incr 	=   (+1)
decr 	= (+(-1))

tobit True  = 1
tobit False = 0


{-----------------------------------------------------------------------------------------
| initstate: initial values for register bank, data memory, program counter, stack pointer
-----------------------------------------------------------------------------------------}
initstate	= State	   { regbank	= replicate regbanksize 0
			   , dmem	= replicate dmemsize 0
			   , pc		= 0
			   , sp		= sp0
			   }

{-----------------------------------------------------------------------------------------
| nullcode: default result of the decode founction.
-----------------------------------------------------------------------------------------}
nullcode	= MachCode { ldCode	= NoLoad
			   , stCode	= NoStore
			   , jmpCode	= NoJump
			   , spCode	= None
			   , opCode	= NoOp
			   , immvalueR	= 0
			   , immvalueS	= 0
			   , fromreg0	= 0
			   , fromreg1	= 0
			   , fromaddr	= 0
			   , toreg	= 0
			   , toaddr	= 0
			   , jumpN	= 0
			   }


{-------------------------------------------------------------
| The actual Sprockell
-------------------------------------------------------------}

-- -----------------------------------------------------------------------------------------------------------------
-- decode: transforms an assembly instruction into machine code by updating the default code on the relevant fields.
--		Also needs the stack pointer sp.

decode :: Int -> Assembly -> MachCode

decode sp instr  = case instr of

       		Compute c i0 i1 i2	->  nullcode {ldCode =LdAlu,  opCode=c, fromreg0=i0, fromreg1=i1, toreg=i2}

		Jump jc n		->  nullcode {jmpCode=jc,     fromreg0=regA, fromreg1 =jmpreg, jumpN=n}

   		Load  (Imm  n) j	->  nullcode {ldCode =LdImm,  immvalueR=n,  toreg=j}
   		Load  (Addr i) j	->  nullcode {ldCode =LdAddr, fromaddr =i,  toreg=j}

       		Store (Imm  n) j	->  nullcode {stCode =StImm,  immvalueS=n,  toaddr=j}
       		Store (Addr i) j	->  nullcode {stCode =StReg,  fromreg0 =i,  toaddr=j}

		Push r			->  nullcode {stCode =StReg,  fromreg0 =r,  toaddr=incr sp, spCode=Up}
		Pop r			->  nullcode {ldCode =LdAddr, fromaddr =sp, toreg =r,       spCode=Down}

		EndProg			->  nullcode


-- -----------------------------------------------
-- alu: performs a calculation based on an OpCode.

alu :: OpCode -> Int -> Int -> Int

alu opCode x y = case opCode of
			Id	-> x
			Incr	-> incr x
			Decr	-> decr x
			Neg	-> -x
			Add	-> x + y
			Sub	-> x - y
			Mul	-> x * y
			Div	-> x `div` y
			Mod	-> x `mod` y
			Equal	-> tobit (x == y)
			NEq	-> tobit (x /= y)
			Gt	-> tobit (x > y)
			Lt	-> tobit (x < y)
			And	-> x * y
			Or	-> x `max` y
			Not	-> 1 - x
			NoOp	-> 0

-- ----------------------------------------------------
-- load: puts a value in the register bank.
--	Note: the value in register 0 will always be 0.
load :: [Int] -> LdCode -> Int -> (Int,Int,Int) -> [Int]

load regbank ldCode toreg (immvalueR,mval,z) = regbank'
		where
		  v =  case ldCode of
			NoLoad	-> 0
			LdImm	-> immvalueR
			LdAddr	-> mval
			LdAlu	-> z

		  regbank'	| toreg==0	= regbank
		  		| otherwise	= regbank <~ (toreg,v)


-- -----------------------------------
-- store: puts a value in data memory.
store :: [Int] -> StCode -> Int -> (Int,Int) -> [Int]

store dmem stCode toaddr (immvalueS,x) = dmem'
		where
		  dmem' =  case stCode of
				NoStore	-> dmem
				StImm	-> dmem <~ (toaddr,immvalueS)
				StReg	-> dmem <~ (toaddr,x)


-- -----------------------------------
-- pcUpd: updates the program counter.
pcUpd :: (JmpCode, Int) -> (Int,Int,Int) -> Int

pcUpd (jmpCode,x) (pc,jumpN,y)
		=  case jmpCode of
			NoJump	-> incr pc
			UA	-> jumpN
			UR	-> pc+jumpN
			CA	-> if x==1 then jumpN    else incr pc
			CR	-> if x==1 then pc+jumpN else incr pc
			Back	-> y



-- ---------------------------------
-- spUpd: updates the stack pointer.
spUpd :: SPCode -> Int -> Int

spUpd spCode sp	= case spCode of
			Up	-> incr sp
			Down	-> decr sp
			None	-> sp




-- ======================================================================================
-- sprockell: puts it all together

sprockell :: [Assembly] -> State -> Tick -> State

sprockell  instrs  State{..}  _  =  State {dmem=dmem',regbank=regbank',pc=pc',sp=sp'}
		where
		  MachCode{..}	=  decode sp (instrs!!pc)
		  regbank0	=  regbank++[pc]

		  (x,y)		=  (regbank0!!fromreg0 , regbank0!!fromreg1)
		  mval	 	=  dmem!!fromaddr

		  z		=  alu    opCode x y

		  regbank'	=  load   regbank ldCode toreg  (immvalueR,mval,z)
		  dmem'		=  store  dmem	  stCode toaddr (immvalueS,x)

		  pc'     	=  pcUpd  (jmpCode,x)  (pc,jumpN,y)
		  sp'		=  spUpd  spCode	 sp


