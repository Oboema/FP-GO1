module TypesEtc where

import Data.List
import Debug.Trace

-- ==========================================================================================================
-- Sprockell

data Value      = Addr Int
                | Imm Int			deriving (Eq,Show)

data LdCode     = NoLoad
                | LdImm
                | LdAddr
                | LdAlu				deriving (Eq,Show)

data StCode     = NoStore
                | StImm
                | StReg				deriving (Eq,Show)

data SPCode     = None
                | Up
                | Down				deriving (Eq,Show)

data JmpCode    = NoJump			-- No jump
                | UA				-- UnConditional - Absolute
                | UR				-- UnConditional - Relative
                | CA				-- Conditional   - Absolute
                | CR				-- Conditional   - Relative
                | Back				-- Back from subroutine
						deriving (Eq,Show)

data MachCode   = MachCode { ldCode     :: LdCode       -- codes which value to load to the register bank
			   , stCode     :: StCode       -- codes which value to store in data memory
			   , jmpCode    :: JmpCode      -- code how to update the program counter
			   , spCode     :: SPCode	-- codes how to update the stack pointer
			   , opCode     :: OpCode       -- opCode for the alu
			   , immvalueR  :: Int          -- immediate value to regbank
			   , immvalueS  :: Int          -- immediate value to store
			   , fromreg0   :: Int          -- first address to get value from registerbank
			   , fromreg1   :: Int          -- second address to get value from registerbank
			   , fromaddr   :: Int          -- address to get value from dmem
			   , toreg      :: Int          -- address to put value in regbank
			   , toaddr     :: Int          -- address to put value in dmem
			   , jumpN      :: Int          -- which instruction to jump to
			   }
			deriving (Eq,Show)


data OpCode	= NoOp | Id  | Incr | Decr						-- no corresponding functions in prog.language
		| Neg  | Not								-- unary operations
		| Add  | Sub | Mul  | Div | Mod | Equal | NEq | Gt | Lt | And | Or	-- binary operations
		deriving (Eq,Show)


data Assembly	= Compute OpCode Int Int Int	-- Compute opCode r0 r1 r2: go to "alu",
						--      do "opCode" on regs r0, r1, and put result in reg r2
		| Jump JmpCode Int		-- JumpAbs n: set program counter to n
		| Load Value Int		-- Load (Addr a) r : from "memory a" to "regbank r"
						-- Load (Imm  v) r : put "Int v" in "regbank r"
		| Store Value Int		-- Store (Addr r) a: from "regbank r" to "memory a"
						-- Store (Imm  v) r: put "Int v" in "memory r"
		| Push Int			-- push a value on the stack
		| Pop Int			-- pop a value from the stack

		| EndProg			-- end of program, handled bij exec function
		deriving (Eq,Show)


data  State	= State	  { regbank	:: [Int]        -- register bank
			  , dmem	:: [Int]        -- main memory, data memory
			  , pc		:: Int		-- program counter
			  , sp		:: Int		-- stack pointer
			  }
			deriving (Eq,Show)


-- ==========================================================================================================
-- Clock for simulation purposes

data Tick = Tick			deriving (Eq,Show)

clock = Tick : clock




-- ==============================================
-- ouput type for demonstration of execution


data DemoOutput	=  DemoOutput	Int				-- program counter
				Assembly			-- corresponding instruction
				[Int]				-- selected registers
				[Int]				-- selected dmem cells
				Int				-- stack pointer
				[Int]				-- the stack


instance Show DemoOutput where

	show (DemoOutput pc instr regs dmems sp stack)

		= (rjustify 3 $ show pc) ++ ": " ++

		  (ljustify 24 $ filter (/= '\"') $ show instr) ++ "  || " ++

		  (concatWith ',' $ map (rjustify 4) $ map show regs) ++ "  || " ++

		  (concatWith ',' $ map (rjustify 4) $ map show dmems) ++  "  ||  " ++

		  (rjustify 3 $ show sp) ++ ": " ++

		  (concatWith ',' $ map show stack)

		where

		  myshow (Just x) = show x
		  myshow Nothing  = "-"



-- ==========================================================================================================
-- some elementary functions


concatWith x []		= []				-- concats lists together and puts x in between
concatWith x [ys]	= ys
concatWith x (ys:yss)	= ys ++ [x] ++ concatWith x yss

ljustify n x = x ++ replicate (n - length x) ' '	-- left justify: to outline textual output
rjustify n x = replicate (n - length x) ' ' ++ x	-- right justify: ibid

xs <~ (i,x) = take i xs ++ [x] ++ drop (i+1) xs         -- puts value x on position i in list xs


