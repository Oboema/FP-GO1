{-# LANGUAGE RecordWildCards, ParallelListComp #-}

module Exec where

import Data.Char
import Data.List
import Debug.Trace

import TypesEtc
import Sprockell



-- ============================================================================================
-- execution functions for simulation purposes: exec, sim
--
-- addrs: register+memory addresses that you want to follow/inspect when running a program (instrs).
-- instrs: a list of assembly instructions (the program) that the Sprockell executes.
-- count: counts the number of instructions that are executeds. The total is shown at the end.
-- state: contains 4 things: program counter, stack pointer, registers, data memory.
-- i:is: list of inputs. In this case just a repeating clock tick.
--
-- The output of exec is generated every clock cycle by the function demoOutput,
--	after which exec continues in the next state (state') calculated by one cycle of the Sprockell processor.

exec addrs instrs (count, state@State{..}) (i:is)

	| instrs!!pc==EndProg	= traceShow ("Instructions: " ++ show count)
				  []

	| otherwise		=    demoOutput addrs instrs state'    :    exec addrs instrs (count+1, state') is

	where
	  state' = sprockell instrs state i




-- ============================================================================================
-- generating demoOutput
--
-- demoOutput calculates a value of type DemoOutput. The function show for this type is in TypesEtc.hs

demoOutput addrs instrs State{..}	 =  DemoOutput	pc
							(instrs!!pc)
							(map (regbank!!) regaddrs)
							(map (dmem!!) memaddrs)
							sp
							(map (dmem!!) [sp0+1..sp])

						where
						  (regaddrs,memaddrs) = addrs


-- sim: the simulation function which runs exec and outputs the result in a readable way.
-- --------------------------------------------------------------------------------------
sim addrs instrs = putStr . unlines . map show $ results
		where
		  results = demoOutput addrs instrs initstate : exec addrs instrs (0,initstate) clock



-- showInstructions: shows a list of instructions in a readable way.
-- -----------------------------------------------------------------
showInstrs instrs = putStr . unlines $ strs
		where
		  m    = length $ show $ length instrs + 1

		  strs = [ ' ' : replicate (m-w) ' ' ++ show n ++ ": " ++ show instr	|  (n,instr) <- zip [0..] instrs
											,  w	     <- [length $ show n]
											]





-- ============================================================================================
-- Examples
-- ============================================================================================

{---------------------------------------------
| Example 1: computes the value of 3^5 (= 243)
----------------------------------------------
Program in imperative pseudo-code:

a = 3;
n = 5;
power = 1;
while (n != 0) {
  power = a * power;
  n = n-1;
};

----------------------------------------------}

-- A list of assembly instruction that calculates example 1
-- --------------------------------------------------------
instrs1 = [ Load (Imm 3) 3		-- 0	value of a (=3) is put in register 3;
					--	Register 3 will be used for a.

	  , Load (Imm 5) 4		-- 1	value of n (=5) is put in register 4;
	  				--	Register 4 will contain the value of n throughout the execution.

	  , Load (Imm 1) 5		-- 2	initial value of power (=1) is put in register 5;
	  				--	Register 5 be used for the value of the power.

	  , Compute Equal 4 0 1		-- 3	Compute n==0 (reg 4 contains the value of n, reg 0 contains 0), and put the result in register 1;
	  				--	Register 1 is checked for conditional jumps.

	  , Jump CA 8			-- 4	If True (ie register 1 contains 1), then go to EndProg

	  , Compute Mul 3 5 5		-- 5	multiply a (reg 3) with power (reg 5), give the result to power

	  , Compute Decr 4 0 4		-- 6	Decrement n (reg 4) with 1

	  , Jump UA 3			-- 7	Go back to instruction 3

	  , EndProg			-- 8
	  ]


-- relevant addresses to show during simulation
-- --------------------------------------------
addrs1 = 	( [3,4,5]			-- registers
		, []				-- heap
		) :: ([Int],[Int])

-- show the list of instructions
-- -----------------------------
is1  = showInstrs instrs1

-- run the program instrs1, and show the content of the addresses addrs1
-- ---------------------------------------------------------------------
run1 = sim addrs1 instrs1


{---------------------------------------
| Example 2: compute the "3n+1" function
----------------------------------------
Program in imperative pseudo-code:

program threeNplus1;
var a;

function even (n);
              {return (n%2) == 0};

function three (n);
  { while n>1
      { if even(n)
          { n=n/2; }
          { n=(3*n)+1; };
      };
    return n
  };

{ a = three(7);
}

-}

-- Haskell definition (runnable):
-- -----------------------------------------------------
three :: Int -> [Int]
three n	| n == 1		= [1]
	| n `mod` 2 == 0	= n : three (n `div` 2)
	| otherwise		= n : three (3*n +1)


-- A list of assembly instruction that calculates example 2
-- --------------------------------------------------------
instrs2 = [ Load (Imm 1) 2		-- 0	Load the constant 1 in register 2
	  , Load (Imm 2) 3		-- 1	Load the constant 2 in register 3
	  , Load (Imm 3) 4		-- 2	Load the constant 3 in register 4

	  , Load (Imm 7) 5		-- 3	Load initial value of n (7) in register 5

	  , Compute Equal 5 2 1		-- 4	Compute n==1, and load result in register 1;
	  , Jump CA 13			-- 5	If reg1=1, then we're done, and thus go to EndProg

	  , Compute Mod 5 3 1		-- 6	Otherwise: calculate n`mod`2, and load the result in reg1.
	  , Jump CA 10			-- 7	If reg1=1 (i.e: if n is odd), then go to instruction 10

	  , Compute Div 5 3 5		-- 8	else divide n by 2 (the content of reg3) and put the result in register 5.
	  , Jump UA 4			-- 9	Jump back to instruction 4.

	  , Compute Mul 5 4 5		-- 10	At this point n is odd, thus multiply by 3 (the content of reg4)...
	  , Compute Add 5 2 5		-- 11	... and add 1 (the content of reg2).
	  , Jump UA 4			-- 12	Jump back to 4.

	  , EndProg			-- 13	End of Program.
	  ]


-- relevant addresses to show during simulation
-- --------------------------------------------
addrs2  =	( [1,5]			-- registers
		, []			-- heap
		) :: ([Int],[Int])


-- show the list of instructions
-- -----------------------------
is2  = showInstrs instrs2

-- run the program instrs2, and show the content of the addresses addrs2
-- ---------------------------------------------------------------------
run2 = sim addrs2 instrs2






