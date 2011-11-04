{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TypeOperators, OverloadedStrings, TemplateHaskell #-}
module Hardware.KansasLava.LCD.ST7066U
	( phy_Inst_4bit_LCD
	, init_LCD
	, mm_LCD_Inst
	-- * Instruction Set
	, LCDInstruction(..)
	, setDDAddr
	, writeChar
	-- * For testing only
	, phy_4bit_LCD
	) where

import Language.KansasLava as KL
import Data.Sized.Unsigned
import Data.Sized.Ix
import Data.Sized.Matrix as M
import Control.Applicative
import Data.Char
import qualified Data.Bits as B

import Hardware.KansasLava.Text as F

----------------------------------------------------------------------
-- Example usage
----------------------------------------------------------------------

-- example_lcd_driver = init_LCD $$ phy_Inst_4bit_LCD

-- The Sitronix ST7066U is compatible with Samsung X60069X, Samsung KS0066U,
-- Hitachi HD44780, and SMOS SED1278.

----------------------------------------------------------------------
-- Controller datastructure& bit formats 

----------------------------------------------------------------------
data LCDInstruction 
	= ClearDisplay
	| ReturnHome
	| EntryMode { moveRight :: Bool, displayShift :: Bool }
	| SetDisplay { displayOn :: Bool, cursorOn :: Bool, blinkingCursor :: Bool }
	| SetShift { displayShift :: Bool, rightShift :: Bool }
	| FunctionSet { eightBit :: Bool, twoLines :: Bool, fiveByEleven :: Bool }
	| SetCGAddr { cg_addr :: U6 }
	| SetDDAddr { dd_addr :: U7 }
	| ReadBusyAddr
	| ReadRam
	| WriteChar { char :: U8 }	
   deriving (Eq, Ord, Show)

$(repBitRep ''LCDInstruction 9)

setDDAddr :: Signal comb U7 -> Signal comb LCDInstruction 
setDDAddr = funMap (return . SetDDAddr)

writeChar :: Signal comb U8 -> Signal comb LCDInstruction 
writeChar = funMap (return . WriteChar)

-- 9-bit version; am okay with making it 10-bit
instance BitRep LCDInstruction where
	-- TODO: complete
    bitRep =
	--					LCD_RS & DB(7 downto 0)
	[ (ClearDisplay, 			"00000001") ] ++ 
	[ (ReturnHome, 				"0000001X") ] ++
	[ (EntryMode (bool a) 
		     (bool b),			"000001" & a & b) 
		| a <- every
		, b <- every
	] ++
	[ (SetDisplay (bool a) 
		      (bool b)
		      (bool c),			"00001" & a & b & c)
		| a <- every
		, b <- every
		, c <- every
	] ++ 
	[ (FunctionSet (bool a) 
		       (bool b)
		       (bool c),		"0010" & a & b & c & ("XX" :: BitPat X2))
		| a <- every
		, b <- every
		, c <- every
	] ++ 
	[ (SetCGAddr (fromIntegral addr), 	"001" & addr)
		| addr <- every :: [BitPat X6]
	] ++ -- 
	[ (SetDDAddr (fromIntegral addr), 	"01" & addr)
		| addr <- every :: [BitPat X7]
	] ++ -- 
	[ (WriteChar (fromIntegral c), 		"1" & c)
		| c <- every :: [BitPat X8]
	]

----------------------------------------------------------------------
-- Low level 4-bit physical driver
----------------------------------------------------------------------

-- The physical driver for the LCD patch
--  input: RS+nibble (5bits) and pause length in cycles
-- output: RS, SF_D[11:8], LCD_E
-- assuming LCD_RW is set always low
-- assuming 50Mhz clock

phy_4bit_LCD :: forall c sig . (Clock c, sig ~ Signal c)
	=> Patch (sig (Enabled (U5,U18)))	(sig (U1,U4,Bool))
		 (sig Ack)			()
phy_4bit_LCD ~(inp,_) = (toAck inAck,out)
   where

	(inAck,out) = runRTL $ do
		state   <- newReg (5 :: X6)
		pause   <- newReg (0 :: U18)
		counter <- newReg (0 :: U20)
		ack     <- newReg False
		rs      <- newReg (0 :: U1)
		sf_d    <- newReg (0 :: U4)
		lcd_e   <- newReg False 

		let wait = waitFor counter
		
		let firstWait = 750000


		CASE [ IF (reg state .==. 0 .&&. isEnabled inp) $ do
			-- waiting for input
			ack := pureS True
			let (cmd' :: sig U5,pause' :: sig U18) = unpack (enabledVal inp)
			let (sf_d':: sig U4,rs' :: sig U1) = unappendS cmd'
			pause := pause'
			rs    := rs'
			sf_d  := sf_d'
			state := 1
		     , IF (reg state .==. 1) $ do
			wait 2 $ state := 2
		     , IF (reg state .==. 2) $ do
		 	lcd_e := commentS "lcd_e := high" high
			wait 12 $ state := 3
		     , IF (reg state .==. 3) $ do
		 	lcd_e := commentS "lcd_e := low" low
			state := 4
			wait 1 $ state := 4
		     , IF (reg state .==. 4) $ do
			wait ((unsigned) (reg pause)) $ state := 0
		     , IF (reg state .==. 5) $ do
			wait firstWait $ state := 0
		     ]

		-- Ack for one cycle only
		CASE [ IF (reg ack .==. high) $ do
			ack  := pureS False
		     ]

--		DEBUG "state" state
{-
			  wait 750000 $ state := 1
		     , IF (reg state .==. 1) $ do
			  output := pureS (Just 
		     ]
-}
		return (commentS "ack" (var ack),pack (reg rs,reg sf_d,commentS "lcd_e" $ reg lcd_e))

waitFor :: (Rep b, Num b) => Reg s c b -> Signal c b -> RTL s c () -> RTL s c ()
waitFor counter count nextOp = do
	CASE [ IF (reg counter ./=. count) $ do
			counter := reg counter + 1
             , OTHERWISE $ do
			counter := 0
			nextOp
	     ]

----------------------------------------------------------------------
-- Instruction-based driver(s)
----------------------------------------------------------------------

-- | 'phy_4bit_Inst' gives a instruction-level interface, in terms of the 4-bit interface.
phy_Inst_4bit_LCD :: forall c sig . (Clock c, sig ~ Signal c)
	=> Patch (sig (Enabled LCDInstruction))	(sig (U1,U4,Bool))
		 (sig Ack)			()
phy_Inst_4bit_LCD = toCmds $$ prependP bootCmds $$ phy_4bit_LCD
   where
	toCmds = mapP splitCmd $$ matrixToElementsP

	bootCmds :: Matrix X4 (U5,U18)
	bootCmds = matrix 
		[ (0x3, 205000)
		, (0x3, 5000)
		, (0x3, 2000)
		, (0x2, 2000)
		] 

splitCmd :: forall comb . Signal comb LCDInstruction -> Signal comb (Matrix X2 (U5,U18))
splitCmd cmd = pack $ matrix 
	[ pack ( high_op `appendS` mode
	       , smallGap
	       )
	, pack ( low_op `appendS` mode
	       , mux ((bitwise) cmd .<=. (0x03 :: Signal comb U9)) (bigGap,hugeGap)
	       )
	]
    where
	(op :: Signal comb U8, mode :: Signal comb U1) = unappendS ((bitwise) cmd :: Signal comb U9)
	(low_op :: Signal comb U4, high_op :: Signal comb U4) = unappendS op

	smallGap = 50		-- between nibbles
	bigGap   = 2000		-- between commands
	hugeGap	 = 100000	-- after clear display or return cursor home

----------------------------------------------------------------------
-- initialization instructions
----------------------------------------------------------------------

init_LCD :: forall c sig . (Clock c, sig ~ Signal c)
	=> Patch (sig (Enabled LCDInstruction))	(sig (Enabled LCDInstruction))
		 (sig Ack)			(sig Ack)
init_LCD = prependP initCmds
   where
	initCmds :: Matrix X4 LCDInstruction
	initCmds = matrix [ FunctionSet { eightBit = False, twoLines = True, fiveByEleven = False }
			  , EntryMode { moveRight = True, displayShift = False }
			  , SetDisplay { displayOn = True, cursorOn = False, blinkingCursor = False }
			  , ClearDisplay
	 		  ]

----------------------------------------------------------------------
-- Memory Mapped version
----------------------------------------------------------------------

mm_LCD_Inst :: forall c sig . (Clock c, sig ~ Signal c)
	=> Patch (sig (Enabled ((X2,X16),U8)))	(sig (Enabled LCDInstruction))
		 (sig Ack)			(sig Ack)

mm_LCD_Inst = mapP toInsts $$ matrixToElementsP
  where
	toInsts :: forall comb . Signal comb ((X2,X16),U8) -> Signal comb (Matrix X2 LCDInstruction)
	toInsts wr = pack (matrix [ setDDAddr dd_addr, writeChar ch ] :: Matrix X2 (Signal comb LCDInstruction))
	    where
		(addr,ch) = unpack wr
		(row,col) = unpack addr

		dd_addr :: Signal comb U7
		dd_addr = mux (row .==. 0) (0x40 + (unsigned)col,0x00 + (unsigned)col)

