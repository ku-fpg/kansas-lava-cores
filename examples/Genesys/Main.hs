{-# LANGUAGE TypeFamilies #-}
module Main where

import Language.KansasLava as KL
import Hardware.KansasLava.RS232
import Hardware.KansasLava.FIFO
import Hardware.KansasLava.Spartan3e.LCD
import Hardware.KansasLava.Rate


import Data.Sized.Unsigned
import Data.Sized.Matrix
import Data.Default
import System.CPUTime
import Data.Char as C
import Data.Bits


type X512 = ADD X256 X256
type X1024 = ADD X512 X512
type X16K   = MUL X1024 X16


circuit :: (sig ~ Signal c, Clock c, c ~ ())
	=> Patch () 	(sig (U1,U4,Bool))
	         ()	()
circuit = pulse $$ prependP msg $$ lcdDriver
  where
	msg :: Matrix X38 U9
	msg = matrix
	    [ 0x80	-- set write address to start
	    , 0x06	-- cursor, no blinking

	    , 0x40	-- set CG addr to 0
	    , 0x10a	--
	    , 0x41	-- set CG addr to 0
	    , 0x115	--
	    , 0x42	-- set CG addr to 0
	    , 0x10a	--
	    , 0x43	-- set CG addr to 0
	    , 0x115	--
	    , 0x44	-- set CG addr to 0
	    , 0x10a	--
	    , 0x45	-- set CG addr to 0
	    , 0x115	--
	    , 0x46	-- set CG addr to 0
	    , 0x10a	--
	    , 0x47	-- set CG addr to 0
	    , 0x100	--

	    , 0x80	-- write into DD ram

	    , 0x100	-- write the special char

	    , 0x40	-- set CG addr to 0
	    , 0x10f	--
	    , 0x41	-- set CG addr to 0
	    , 0x115	--
	    , 0x42	-- set CG addr to 0
	    , 0x10f	--
	    , 0x43	-- set CG addr to 0
	    , 0x115	--
	    , 0x44	-- set CG addr to 0
	    , 0x10f	--
	    , 0x45	-- set CG addr to 0
	    , 0x115	--
	    , 0x46	-- set CG addr to 0
	    , 0x10a	--
	    , 0x47	-- set CG addr to 0
	    , 0x100	--

	    , 0x82	-- write into DD ram

	    , 0x100	-- write the special char


--	    , 0x07	-- move display
--	    , 0x18	-- shift to the *LEFT*
	    ]

pulse :: (sig ~ Signal c, Clock c)
      => Patch () (sig (Enabled U9))
	       () (sig Ack)
pulse = openP $$
	(top `stackP` bottom) $$
	zipP $$
	mapP (\ ab -> snd (unpack ab))
   where
	top = outputP (packEnabled (powerOfTwoRate (Witness :: Witness X25)) (pureS ())) $$
	      enabledToAckBox
	bottom = cycleP (matrix (map ((+ 0x100) . fromIntegral . C.ord) msg) :: Matrix X19 U9)

	msg = "Kansas Lava rocks! "


diff :: (Eq a) => [a] -> [(Integer,a)]
diff (x:xs) = f 0 x xs
  where
	f n x (x':xs)
	  | x /= x' = (n,x') : f (n+1) x' xs
	  | otherwise = f (n+1) x xs
	f _ _ [] = []

main = do
	let (_,res) = execP circuit ((),())

	let (rs,sf_d,e) = unpack res


	let fabric = do
		theClk "CLK_50MHZ"
		theRst "ROT_CENTER"

		outStdLogic "LCD_RS" rs
		outStdLogicVector "SF_D" (appendS (0 :: Seq (U8)) sf_d  :: Seq U12)
		outStdLogic "LCD_E"  e

		outStdLogic "LCD_RW" low
		outStdLogic "SF_CE0" high
		return ()

	kleg <- reifyFabric fabric

	kleg' <- optimizeCircuit def kleg
	writeVhdlCircuit "main" "main.vhd" kleg


-- Notes for the genesys (note that the Genesys has an 8-bit data path).
-- In addition to data pins DB7 downto DB0 we have:
--  * RS: register select (high for data, low for instructions)
--  * R/W: high for read, low for write
--  * E: enable, high for OE; falling edge writes data (?)
--  * DB7 .. DB0 Data



data LCDInstruction = ClearDisplay
                    | ReturnHome
                    | EntryMode { moveRight :: Bool, displayShift :: Bool }
                    | SetDisplay { displayOn :: Bool, cursorOn :: Bool, blinkingCursor :: Bool }
                    | SetShift { displayShift :: Bool, rightShift :: Bool }
                    | FunctionSet { eightBit :: Bool, twoLines :: Bool, notFiveByEight :: Bool }
                    | SetCGAddr { addr :: Int }
                    | SetDDAddr { addr :: Int }
                    | ReadBusyAddr
                    | ReadRam
                    | WriteChar { chr :: Char }
-- Some instruction encodings
-- Clear Display: 0x01
-- Return home: 0x02
-- Entry mode select:
--    DB2 high
--    DB1 high for right-moving cursor, DB1 low for left-moving cursor
--    DB0 high for display shift, DB0 low for no shift
-- Set display:
--   DB3 high
--   DB2 high for display on
--   DB1 high for cursor on
--   DB0 high for blinking cursor on
-- Cursor or display shift
--   DB4 high
--   DB3 low for cursor shift, DB3 high for display shift
--   DB2 high for right shift, low for left shift
-- Function Set
--   DB5 high
--   DB4 high for 8 bit data length
--   DB3 high for 2 lines, DB3 low for 1 line
--   DB2 low for 5x8 dots
-- Set CGRAM address
--   DB6 high
--   DB6..DB0 for actual address
-- Set DDRam address
--   DB7 high
--   DB6..DB0 for actual address
--  Read busy flag address (input)
--   R/W high
--   DB7 is busy flag
--   DB6.DB0 is the address
--  Write data to RAM
--  RS high
--  R/W low
--  DB7..DB0 is data
--  Read data from RAM (input)
--  RS high
--  R/W high
--  DB7..DB0 is data


toInt :: LCDInstruction -> Int
toInt ClearDisplay = 0x1
toInt ReturnHome = 0x2
toInt (EntryMode mr ds) = withBit mr 1 $ withBit ds 0 0x4
toInt (SetDisplay dIn cOn blink) = withBit dIn 2 $ withBit cOn 1 $ withBit blink 0 $ 0x8
toInt (SetShift ds rs) = withBit ds 3 $ withBit rs 2 $ 0xF
toInt (FunctionSet eb tl fx8) = withBit eb 4 $ withBit tl 3 $ withBit fx8 2 $ 0x10
toInt (SetCGAddr addr) = 0x20 .|. addr
toInt (SetDDAddr addr) = 0x40 .|. addr
toInt (WriteChar chr) = setBit (ord chr) 9

toInt ReadBusyAddr = setBit 0 0x80
toInt ReadRam = 0x180




withBit True idx d = setBit d idx
withBit False idx d = clearBit d idx


instance Rep LCDInstruction where
  type W LCDInstruction = X10
  data X LCDInstruction = XLCDInstruction (Maybe LCDInstruction)

  unX (XLCDInstruction i) = i
  optX i = XLCDInstruction i
  toRep (XLCDInstruction Nothing) = RepValue (replicate (size (undefined :: W LCDInstruction)) Nothing)
  toRep (XLCDInstruction (Just ClearDisplay)) = RepValue (map Just $ replicate 9 False ++ [True]) -- Low bit set




bootSequence = [FunctionSet { eightBit = True, twoLines = True, notFiveByEight = False}
               ,SetDisplay { displayOn = True, cursorOn = False, blinkingCursor = False}
               ,ClearDisplay
               ,EntryMode { moveRight = True, displayShift = True }
               ]


bootRom :: Matrix X4 U9
bootRom = matrix $ map (fromIntegral . toInt) bootSequence