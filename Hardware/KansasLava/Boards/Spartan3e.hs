module Hardware.KansasLava.Boards.Spartan3e where

import Language.KansasLava as KL
import Hardware.KansasLava.LCD.ST7066U
import Data.Sized.Unsigned
import Data.Sized.Ix
import Data.Sized.Matrix

-- 
-- | 'lcdPatch' gives a patch-level API to the LCD, based on LCDInstructions.
--  Disables the StrataFlash (for now).
lcdPatch :: Patch (Seq (Enabled LCDInstruction)) (Fabric ())
	     	  (Seq Ack)	                    ()
lcdPatch = 
	init_LCD $$ 
	phy_Inst_4bit_LCD $$ 
	forwardPatch (\ bus -> do 
		let (rs,sf_d,e) = unpack bus
		lcd rs sf_d e)

-- | 'lcd' give raw access to the lcd bus. Disables the StrataFlash (for now).

lcd :: Seq U1 -> Seq U4 -> Seq Bool -> Fabric ()
lcd rs sf_d e = do 
		outStdLogic 	  "LCD_RS" rs
		outStdLogicVector "SF_D" (KL.append (0 :: Seq (U8)) sf_d  :: Seq U12)
		outStdLogic       "LCD_E"  e
		outStdLogic "LCD_RW" low
		outStdLogic "SF_CE0" high

-- | 'switchesPatch' gives a patch-level API for the toggle switches.

switchesPatch :: Fabric (Patch () (Matrix X4 (Seq Bool))
			       () (Matrix X4 ()))
switchesPatch = do
	sws <- switches
	return (unitPatch sws $$ backwardPatch (\ _mat -> ()))

-- | 'switches' gives raw access to the position of the toggle switches.
switches :: Fabric (Matrix X4 (Seq Bool))
switches = undefined

-- | 'board_init' sets up the use of the clock. Required.
board_init :: Fabric ()
board_init = do
	theClk "CLK_50MHZ"		-- we need to name and pull in the clock

-- | 'rot_as_reset' sets up the rotary dial as a reset switch.
rot_as_reset :: Fabric ()
rot_as_reset = theRst "ROT_CENTER"
