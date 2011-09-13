module Hardware.KansasLava.Boards.Spartan3e (
        -- * Class for the methods of the Spartan3e
          Spartan3e(..)
	-- * Initialization, and global settings.
	, clockRate
	, writeUCF
	) where


import Language.KansasLava as KL
import Hardware.KansasLava.LCD.ST7066U
import Hardware.KansasLava.Boards.UCF

import Data.Sized.Unsigned
import Data.Sized.Ix hiding (all)
import Data.Sized.Matrix hiding (all)
import Data.Char
import System.IO
 
------------------------------------------------------------
-- The Spartan3e class
------------------------------------------------------------

class Monad fabric => Spartan3e fabric where

   -- | 'board_init' sets up the use of the clock.
   -- Always call 'board_init' first. [Required].
   board_init :: fabric ()

   -- | 'rot_as_reset' sets up the rotary dial as a reset switch.
   rot_as_reset :: fabric ()

   -- | 'lcdPatch' gives a memory mappped (mm) API to the LCD.
   --  Disables the StrataFlash (for now).
   mm_lcdPatch :: Patch (Seq (Enabled ((X2,X16),U8)))  (fabric ())
	                (Seq Ack)	               ()

   -- | 'lcdPatch' gives a patch-level API to the LCD, based on LCDInstructions.
   --  Disables the StrataFlash (for now).
   lcdPatch :: Patch (Seq (Enabled LCDInstruction)) (fabric ())
	             (Seq Ack)	                    ()

   -- | 'switchesPatch' gives a patch-level API for the toggle switches.
   switchesPatch :: fabric (Patch () (Matrix X4 (Seq Bool))
		                  () (Matrix X4 ()))

   -- | 'lcd' give raw access to the lcd bus. Disables the StrataFlash (for now).
   lcd :: Seq U1 -> Seq U4 -> Seq Bool -> fabric ()

   -- | 'switches' gives raw access to the position of the toggle switches.
   switches :: fabric (Matrix X4 (Seq Bool))

   -- | 'buttons' gives raw access to the state of the buttons.
   buttons :: fabric (Matrix X4 (Seq Bool))
  
   -- | 'leds' drives the leds
   leds :: Matrix X8 (Seq Bool) -> fabric ()

   -- 'dial_button' gives raw access to the state of the dial button
   dial_button :: fabric (Seq Bool)

   -- 'dial_rot' gives Enabled packets when dial is rotated,
   -- and if the rotation was clockwise
   dial_rot :: fabric (Seq (Enabled Bool))

------------------------------------------------------------
-- initialization
------------------------------------------------------------

-- | The clock rate on the Spartan3e (50MHz), in hertz.
clockRate :: Integer
clockRate = 50 * 1000 * 1000

-- | show out a suggested UCF file for Spartan3e, for a specific circuit.
writeUCF :: FilePath -> KLEG -> IO ()
writeUCF = copyUCF "Spartan3e.ucf"

------------------------------------------------------------
-- instance
------------------------------------------------------------

instance Spartan3e Fabric where
  board_init = do
	-- we need to name and pull in the clock
	theClk "CLK_50MHZ"

  rot_as_reset = theRst "ROT_CENTER"

------------------------------------------------------------
-- Patches
------------------------------------------------------------

  mm_lcdPatch = mm_LCD_Inst $$ lcdPatch

  lcdPatch = 
	init_LCD $$ 
	phy_Inst_4bit_LCD $$ 
	forwardPatch (\ bus -> do 
		let (rs,sf_d,e) = unpack bus
		lcd rs sf_d e)

  switchesPatch = do
	sws <- switches
	return (unitPatch sws $$ backwardPatch (\ _mat -> ()))

  ------------------------------------------------------------
  -- RAW APIs
  ------------------------------------------------------------

  lcd rs sf_d e = do 
		outStdLogic 	  "LCD_RS" rs
		outStdLogicVector "SF_D" (KL.append (0 :: Seq (U8)) sf_d  :: Seq U12)
		outStdLogic       "LCD_E"  e
		outStdLogic "LCD_RW" low
		outStdLogic "SF_CE0" high


  switches = do
        inp <- inStdLogicVector "SW" :: Fabric (Seq (Matrix X4 Bool))
        return (unpack inp)


  buttons = do
        i0 <- inStdLogic "BTN_WEST"
        i1 <- inStdLogic "BTN_NORTH"
        i2 <- inStdLogic "BTN_EAST"
        i3 <- inStdLogic "BTN_SOUTH"
        return (matrix [i0,i1,i2,i3])

  leds inp = outStdLogicVector "LED" (pack inp :: Seq (Matrix X8 Bool))

  dial_button = 
        inStdLogic "ROT_CENTER"



 