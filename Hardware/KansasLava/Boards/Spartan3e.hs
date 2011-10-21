{-# LANGUAGE ScopedTypeVariables #-}

module Hardware.KansasLava.Boards.Spartan3e (
        -- * Class for the methods of the Spartan3e
          Spartan3e(..)
	-- * Initialization, and global settings.
	, clockRate
	, writeUCF
        -- * Data structures 
        , Serial(..)
        -- * Utilities for Board and Simulation use
        , switchesP
        , buttonsP
        , ledsP
	) where


import Language.KansasLava as KL
import Hardware.KansasLava.LCD.ST7066U
import Hardware.KansasLava.RS232
import Hardware.KansasLava.Rate
import Hardware.KansasLava.Boards.UCF
import qualified Hardware.KansasLava.VGA as VGA

import Data.Sized.Unsigned
import Data.Sized.Ix hiding (all)
import Data.Sized.Matrix hiding (all)
import Data.Char
import System.IO
import Control.Applicative
import Control.Monad.Fix

------------------------------------------------------------
-- The Spartan3e class
------------------------------------------------------------

class MonadFix fabric => Spartan3e fabric where
   ----------------------------------------------------------------------------

   -- | 'board_init' sets up the use of the clock.
   -- Always call 'board_init' first. [Required].
   board_init :: fabric ()

   -- | 'rot_as_reset' sets up the rotary dial as a reset switch.
   rot_as_reset :: fabric ()

   ----------------------------------------------------------------------------

   -- | 'mm_lcdP' gives a memory mappped (mm) API to the LCD.
   --  Disables the StrataFlash (for now).
   mm_lcdP :: FabricPatch fabric
                          (Seq (Enabled ((X2,X16),U8)))  ()
	                  (Seq Ack)	                 ()
   mm_lcdP = patchF mm_LCD_Inst |$| lcdP


{-
   test_idea :: EdgePatch
   fabric (Patch (Seq U8) (fabric ())
                              (Seq Ack) ())
 -}
 
   -- | 'lcdP' gives a patch-level API to the LCD, based on LCDInstructions.
   --  Disables the StrataFlash (for now).
   lcdP :: FabricPatch fabric
                       (Seq (Enabled LCDInstruction)) ()
	               (Seq Ack)	              ()

   -- | 'rs232_txP' gives a patch level API for transmission of bytes
   -- over one of the serial links.
   rs232_txP :: Serial  -- ^ port
             -> Integer -- ^ baud rate 
             -> FabricPatch fabric
                            (Seq (Enabled U8))    ()
	                    (Seq Ack)	          ()

   -- | 'rs232_rxP' gives a patch level API for reception of bytes
   -- over one of the serial links. Note there is no hand-shaking
   -- because the (implied) UART does no buffering; you better be
   -- ready.
   rs232_rxP :: Serial  -- ^ port
             -> Integer -- ^ baud rate
             -> fabric (Patch () (Seq (Enabled U8))
	                      () ())

   -- | 'debounceP' gives a small debounce correction.
   -- Use by on the (input) patches. The simulator 
   -- does no debouncing; the real hardware needs to.
   debounceP :: fabric (Patch (Seq Bool)   (Seq Bool)
	                      ()	   ())
   debounceP = return emptyP

   ----------------------------------------------------------------------------

   -- | 'tickTock' generates 'n' pulses per second, 
   -- based on the expected simulation, or clockrate on the board.
   -- The purpose is for controlling real-time sampling, or for animations.
   -- 
   tickTock :: (Size w) => Witness w -> Integer -> fabric (Seq Bool)

   ----------------------------------------------------------------------------
 
   -- | 'lcd' give raw access to the lcd bus. Disables the StrataFlash (for now).
   lcd :: Seq U1 -> Seq U4 -> Seq Bool -> fabric ()

   -- | 'switches' gives raw access to the position of the toggle switches.
   switches :: fabric (Matrix X4 (Seq Bool))

   -- | 'buttons' gives raw access to the state of the buttons.
   buttons :: fabric (Matrix X4 (Seq Bool))
  
   -- | 'leds' drives the leds
   leds :: Matrix X8 (Seq Bool) -> fabric ()

   -- | 'dial_button' gives raw access to the state of the dial button
   dial_button :: fabric (Seq Bool)

   -- | 'dial_rot' gives Enabled packets when dial is rotated,
   -- and if the rotation was clockwise
   dial_rot :: fabric (Seq (Enabled Bool))

   -- | 'mm_vgaP' gives a memory mapped API to the VGA port.
   -- Each charactor has an extra attribute
   mm_vgaP :: Patch (Seq (Enabled ((X40,X80),(VGA.Attr,U7)))) (fabric ())
                    (Seq Ack)	                              ()

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

  lcdP = patchF (init_LCD $$ phy_Inst_4bit_LCD) |$| buildF (\ (bus,_) -> do
                let (rs,sf_d,e) = unpack bus
                lcd rs sf_d e                
                return ((),()))

  rs232_rxP serial baud = do
           inp :: Seq Bool <- inStdLogic ("RS232_" ++ show serial ++ "_RX") 
           return (outputP inp $$ rs232in baud clockRate)

  ------------------------------------------------------------
  -- RAW APIs
  ------------------------------------------------------------

  lcd rs sf_d e = do 
		outStdLogic 	  "LCD_RS" rs
		outStdLogicVector "SF_D" (appendS (0 :: Seq (U8)) sf_d  :: Seq U12)
		outStdLogic       "LCD_E"  e
		outStdLogic       "LCD_RW" low
		outStdLogic       "SF_CE0" high


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

  dial_rot = error "dial_rot is not (yet) supported in the hardware"


  tickTock wit hz = do
           return (rate wit (1 / (fromIntegral clockRate / fromIntegral hz)))


-------------------------------------------------------------
-- data structures
-------------------------------------------------------------

data Serial = DCE | DTE deriving (Eq, Ord, Show)
        
-------------------------------------------------------------
-- Utilites that can be shared
-------------------------------------------------------------

-- | 'switchesP' gives a patch-level API for the toggle switches.
switchesP :: (Spartan3e fabric) =>
             fabric (Patch () (Matrix X4 (Seq Bool))
	                   () (Matrix X4 ()))
switchesP = do
	sws <- switches
        db <- debounceP
	return (outputP sws $$ 
	        backwardP (\ _mat -> ()) $$
                matrixStackP (pure db))


-- | 'buttonsP' gives a patch-level API for the toggle switches.
buttonsP :: (Spartan3e fabric) =>
             fabric (Patch () (Matrix X4 (Seq Bool))
	                   () (Matrix X4 ()))
buttonsP = do
	sws <- buttons
        db <- debounceP
        return (outputP sws $$ 
	        backwardP (\ _mat -> ()) $$
                matrixStackP (pure db))

-- | 'ledP' gives a patch-level API for the leds.
ledsP :: (Spartan3e fabric) =>
             Patch (Matrix X8 (Seq Bool)) (fabric ())
                   (Matrix X8 ())         ()
ledsP = 
        backwardP (\ () -> pure ()) $$
        forwardP leds


 