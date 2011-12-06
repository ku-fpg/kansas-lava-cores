{-# LANGUAGE ScopedTypeVariables,TypeFamilies, FlexibleContexts #-}

module Hardware.KansasLava.Boards.Spartan3e (
        -- * Class for the methods of the Spartan3e
          Spartan3e(..)
	-- * Initialization, and global settings.
	, clockRate
	, writeUCF
        -- * Data structures 
        , Serial(..)
        -- * Utilities for Board and Simulation use
--        , switchesP
--        , buttonsP
--        , ledsP
        , LEDs(..)
        , Switches(..)
        , DialRotation(..)
	) where


import Language.KansasLava as KL
import Hardware.KansasLava.LCD.ST7066U
import Hardware.KansasLava.RS232
import Hardware.KansasLava.Rate
import Hardware.KansasLava.Boards.UCF
import Hardware.KansasLava.Peripherals
import Hardware.KansasLava.Core

import Data.Sized.Unsigned
import Data.Sized.Ix hiding (all)
import Data.Sized.Matrix hiding (all)
import Data.Char
import System.IO
import Control.Applicative
import Control.Monad.Fix

--class MonadFix fabric => LEDs fabric where
--        leds :: fabric (Matrix X8 (REG Bool))

-- This abstraction is at the STMT level.
class (CoreMonad fab) => DialRotation fab where
        -- | 'dialRotation' gives Enabled packets when dial is rotated,
        -- and is Enabled True if the rotation was clockwise
        dialRotation :: fab (EXPR (Enabled Bool))
        dialButton   :: fab (EXPR Bool)

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

   leds8        :: fabric (Matrix X8 (REG Bool))
   switches4    :: fabric (Matrix X4 (EXPR Bool))

   ----------------------------------------------------------------------------

   -- | 'mm_lcdP' gives a memory mappped (mm) API to the LCD.
   --  Disables the StrataFlash (for now).
   mm_lcdP :: FabricPatch fabric
                          (Seq (Enabled ((X2,X16),U8)))  ()
	                  (Seq Ack)	                 ()

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
             -> FabricPatch fabric
                            () (Seq (Enabled U8))
	                    () ()

   ----------------------------------------------------------------------------

   -- | 'tickTock' generates 'n' pulses per second, 
   -- based on the expected simulation, or clockrate on the board.
   -- The purpose is for controlling real-time sampling, or for animations.
   -- 
   tickTock :: (Size w) => Witness w -> Integer -> fabric (Seq Bool)

   ----------------------------------------------------------------------------
 
--   -- | 'lcd' give raw access to the lcd bus. Disables the StrataFlash (for now).
--   lcd :: Seq U1 -> Seq U4 -> Seq Bool -> fabric ()

   -- | 'switches' gives raw access to the position of the toggle switches.
--   switches :: fabric (Matrix X4 (Seq Bool))

   -- | 'buttons' gives raw access to the state of the buttons.
--   buttons :: fabric (Matrix X4 (Seq Bool))
  
   -- | 'leds' drives the leds
--   leds :: Matrix X8 (Seq Bool) -> fabric ()

   -- | 'dial_button' gives raw access to the state of the dial button
   dial_button :: fabric (Seq Bool)

   -- | 'dial_rot' gives Enabled packets when dial is rotated,
   -- and if the rotation was clockwise
   dial_rot :: fabric (Seq (Enabled Bool))

{-
   -- | 'mm_vgaP' gives a memory mapped API to the VGA port.
   -- Each charactor has an extra attribute
   mm_vgaP :: Patch (Seq (Enabled ((X40,X80),(VGA.Attr,U7)))) (fabric ())
                    (Seq Ack)	                              ()
-}

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

  mm_lcdP = patchF (mm_LCD_Inst $$ init_LCD $$ phy_Inst_4bit_LCD) |$| buildF (\ (bus,_) -> do
                let (rs,sf_d,e) = unpack bus
                lcd rs sf_d e                
                return ((),()))
      where lcd rs sf_d e = do 
		outStdLogic 	  "LCD_RS" rs
		outStdLogicVector "SF_D" (appendS (0 :: Seq (U8)) sf_d  :: Seq U12)
		outStdLogic       "LCD_E"  e
		outStdLogic       "LCD_RW" low
		outStdLogic       "SF_CE0" high

  rs232_txP serial baud = patchF (rs232out baud clockRate) |$| buildF (\ (bus,_) -> do
          outStdLogic ("RS232_" ++ show serial ++ "_TX") bus
          return ((),()))

  rs232_rxP serial baud = buildF (\ ~(_,_) -> do
           inp :: Seq Bool <- inStdLogic ("RS232_" ++ show serial ++ "_RX") 
           let (_,out) = execP (rs232in baud clockRate) (inp,())
           return ((),out))

  ------------------------------------------------------------
  -- RAW APIs
  ------------------------------------------------------------


{-
  switches = do
        inp <- inStdLogicVector "SW" :: Fabric (Seq (Matrix X4 Bool))
        return (unpack inp)
-}

--  leds inp = outStdLogicVector "LED" (pack inp :: Seq (Matrix X8 Bool))

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

{-
-- | 'switchesP' gives a patch-level API for the toggle switches.
switchesP :: (Spartan3e fabric) =>
             fabric (Patch () (Matrix X4 (Seq Bool))
	                   () (Matrix X4 ()))
switchesP = do
	sws <- switches
	return (outputP sws $$ 
	        backwardP (\ _mat -> ()) $$
                matrixStackP (pure emptyP))
-}


{-
-- | 'ledP' gives a patch-level API for the leds.
ledsP :: (Spartan3e fabric) =>
             Patch (Matrix X8 (Seq Bool)) (fabric ())
                   (Matrix X8 ())         ()
ledsP = 
        backwardP (\ () -> pure ()) $$
        forwardP leds
-}

 