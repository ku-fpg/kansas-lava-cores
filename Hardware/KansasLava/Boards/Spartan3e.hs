module Hardware.KansasLava.Boards.Spartan3e (
	-- * Initialization, and global settings.
	  board_init
	, rot_as_reset
	, clockRate
	, writeUCF
	-- * Patch API's.
	, lcdPatch
	, mm_lcdPatch
	, switchesPatch
	-- * Raw API's.
	, lcd
	, switches
--        , dial  
        , leds
        , buttons
	) where

import Language.KansasLava as KL
import Hardware.KansasLava.LCD.ST7066U
import Data.Sized.Unsigned
import Data.Sized.Ix hiding (all)
import Data.Sized.Matrix hiding (all)
import Data.Char
import System.IO
import Paths_kansas_lava_cores
 
------------------------------------------------------------
-- initialization
------------------------------------------------------------

-- | 'board_init' sets up the use of the clock.
-- Always call 'board_init' first. 
-- Required.
board_init :: Fabric ()
board_init = do
	theClk "CLK_50MHZ"		-- we need to name and pull in the clock

-- | 'rot_as_reset' sets up the rotary dial as a reset switch.
rot_as_reset :: Fabric ()
rot_as_reset = theRst "ROT_CENTER"

-- | The clock rate on the Spartan3e (50MHz), in hertz.
clockRate :: Integer
clockRate = 50 * 1000 * 1000

-- | show out a suggested UCF file for Spartan3e, for a specific circuit.
writeUCF :: FilePath -> KLEG -> IO ()
writeUCF ucf_filename kleg = do
        let inputs = theSrcs kleg
        print inputs
        let findMe = concat
                     [ case toStdLogicType ty of
                         SL -> [ nm ]
                         SLV n -> [ nm ++ "<" ++ show i ++ ">" 
                                  | i <- [0..(n-1)]
                                  ]
                     | (OVar _ nm,ty) <- (theSrcs kleg) ++ map (\ (a,b,c) -> (a,b)) (theSinks kleg)
                     ]

        let isComment ('#':_) = True
            isComment xs             | all isSpace xs = True
            isComment _       = False

        let getName xs | take 5 xs == "NET \""
                       = Just (takeWhile (/= '"') (drop 5 xs))
            getName _ = Nothing

        let hdr = unlines 
                [ "# Generated automatically by kansas-lava-cores"
                , "#" ++ show findMe
                ]

        filename <- getDataFileName "UCF/Spartan3e.ucf"
        print filename
        big_ucf <- readFile filename
        let lns = unlines
                  [ let allow = case getName ln of
                          Nothing -> True
                          Just nm -> nm `elem` findMe
                    in (if allow then ""  else "# -- ") ++ ln
                  | ln <- lines big_ucf
                  ]

        writeFile ucf_filename (hdr ++ lns)

------------------------------------------------------------
-- Patches
------------------------------------------------------------

-- | 'lcdPatch' gives a memory mappped (mm) API to the LCD.
--  Disables the StrataFlash (for now).

mm_lcdPatch :: Patch (Seq (Enabled ((X2,X16),U8)))  (Fabric ())
	             (Seq Ack)	                    ()
mm_lcdPatch = 
        mm_LCD_Inst $$
	init_LCD $$ 
	phy_Inst_4bit_LCD $$ 
	forwardPatch (\ bus -> do 
		let (rs,sf_d,e) = unpack bus
		lcd rs sf_d e)

-- 
-- | 'lcdPatch' gives a patch-level API to the LCD, based on LCDInstructions.
--  Disables the StrataFlash (for now).
lcdPatch :: Patch (Seq (Enabled LCDInstruction)) (Fabric ())
	     	  (Seq Ack)	                 ()
lcdPatch = 
	init_LCD $$ 
	phy_Inst_4bit_LCD $$ 
	forwardPatch (\ bus -> do 
		let (rs,sf_d,e) = unpack bus
		lcd rs sf_d e)

-- | 'switchesPatch' gives a patch-level API for the toggle switches.

switchesPatch :: Fabric (Patch () (Matrix X4 (Seq Bool))
			       () (Matrix X4 ()))
switchesPatch = do
	sws <- switches
	return (unitPatch sws $$ backwardPatch (\ _mat -> ()))

------------------------------------------------------------
-- RAW APIs
------------------------------------------------------------

-- | 'lcd' give raw access to the lcd bus. Disables the StrataFlash (for now).

lcd :: Seq U1 -> Seq U4 -> Seq Bool -> Fabric ()
lcd rs sf_d e = do 
		outStdLogic 	  "LCD_RS" rs
		outStdLogicVector "SF_D" (KL.append (0 :: Seq (U8)) sf_d  :: Seq U12)
		outStdLogic       "LCD_E"  e
		outStdLogic "LCD_RW" low
		outStdLogic "SF_CE0" high


-- | 'switches' gives raw access to the position of the toggle switches.
switches :: Fabric (Matrix X4 (Seq Bool))
switches = do
        inp <- inStdLogicVector "SW" :: Fabric (Seq (Matrix X4 Bool))
        return (unpack inp)


buttons :: Fabric (Matrix X4 (Seq Bool))
buttons = do
        i0 <- inStdLogic "BTN_WEST"
        i1 <- inStdLogic "BTN_NORTH"
        i2 <- inStdLogic "BTN_EAST"
        i3 <- inStdLogic "BTN_SOUTH"
        return (matrix [i0,i1,i2,i3])

leds :: Matrix X8 (Seq Bool) -> Fabric ()
leds inp = outStdLogicVector "LED" (pack inp :: Seq (Matrix X8 Bool))


