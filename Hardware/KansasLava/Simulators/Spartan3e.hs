-- | This API mirrors 'Hardware.KansasLava.Boards.Spartan3e' via a class
-- abstaction. The other API also contains some Board specific utilties
-- that can also be used for simulation.

module Hardware.KansasLava.Simulators.Spartan3e 
        ( Spartan3e(..)
        , Graphic(..)
        ) where
{-        
	-- * Initialization, and global settings.
	  board_init
	, rot_as_reset
	, Board.clockRate
	, showUCF
	-- * Patch API's.
--	, lcdP              -- unsupported in the simulator
	, mm_lcdP
	, switchesP
        , rs232_dce_rx
        , rs232_dce_tx
--        , rs232_dte_rx
--        , rs232_dte_tx
	 -- * Raw API's.
--	, lcd                   -- unsupported in the simulator
	, switches
        , dial  
        , leds
        , buttons
	) where
-}

import qualified Hardware.KansasLava.Boards.Spartan3e as Board
import Hardware.KansasLava.Boards.Spartan3e -- (board_init, rot_as_reset)
import qualified Hardware.KansasLava.VGA as VGA
import qualified Data.ByteString as B

import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix as M
import Language.KansasLava
import qualified Language.KansasLava as KL
import System.IO
import Control.Monad
import Data.List as List
import Data.Char as Char
import Control.Concurrent
import System.IO.Unsafe
import Data.Maybe

import Hardware.KansasLava.Simulators.Polyester

------------------------------------------------------------
-- initialization
------------------------------------------------------------

-- | 'board_init' sets up the use of the clock.
-- Always call 'board_init' first. 
-- Required.
instance Board.Spartan3e Polyester where 

   board_init = do
        generic_init BOARD CLOCK

        -- we now grab the input streams, and display any change of switches.
        sw <- switches
        sequence_ 
           [ outPolyester (TOGGLE i) (map fromJust (fromS (sw ! i)))
           | i <- [0..3]
           ]
        sw <- buttons
        sequence_ 
           [ outPolyester (BUTTON i) (map fromJust (fromS (sw ! i)))
           | i <- [0..3]
           ]

        ss <- ll_dial
        outPolyester DIAL ss

        
   -- This does nothing on the simulator, because the shallow circuits
   -- can not do a hard reset.
   rot_as_reset = return ()
 
   -----------------------------------------------------------------------
   -- Patches
   -----------------------------------------------------------------------

   mm_lcdP = fromAckBox $$ forwardP fab
      where
        fab inp = outPolyesterEvents $ map (just $ \ ((x,y),ch) -> Just (LCD (x,y) (Char.chr (fromIntegral ch)))) inp

        just :: (a -> Maybe b) -> Maybe a -> Maybe b
        just _ Nothing  = Nothing
        just k (Just a) = k a

   lcdP = error "lcdP is not supported in the simulator. Use mm_lcdP and the memory mapped API instead"

   rs232_txP port baud = shallowSlowDownAckBoxP slow_count $$ fromAckBox $$ forwardP fab
      where
        -- 10 bits per byte
        slow_count = 10 * Board.clockRate `div` baud
        fab inp = do
                writeFilePolyester ("dev/" ++ serialName port ++ "_tx") 
                        $ map (fmap (\ i -> [chr (fromIntegral i)])) inp
                outPolyesterCount (RS232 TX port) inp

   rs232_rxP port baud = do
        -- 10 bits per byte
        let slow_count = 10 * Board.clockRate `div` baud
        ss0 <- readFilePolyester ("dev/" ++ serialName port ++ "_rx")
        let ss = concatMap (\ x -> x : replicate (fromIntegral slow_count) Nothing) ss0
        outPolyesterCount (RS232 RX port) ss
        return (outputP (toS (map (fmap fromIntegral) ss)))

   -----------------------------------------------------------------------
   -- Native APIs
   -----------------------------------------------------------------------

   lcd = error "lcd is not supported in the simulator. (to low level)"

   switches = do
        ms <- sequence [ do ss <- inPolyester False (sw i)
                            return ss
                       | i <- [0..3]
                       ]
        return (matrix (map toS ms))
      where
        sw i ch old | key ! i == ch = not old       -- flip
                    | otherwise     = old           -- leave
         
        key :: Matrix X4 Char
        key = matrix "lkjh"

   buttons = do
        ms <- sequence [ do ss <- inPolyester False (sw i)
                            return ss
                       | i <- [0..3]
                       ]
        return (matrix (map toS ms))
      where
        sw i ch old | key ! i == ch = not old       -- flip
                    | otherwise     = old           -- leave
         
        key :: Matrix X4 Char
        key = matrix "aegx"

   leds m = do
        sequence_ [ outPolyester (LED (fromIntegral i)) (fromS (m ! i))
	          | i <- [0..7]
	          ]

   mm_vgaP = fromAckBox $$ forwardP fab
      where
        fab :: [Maybe ((X40, X80), (VGA.Attr, U7))] -> Polyester ()
        fab inp = do
                writeFilePolyester ("dev/vga") 
                        ((Just $ VGA.init_VCG_ANSI) : map (fmap (VGA.show_VCG_ANSI)) inp)


   dial_button = do
        st <- ll_dial
        return $ toS $ map (\ (Dial b _) -> b) $ st


   dial_rot = do
        st <- ll_dial
        return $ toS $ rot $ map (\ (Dial _ p) -> p) $ st
      where
          rot xs = map f $ List.zipWith (-) (0:xs) xs

          f 0 = Nothing
          f 1 = Just False
          f 2 = error "turned dial twice in one cycle?"
          f 3 = Just True

-----------------------------------------------------------------------
-- Utilities uses in the class defintion.
-----------------------------------------------------------------------

serialName :: Serial -> String
serialName DCE = "dce"
serialName DTE = "dte"

data Dial = Dial Bool U2
        deriving Eq


ll_dial :: Polyester [Dial]
ll_dial = do 
        ss <- inPolyester (Dial False 0) switch
        return ss
   where 
           switch 'd' (Dial b p) = Dial (not b) p
           switch 's' (Dial b p) = Dial b (pred p)
           switch 'f' (Dial b p) = Dial b (succ p)
           switch _   other      = other

shallowSlowDownAckBoxP ::
        Integer -> Patch (Seq (Enabled U8))  (Seq (Enabled U8))
	                 (Seq Ack)	     (Seq Ack)
shallowSlowDownAckBoxP slow ~(inp,ack) = (toAck (toS ack_out),packEnabled (toS good) (enabledVal inp))
  where
        ack_in :: [Bool]
        ack_in = [ x | Just x <- fromS (fromAck ack) ]

        inp_in :: [Bool]
        inp_in = [ x | Just x <- fromS (isEnabled inp) ]

        good :: [Bool]        
        good = f 0 inp_in
        
        f 0 (False:is) =  False : f 0 is
        f 0 (True:is)  =  True  : f slow is
        f other (_:is)  = False : f (pred other) is

        ack_out :: [Bool]
        ack_out = ack_in


-- | The clock rate on the Spartan3e (50MHz), in hertz.
clockRate :: Integer
clockRate = Board.clockRate

-----------------------------------------------------------------------
-- 
-----------------------------------------------------------------------


boardASCII = unlines
 [ "    _||_____|VGA|_____|X|__|232 DCE|__|232 DTE|__"
 , "   |o||                                          |_"
 , "   |                                             | |"
 , "   |                     +----+                  | |"
 , "  ----+         DIGILENT |FPGA|                  | |"
 , "  RJ45|   ##             |    |    SPARTAN-3E    | |"
 , "  ----+   ##             +----+     \\      /     | |"
 , "  _|_                                \\    / ()   | |"
 , "  USB|     +--+                       \\  /       |_|"
 , "  ---'     |##|         +----+       FPGA        |_"
 , "   |+--+   +--+         |####|         oooooooo  | |"
 , "   ||##|           +----------------+  76543210  |_|"
 , "   |+--+  (e)      |                |            |_"
 , "   |  (a) (|) (g)  |                |   : : : :  | |"
 , "   |      (x)      +----------------+   * * * *  |_|"
 , "   +---------------------------------------------+"
 , ""
 , "   Keyboard Commands:"
 , "     a, e, g, x - press buttons"
 , "     d          - press dial"
 , "     s, f       - turn dial counter-clock/clockwise"
 , "     h,j,k,l    - toggle switches"
 , "     q          - quit"
 ]



-----------------------------------------------------------------------
-- Output To Screen Driver
-----------------------------------------------------------------------

data Output
	= LED X8 (Maybe Bool)
	| TOGGLE X4 Bool
        | CLOCK Integer
        | LCD (X2,X16) Char
        | BOARD
        | BUTTON X4 Bool
        | DIAL Dial
        | QUIT Bool
        | RS232 DIR Serial Integer

data DIR  = RX | TX

at = AT

instance Graphic Output where 
 drawGraphic (LED x st) = 
        opt_green $ PRINT [ledASCII st] `at` (11,46 - fromIntegral x)
   where
        opt_green = if st == Just True then COLOR Green else id

        ledASCII :: Maybe Bool -> Char
        ledASCII Nothing      = '?'
        ledASCII (Just True)  = '@'
        ledASCII (Just False) = '.'

 drawGraphic (TOGGLE x b) = do
        PRINT [up]   `at` (14,46 - 2 * fromIntegral x) 
        PRINT [down] `at` (15,46 - 2 * fromIntegral x)
  where
       ch = "lkjh" !! fromIntegral x
 
       up = if b then ch else ':'
       down = if b then ':' else ch
 drawGraphic (CLOCK n) = 
        PRINT ("clk: " ++ show n) `at` (5,35)
 drawGraphic (LCD (row,col) ch) =
        PRINT [ch] `at` (13 + fromIntegral row,20 + fromIntegral col)
 drawGraphic BOARD = do
        PRINT boardASCII `at` (1,1)
        COLOR Red $ PRINT ['o'] `at` (2,4)
 drawGraphic (BUTTON x b) = 
        (if b then REVERSE else id) $
        PRINT [snd (buttons !! fromIntegral x)] `at` 
              (fst (buttons !! fromIntegral x)) 
  where
       buttons = 
               [ ((14,7),'a')
               , ((13,11),'e')
               , ((14,15),'g')
               , ((15,11),'x')
               ]
 drawGraphic (DIAL (Dial b p)) = 
        (if b then REVERSE else id) $
        PRINT ["|/-\\" !! fromIntegral p] `at` (14,11)
 drawGraphic (QUIT b)
        | b = do PRINT "" `at` (25,1)
                 error "Simulation Quit"
        | otherwise = return ()
 drawGraphic (RS232 dir port val) 
      | val > 0   = PRINT (prefix ++ show val) `at` (col,row)
      | otherwise = PRINT (prefix ++ "-")      `at` (col,row)
  where
        row = case port of
                DCE -> 27
                DTE -> 38

        prefix = case dir of
                   RX -> "rx "
                   TX -> "tx "
        col = case dir of
                   RX -> 3
                   TX -> 2
