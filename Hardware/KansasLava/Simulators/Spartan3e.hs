-- | This API should mirror 'Hardware.KansasLava.Boards.Spartan3e'.

module Hardware.KansasLava.Simulators.Spartan3e (
	-- * Initialization, and global settings.
	  board_init
	, rot_as_reset
	, clockRate
	, showUCF
        -- * Simulator only options
        , showClock
	-- * Patch API's.
--	, lcdPatch              -- unsupported in the simulator
	, mm_lcdPatch
	, switchesPatch
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

import Hardware.KansasLava.Boards.Spartan3e (clockRate)
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Matrix as M
import Language.KansasLava hiding (Fabric, runFabric)
import qualified Language.KansasLava as KL
import System.IO
import Control.Monad
import Data.List as List
import Data.Char as Char
import Control.Concurrent
import System.IO.Unsafe

import Hardware.KansasLava.Simulators.Fabric

rot_as_reset = undefined
showUCF _ = return "/* Simulator does not need a UCF */\n"
switchesPatch = undefined

board_init = do
        outFabric (\ _ -> BOARD) [()]
        quit <- inFabric False (\ c _ -> c == 'q')
        outFabric QUIT quit

showClock :: Int -> Fabric ()
showClock m = outFabric (CLOCK) [0..]

-----------------------------------------------------------------------
-- Patches
-----------------------------------------------------------------------

shallowSlowDownAckBoxPatch ::
        Integer -> Patch (Seq (Enabled U8))  (Seq (Enabled U8))
	                 (Seq Ack)	     (Seq Ack)
shallowSlowDownAckBoxPatch slow ~(inp,ack) = (toAck (toSeq ack_out),packEnabled (toSeq good) (enabledVal inp))
  where
        ack_in :: [Bool]
        ack_in = [ x | Just x <- fromSeq (fromAck ack) ]

        inp_in :: [Bool]
        inp_in = [ x | Just x <- fromSeq (isEnabled inp) ]

        good :: [Bool]        
        good = f 0 inp_in
        
        f 0 (False:is) =  False : f 0 is
        f 0 (True:is)  =  True  : f slow is
        f other (_:is)  = False : f (pred other) is

        ack_out :: [Bool]
        ack_out = ack_in


rs232_dce_tx :: Integer         -- ^ baud rate (ignored for simulation)
             -> Patch (Seq (Enabled U8))  (Fabric ())
	              (Seq Ack)	          ()

rs232_dce_tx baud = shallowSlowDownAckBoxPatch slow_count $$ fromAckBox $$ forwardPatch fab
   where
        -- 10 bits per byte
        slow_count = 10 * clockRate `div` baud
        fab inp = do
                writeFileFabric "dev/dce_tx" $ map (fmap fromIntegral) inp
                outFabricCount (RS232 TX 1) inp

rs232_dce_rx :: Integer
             -> Fabric (Patch () (Seq (Enabled U8))
	                      () ())
rs232_dce_rx baud = do
        -- 10 bits per byte
        let slow_count = 10 * clockRate `div` baud
        ss0 <- readFileFabric "dev/dce_rx"
        let ss = concatMap (\ x -> x : replicate (fromIntegral slow_count) Nothing) ss0
        outFabricCount (RS232 RX 1) ss
        return (unitPatch (toSeq (map (fmap fromIntegral) ss)))

-----------------------------------------------------------------------
-- 
-----------------------------------------------------------------------


leds :: Matrix X8 (Seq Bool) -> Fabric ()
leds m = do
        sequence_ [ outFabric ( LED (fromIntegral i)) (fromSeq (m ! i))
	          | i <- [0..7]
	          ]

switches :: Fabric (Matrix X4 (Seq Bool))
switches = do
        ms <- sequence [ do ss <- inFabric False (sw i)
                            outFabric (TOGGLE i) ss
                            return ss
                       | i <- [0..3]
                       ]
        return (matrix (map toSeq ms))
  where
        sw i ch old | key ! i == ch = not old       -- flip
                    | otherwise     = old           -- leave
         
        key :: Matrix X4 Char
        key = matrix "hjkl"

buttons :: Fabric (Matrix X4 (Seq Bool))
buttons = do
        ms <- sequence [ do ss <- inFabric False (sw i)
                            outFabric (BUTTON i) ss
                            return ss
                       | i <- [0..3]
                       ]
        return (matrix (map toSeq ms))
  where
        sw i ch old | key ! i == ch = not old       -- flip
                    | otherwise     = old           -- leave
         
        key :: Matrix X4 Char
        key = matrix "aegx"

data Dial = Dial Bool U2
        deriving Eq

-- 'dial' returns the status of the 
dial :: Fabric (Seq Bool, Seq (Enabled Bool))
dial = do 
        st <- ll_dial
        return ( toSeq $ map (\ (Dial b _) -> b) $ st
               , toSeq $ rot $ map (\ (Dial _ p) -> p) $ st
               )
  where
          rot xs = map f $ List.zipWith (-) (0:xs) xs

          f 0 = Nothing
          f 1 = Just False
          f 2 = error "turned dial twice in one cycle?"
          f 3 = Just True
        
ll_dial :: Fabric [Dial]
ll_dial = do 
        ss <- inFabric (Dial False 0) switch
        outFabric DIAL ss
        return ss
   where 
           switch 'd' (Dial b p) = Dial (not b) p
           switch 's' (Dial b p) = Dial b (pred p)
           switch 'f' (Dial b p) = Dial b (succ p)
           switch _   other      = other

mm_lcdPatch :: Patch (Seq (Enabled ((X2,X16),U8)))   (Fabric ())
	             (Seq Ack)	                    ()
mm_lcdPatch = fromAckBox $$ forwardPatch fab
   where
        fab inp = outFabricEvents $ map (just $ \ ((x,y),ch) -> Just (LCD (x,y) (Char.chr (fromIntegral ch)))) inp

        just :: (a -> Maybe b) -> Maybe a -> Maybe b
        just _ Nothing  = Nothing
        just k (Just a) = k a


boardASCII = unlines
 [ "    _||_____|VGA|_____|X|__|232 DCE|__|232 CTE|__"
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
 , "   |      (x)      +----------------+   h j k l  |_|"
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
        | RS232 DIR X2 Integer

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
        PRINT [up]   `at` (14,40 + 2 * fromIntegral x) 
        PRINT [down] `at` (15,40 + 2 * fromIntegral x)
  where
       ch = "hjkl" !! fromIntegral x
 
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
{-
 drawGraphic (RS232_TX DCE h var c) = do
        v <- takeMVar var
        let v' = v + 1
        putMVar var v'
        PRINT ("tx " ++ show v') `at` (2,27)
        hPutChar h (chr (fromIntegral c))
        hFlush h
-}
 drawGraphic (RS232 dir port val) 
      | val > 0   = PRINT (prefix ++ show val) `at` (col,row)
      | otherwise = PRINT (prefix ++ "-")      `at` (col,row)
  where
        row = matrix [ 27, 38 ] ! port

        prefix = case dir of
                   RX -> "rx "
                   TX -> "tx "
        col = case dir of
                   RX -> 3
                   TX -> 2
