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
--        , rs232_dce_rx
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
clockRate = undefined
showUCF _ = return "/* Simulator does not need a UCF */\n"
switchesPatch = undefined

board_init = do
        outFabric $ [return BOARD]
        inFabric False
                (QUIT)
                (\ c _ -> c == 'q')
        

showClock :: Int -> Fabric ()
showClock m = outFabric $ 
        [ if (n `mod` fromIntegral m == 0) 
          then Just (CLOCK n)
          else Nothing
        | n <- [0..] 
        ]

-----------------------------------------------------------------------
-- Patches
-----------------------------------------------------------------------

rs232_dce_tx :: Integer         -- ^ baud rate (ignored for simulation)
             -> Patch (Seq (Enabled U8))  (Fabric ())
	              (Seq Ack)	          ()

rs232_dce_tx baud = fromAckBox $$ forwardPatch fab
   where
        fab inp = outFabricIO start $ \ (h,c) ->
                     map (just $ \ ch -> Just (RS232_TX DCE h c ch)) inp

        start = do
                h <- openBinaryFile "dev/dce_out" AppendMode
                hSetBuffering h NoBuffering        
                c <- newMVar 0
                return (h,c)

        just :: (a -> Maybe b) -> Maybe a -> Maybe b
        just _ Nothing  = Nothing
        just k (Just a) = k a

--rs232_dce_rx :: Fabric (Patch () (Seq (Enabled U8))
--	                      () ())


-----------------------------------------------------------------------
-- 
-----------------------------------------------------------------------


leds :: Matrix X8 (Seq Bool) -> Fabric ()
leds m = do
        sequence_ [ outFabric $ map (\ c -> case c of
	                                    Nothing -> Nothing
	                                    Just b -> Just (LED (fromIntegral i) b))
	                     $ changed (fromSeq (m ! i))
	          | i <- [0..7]
	          ]

switches :: Fabric (Matrix X4 (Seq Bool))
switches = do
        ms <- sequence [ inFabric False
                                  (\ b -> TOGGLE i b)
                                  (sw i)
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
        ms <- sequence [ inFabric False
                                  (\ b -> BUTTON i b)
                                  (sw i)
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
ll_dial = inFabric (Dial False 0)
                   (DIAL)
                   switch
   where 
           switch 'd' (Dial b p) = Dial (not b) p
           switch 's' (Dial b p) = Dial b (pred p)
           switch 'f' (Dial b p) = Dial b (succ p)
           switch _   other      = other

mm_lcdPatch :: Patch (Seq (Enabled ((X2,X16),U8)))   (Fabric ())
	             (Seq Ack)	                    ()
mm_lcdPatch = fromAckBox $$ forwardPatch fab
   where
        fab inp = outFabric $ map (just $ \ ((x,y),ch) -> Just (LCD (x,y) (Char.chr (fromIntegral ch)))) inp

        just :: (a -> Maybe b) -> Maybe a -> Maybe b
        just _ Nothing  = Nothing
        just k (Just a) = k a

toggle :: [Bool] -> [Bool]
toggle = f False
  where
        f st (True:rs) = not st : f (not st) rs
        f st (_:rs)    = st     : f st rs
        f st []        = []


changed :: (Eq a) => [a] -> [Maybe a]
changed (a:as) = Just a : f a as
  where
        f x (y:ys) | x == y    = Nothing : f x ys
                   | otherwise = Just y : f y ys
        f _ [] = []

diff :: (Eq a) => [Maybe a] -> [Maybe a]
diff xs = Nothing : f Nothing xs
 where 
  f x (y:ys) | x == y    =     f x ys
	     | otherwise = y : f y ys
  f x [] = []

boardASCII = unlines
 [ "   _||_____|VGA|_____|X|__|232 DCE|__|232 CTE|__"
 , "  |oXX                                          |_"
 , "  |                                             | |"
 , "  |                     +----+                  | |"
 , " ----+         DIGILENT |FPGA|                  | |"
 , " RJ45|   ##             |    |    SPARTAN-3E    | |"
 , " ----+   ##             +----+     \\      /     | |"
 , " _|_                                \\    / ()   | |"
 , " USB|     +--+                       \\  /       |_|"
 , " ---'     |##|         +----+       FPGA        |_"
 , "  |+--+   +--+         |####|         oooooooo  | |"
 , "  ||##|           +----------------+  76543210  |_|"
 , "  |+--+  (e)      |                |            |_"
 , "  |  (a) (|) (g)  |                |   : : : :  | |"
 , "  |      (x)      +----------------+   h j k l  |_|"
 , "  +---------------------------------------------+"
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
        | RS232_TX RS232 Handle (MVar Integer) U8
        
data RS232 = DCE | DTE

instance Graphic Output where 
 drawGraphic (LED x st) = 
        opt_green $ putChar (ledASCII st) `at` (11,46 - fromIntegral x)
   where
        opt_green = if st == Just True then green else id

        ledASCII :: Maybe Bool -> Char
        ledASCII Nothing      = '?'
        ledASCII (Just True)  = '@'
        ledASCII (Just False) = '.'

 drawGraphic (TOGGLE x b) = do
        putChar up   `at` (14,40 + 2 * fromIntegral x)
        putChar down `at` (15,40 + 2 * fromIntegral x)
  where
       ch = "hjkl" !! fromIntegral x
 
       up = if b then ch else ':'
       down = if b then ':' else ch
 drawGraphic (CLOCK n) = do
        let n_txt = show n
        putStr n_txt `at` (5,47 - Prelude.length n_txt)
 drawGraphic (LCD (row,col) ch) =
        putChar ch `at` (13 + fromIntegral row,20 + fromIntegral col)
 drawGraphic BOARD =
         putStr boardASCII `at` (1,1)
 drawGraphic (BUTTON x b) = 
        (if b then reverse_video else id) $
        putChar (snd (buttons !! fromIntegral x)) `at` 
                (fst (buttons !! fromIntegral x)) 
  where
       buttons = 
               [ ((14,7),'a')
               , ((13,11),'e')
               , ((14,15),'g')
               , ((15,11),'x')
               ]
 drawGraphic (DIAL (Dial b p)) = 
        (if b then reverse_video else id) $
        putChar ("|/-\\" !! fromIntegral p) `at` (14,11)
 drawGraphic (QUIT b)
        | b = do return () `at` (25,1)
                 error "Simulation Quit"
        | otherwise = return ()
 drawGraphic (RS232_TX DCE h var c) = do
        v <- takeMVar var
        let v' = v + 1
        putMVar var v'
        putStr ("o:" ++ show v') `at` (2,27)
        hPutChar h (chr (fromIntegral c))
        hFlush h

