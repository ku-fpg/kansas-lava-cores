{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
-- | This API mirrors 'Hardware.KansasLava.Boards.Spartan3e' via a class
-- abstaction. The other API also contains some Board specific utilties
-- that can also be used for simulation.

module Hardware.KansasLava.Simulators.Spartan3e 
--        ( Spartan3e(..)
--        , Graphic(..)
--        ) where
        where

import qualified Hardware.KansasLava.Boards.Spartan3e as Board
import Hardware.KansasLava.Boards.Spartan3e -- (board_init, rot_as_reset)
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
import Control.Monad.Fix

import Hardware.KansasLava.Rate

import Hardware.KansasLava.Simulators.Polyester
import Hardware.KansasLava.Peripherals

------------------------------------------------------------
-- Spartan3eSimulator Monad
------------------------------------------------------------

data Spartan3eSimulator a = Spartan3eSimulator (Polyester a)

instance Monad Spartan3eSimulator where
        return = Spartan3eSimulator . return
        (Spartan3eSimulator m) >>= k = Spartan3eSimulator $ do
                                        r <- m
                                        case k r of
                                          Spartan3eSimulator r2 -> r2

instance CoreMonad Spartan3eSimulator where
        core nm m = Spartan3eSimulator (core nm m)
         
------------------------------------------------------------
-- The instances for basic IO
------------------------------------------------------------

instance LEDs Spartan3eSimulator where
        type LEDCount Spartan3eSimulator = X8
        ledNames = return $ matrix ["LED<" ++ show i ++ ">" | i <- [0..maxBound :: X8]]
        
instance Switches Spartan3eSimulator where
        type SwitchCount Spartan3eSimulator = X4
        switchNames = return $ matrix ["SW<" ++ show i ++ ">" | i <- [0..maxBound :: X4]]

instance Buttons Spartan3eSimulator where
        type ButtonCount Spartan3eSimulator = X4
        buttonNames = return $ matrix ["BTN_NORTH","BTN_EAST","BTN_SOUTH","BTN_WEST"]

------------------------------------------------------------
-- The instances for higher level APIs
------------------------------------------------------------

instance DialRotation Spartan3eSimulator where
        dialRotation = core "dialRotation" $ do
                -- re-cast into U2
                pos     :: EXPR U2 <- INPUT (inStdLogicVector "dial_pos")
                VAR reg :: VAR U2  <- SIGNAL $ var 0
                (diff :: REG U2, result  :: EXPR (Maybe Bool))
                        <- mkChannel (\ x -> cASE [ (enabledVal x .==. 0, disabledS)
                                                  , (enabledVal x .==. 1, enabledS low)
                                                  , (enabledVal x .==. 3, enabledS high)
                                                  ] undefinedS)

                always $ do
                        reg := pos

                always $ do
                        diff := reg - pos

                return (result :: EXPR (Maybe Bool))
        dialButton = error "dial Button"

instance RS232 Spartan3eSimulator where
        type RS232Count Spartan3eSimulator = Board.Serial
        rs232rx port baud = do
           let portname = show port
               rx = portname ++ "/rx"
           polyester $ do
                readSocketPolyester
                        ("dev/" ++ portname)
                        rx
                        ((fromIntegral Board.clockRate `div` baud) * 10)
                acks :: Seq Bool <- fabric $ inStdLogic (rx ++ "_ack")
                outPolyesterCount (RS232 RX port) 
                        $ fmap (\ a -> if a then Just () else Nothing)
                        $ alwaysDefined "LCD bus failure"
                        $ fromS
                        $ acks

           core ("rs232rx/" ++ portname) $ do
                res :: EXPR (Enabled U8) <- INPUT (inStdLogicVector rx)
                ack :: REG ()            <- OUTPUT (outStdLogic (rx ++ "_ack") . isEnabled)
                always $ do
                        (OP1 isEnabled res) :? ack := OP0 (pureS ())
                return $ res

        rs232tx port baud = do
           let portname = show port
               tx = portname ++ "/tx"

           polyester $ do
                writeSocketPolyester
                        ("dev/" ++ portname)
                        tx
                        ((fromIntegral Board.clockRate `div` baud) * 10)
                outs :: Seq (Enabled U8) <- fabric $ inStdLogicVector tx
                outPolyesterCount (RS232 TX port) 
                        $ fmap (fmap $ const ())
                        $ alwaysDefined "LCD bus failure"
                        $ fromS
                        $ outs

           core ("rs232tx/" ++ portname) $ do
                (wt,rd) <- newAckBox
                VAR ch :: VAR U8 <- SIGNAL (var 0)
                out    :: REG U8 <- OUTPUT (outStdLogicVector tx)
                SPARK $ \ loop -> do
                        takeAckBox rd $ \ e -> out := e
                        -- insert delay
                        GOTO loop
                return $ wt
{-
               core "lcd" $ do
                wr ::   REG ((X2,X16),U8) <- OUTPUT (outStdLogicVector "lcd_wt")
                wr_ack :: EXPR (Maybe ()) <- INPUT  (inStdLogic "lcd_wt_ack" >>= return . flip packEnabled (pureS ()))
                return $ WriteAckBox wr wr_ack
-}

instance LCD Spartan3eSimulator where
        type LCDSize Spartan3eSimulator = (X2,X16)
        lcd = core "lcd" $ do
                wr ::   REG ((X2,X16),U8) <- OUTPUT (outStdLogicVector "lcd_wt")
                wr_ack :: EXPR (Maybe ()) <- INPUT  (inStdLogic "lcd_wt_ack" >>= return . flip packEnabled (pureS ()))
                return $ WriteAckBox wr wr_ack

------------------------------------------------------------
-- Spartan3eSimulator Initalization
------------------------------------------------------------

instance PolyesterMonad Spartan3eSimulator where
  fabric m = Spartan3eSimulator (fabric m)
  polyester m = Spartan3eSimulator m
  init_board = do
        led_names <- ledNames
        switch_names <- switchNames
        button_names <- buttonNames
        polyester $ do
                generic_init BOARD CLOCK
                cores <- initializedCores
                -- light up the LEDs
                when ("leds" `elem` cores) $
                        sequence_ [ do o :: Seq Bool <- fabric $ inStdLogic nm
                                       outPolyester (LED (fromIntegral i)) (fromS o)
	                          | (i,nm) <- zip [0..] (M.toList led_names)
	                          ]

                -- (always) listen to the switches
                id $ do let sw i ch old | key ! i == ch = not old       -- flip
                                        | otherwise     = old           -- leave

                            key :: Matrix X4 Char
                            key = matrix "lkjh"

                        sequence_ [ do ss <- inPolyester False (sw i)
                                       outPolyester (TOGGLE i) ss
                                       fabric $ outStdLogic nm (toS ss)
                                  | (i,nm) <- zip [0..] (M.toList switch_names)
                                  ]

                -- (always) listen to the buttons
                id $ do let sw i ch old | key ! i == ch = not old       -- flip
                                        | otherwise     = old           -- leave

                            key :: Matrix X4 Char
                            key = matrix "aegx"

                        sequence_ [ do ss <- inPolyester False (sw i)
                                       outPolyester (BUTTON i) ss
                                       fabric $ outStdLogic nm (toS ss)
                                  | (i,nm) <- zip [0..3] (M.toList button_names)
                                  ]

                -- (always) listen to the dial
                
                id $ do let switch 'd' (Dial b p) = Dial (not b) p
                            switch 's' (Dial b p) = Dial b (pred p)
                            switch 'f' (Dial b p) = Dial b (succ p)
                            switch _   other      = other

                        sequence_ [ do ss <- inPolyester (Dial False 0) switch
                                       outPolyester DIAL ss
                                       fabric $ outStdLogic       "dial_button" $ toS (map (\ (Dial b _) -> b) ss)
                                       fabric $ outStdLogicVector "dial_pos"    $ toS (map (\ (Dial _ p) -> p) ss)
                                  | (i,nm) <- zip [0..3] (M.toList button_names)
                                  ]


                when ("lcd" `elem` cores) $ do
                        chs :: Seq (Enabled ((X2,X16),U8)) <- fabric $ do
                                wt <- inStdLogicVector "lcd_wt" :: Fabric (Seq (Enabled ((X2,X16),U8)))
                                outStdLogic "lcd_wt_ack" (isEnabled wt)
                                return wt

                        let just :: (a -> Maybe b) -> Maybe a -> Maybe b
                            just _ Nothing  = Nothing
                            just k (Just a) = k a

                        outPolyesterEvents 
                                $ map (just $ \ ((x,y),ch) -> Just (LCD (x,y) (Char.chr (fromIntegral ch))))
                                $ alwaysDefined "LCD bus failure"
                                $ fromS chs


alwaysDefined :: String -> [Maybe a] -> [a]
alwaysDefined msg = map $ \ x -> case x of { Just r -> r ; Nothing -> error msg }

------------------------------------------------------------
-- initialization
------------------------------------------------------------

-- | 'board_init' sets up the use of the clock.
-- Always call 'board_init' first. 
-- Required.
{-
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

--   leds8        :: fabric (Matrix X8 (REG Bool))
   leds8 = do
           sequence_ [ do o <- fabric $ inStdLogic ("led" ++ show i)
                          outPolyester (LED (fromIntegral i)) (fromS o)
	             | i <- [0..7]
	             ]
           core "" $ do
                leds <- sequence
                        [ OUTPUT (outStdLogic ("led" ++ show i) . enabledVal)
                        | i <- [0..7] :: [Int]
                        ]
                return $ matrix leds

--           return (matrix $ take 8 $ repeat low)
        

--   switches4    :: fabric (Matrix X4 (EXPR Bool))
   switches4 = do
        let sw i ch old | key ! i == ch = not old       -- flip
                           | otherwise     = old           -- leave

            key :: Matrix X4 Char
            key = matrix "lkjh"

        sequence_ [ do ss <- inPolyester False (sw i)
                       fabric $ outStdLogic ("sw" ++ show i) (toS ss)
                  | i <- [0..3]
                  ]
        sws <- core "" $ sequence
                        [ INPUT (inStdLogic $ "sw" ++ show i)
                        | i <- [0..3] :: [Int]
                        ]
        return (matrix sws)


{- 
   --tickTock :: (Size w) => Witness w -> Integer -> fabric (Seq Bool)
   tickTock wit hz = do
           simSpeed <- getPolyesterSimSpeed
           return (rate wit (1 / ((fromIntegral simSpeed) / fromIntegral hz)))

   -----------------------------------------------------------------------
   -- Patches
   -----------------------------------------------------------------------

-- (Seq (Enabled ((X2,X16),U8)))  ()
-- (Seq Ack)	                 ()
	                  
   mm_lcdP = patchF fromAckBox |$| buildF (\ ~(inp_lhs,_) -> do
           outPolyesterEvents $ map (just $ \ ((x,y),ch) -> Just (LCD (x,y) (Char.chr (fromIntegral ch)))) inp_lhs
           return ((),()))
      where
        just :: (a -> Maybe b) -> Maybe a -> Maybe b
        just _ Nothing  = Nothing
        just k (Just a) = k a

   rs232_txP port baud = patchF (shallowSlowDownAckBoxP slow_count $$ fromAckBox) |$| buildF fab
      where
        -- 10 bits per byte
        slow_count = 10 * Board.clockRate `div` baud
        fab ~(inp,_) = do
                writeSocketPolyester ("dev/" ++ serialName port)
                        $ map (fmap (\ i -> [chr (fromIntegral i)])) inp
                outPolyesterCount (RS232 TX port) inp
                return ((),())

   rs232_rxP port baud = buildF (\ ~(_,_) -> do
        -- 10 bits per byte
        clkSpeed <- getPolyesterClkSpeed
        let slow_count = 10 * clkSpeed `div` baud
        ss0 <- readSocketPolyester ("dev/" ++ serialName port)
        let ss = concatMap (\ x -> x : replicate (fromIntegral slow_count) Nothing) ss0
        outPolyesterCount (RS232 RX port) ss
        return ((), toS (map (fmap fromIntegral) ss)))
-}
   -----------------------------------------------------------------------
   -- Native APIs
   -----------------------------------------------------------------------
{-
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
-}
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
{-
   leds m = do
        sequence_ [ outPolyester (LED (fromIntegral i)) (fromS (m ! i))
	          | i <- [0..7]
	          ]
-}
{-
   mm_vgaP = fromAckBox $$ forwardP fab
      where
        fab :: [Maybe ((X40, X80), (VGA.Attr, U7))] -> Polyester ()
        fab inp = do
                writeFilePolyester ("dev/vga") 
                        ((Just $ VGA.init_VCG_ANSI) : map (fmap (VGA.show_VCG_ANSI)) inp)
-}

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
-}

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
