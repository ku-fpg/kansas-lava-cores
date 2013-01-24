{-# LANGUAGE ScopedTypeVariables,TypeFamilies, FlexibleContexts, RankNTypes, RecursiveDo, DoRec, DataKinds,  StandaloneDeriving, DataKinds #-}

module Hardware.KansasLava.Boards.Spartan3e where
{-
        -- * The Spartan3e Monad
          Spartan3e(..)
        , runSpartan3e
	-- * Initialization, and global settings.
	, clockRate
	, writeUCF
        -- * Data structures
        , Serial(..)
        -- * Utilities for Board and Simulation use
        , DialRotation(..)
	) where
-}
import Language.KansasLava as KL
import Language.KansasLava.Fabric
import Language.KansasLava.Signal
import Language.KansasLava.Universal
import Hardware.KansasLava.Simulator as P
import System.Console.ANSI
import Data.Char as Char
import Data.Array.IArray
import GHC.TypeLits
import Control.Monad.IO.Class
import Data.Word
{-

-}
--import Hardware.KansasLava.LCD.ST7066U
--import Hardware.KansasLava.RS232
--import Hardware.KansasLava.Rate
import Hardware.KansasLava.Boards.UCF
--import Hardware.KansasLava.Peripherals
--import Hardware.KansasLava.Core
--import Hardware.KansasLava.Boards.Physical

import Control.Monad.Trans.Class
import Data.Sized.Unsigned
import Data.Sized.Sized

import Data.Sized.Matrix hiding (all)
import qualified Data.Sized.Matrix as M
import Data.Char
import System.IO
import Control.Applicative
import Control.Monad.Fix
import Language.KansasLava.Stream (Stream)
import qualified Language.KansasLava.Stream as S

------------------------------------------------------------
-- The Spartan3e Class
------------------------------------------------------------

class (LocalM m) => Spartan3e m where
        clkSpeed ::                             m Integer -- Hz
        leds     :: Vector 8 (Signal (LocalClock m) Bool)
                                             -> m ()
        switches ::                             m (Vector 4 (Signal (LocalClock m) Bool))
        buttons  ::                             m (Vector 4 (Signal (LocalClock m)  Bool))
        dial     ::                             m ( Signal (LocalClock m)  Bool
                                                  , Signal (LocalClock m)  (Enabled Bool)
                                                  )
        lcd      :: Int -> Bus (LocalClock m) ((Sized 2,Sized 16),U8) -> m ()

        rs232rx  :: Serial -> Int            -> m (Bus (LocalClock m) U8)
        rs232tx  :: Serial -> Int -> Bus (LocalClock m) U8  -> m ()
{-
        mem_chip :: Bus (U20,U8) -> Bus U20  -> m (Bus (U16,U16))       -- unclear how to do this
-}
        probe    :: Rep a => String -> Signal (LocalClock m) a -> m ()


------------------------------------------------------------
-- The board clock

data Spartan3eClock = Spartan3eClock

instance Clock Spartan3eClock

------------------------------------------------------------
-- The Spartan3e Board Monad
------------------------------------------------------------

data Spartan3eBoard a = Spartan3eBoard
        { unSpartan3eBoard :: Integer -> SuperFabric Spartan3eClock Pure a }

instance Monad Spartan3eBoard where
        return a = Spartan3eBoard $ \ _ -> return a
        (Spartan3eBoard m) >>= k = Spartan3eBoard $ \ env -> do
                                r <- m env
                                unSpartan3eBoard (k r) env

instance MonadFix Spartan3eBoard where
        mfix f = Spartan3eBoard $ \ env -> do
                        rec a <- unSpartan3eBoard (f a) env
                        return a


instance LocalM Spartan3eBoard where
        type LocalClock Spartan3eBoard = Spartan3eClock
        newSignalVar           = Spartan3eBoard $ \ _ -> newSignalVar
        writeSignalVar var sig = Spartan3eBoard $ \ _ -> writeSignalVar var sig
        readSignalVar  var fn  = Spartan3eBoard $ \ _ -> readSignalVar var fn

instance Spartan3e Spartan3eBoard where
        clkSpeed = Spartan3eBoard $ \ i -> return i
        leds m = Spartan3eBoard $ \ _ -> outStdLogicVector "LED" (pack m :: Signal (LocalClock Spartan3eBoard) (Vector 8 Bool))
        switches = Spartan3eBoard $ \ _ -> do
                sw :: Signal (LocalClock Spartan3eBoard) (Vector 4 Bool) <- inStdLogicVector "SW"
                return $ unpack sw
        buttons = Spartan3eBoard $ \ _ -> do
                bns <- sequence [ inStdLogic nm
                                | nm <- ["BTN_NORTH"
                                        ,"BTN_EAST"
                                        ,"BTN_SOUTH"
                                        ,"BTN_WEST"
                                        ]
                                ]
                return $ matrix bns

-- The Spartan3e Simulator Monad
------------------------------------------------------------

data Spartan3eSimulator a = Spartan3eSimulator
        { unSpartan3eSimulator :: SuperFabric Spartan3eClock Simulator a }

instance Monad Spartan3eSimulator where
        return a = Spartan3eSimulator (return a)
        (Spartan3eSimulator m) >>= k = Spartan3eSimulator $ do
                                v <- m
                                unSpartan3eSimulator (k v)


instance MonadFix Spartan3eSimulator where
        mfix f = Spartan3eSimulator $ do
                        rec a <- unSpartan3eSimulator (f a)
                        return a

instance LocalM Spartan3eSimulator where
        type LocalClock Spartan3eSimulator = Spartan3eClock
        newSignalVar           = Spartan3eSimulator $ newSignalVar
        writeSignalVar var sig = Spartan3eSimulator $ writeSignalVar var sig
        readSignalVar  var fn  = Spartan3eSimulator $ readSignalVar var fn


-- The point here is you can not generate arbitary outputs and inputs
--instance InOutM Spartan3eSimulator where
--        input nm pad = Spartan3eSimulator $ input nm pad
--        output nm pad = Spartan3eSimulator $ output nm pad


runSpartan3eSimulator :: Spartan3eSimulator () -> IO ()
runSpartan3eSimulator (Spartan3eSimulator m) = runSimulator P.Friendly 100 100 $ do
                liftIO $ showANSI $ drawGraphic BOARD
                ((),pads) <- runFabric m []
--                outDebuggingSimulator pads
                return ()

type instance (2 + 16) = 18
type instance (18 + 8) = 26
type instance (1 + 26) = 27
type instance (1 + 32) = 33

instance Spartan3e Spartan3eSimulator where
        clkSpeed = Spartan3eSimulator $ lift getSimulatorClkSpeed

        leds mat = Spartan3eSimulator $ lift $ sequence_
                [ outSimulator (LED x) $ shallowS light
                | (x,light) <- assocs mat
                ]

        switches = Spartan3eSimulator $ lift $ do
                let sw i ch old | key ! i == ch = not old       -- flip
                                | otherwise     = old           -- leave

                    key :: Vector 4 Char
                    key = matrix "lkjh"

                m <- sequence
                          [ do ss <- inSimulator False (sw i)
                               outSimulator (INPUT $ TOGGLE i) ss
                               return $ mkShallowXS $ fmap pureX $ ss
                          | i <- [0..3]
                          ]
                return $ matrix m

        buttons = Spartan3eSimulator $ lift $ do
                let sw i ch old | key ! i == ch = not old       -- flip
                                | otherwise     = old           -- leave

                    key :: Vector 4 Char
                    key = matrix "aegx"

                m <- sequence
                          [ do ss <- inSimulator False (sw i)
                               outSimulator (INPUT $ BUTTON i) ss
                               return $ mkShallowXS $ fmap pureX $ ss
                          | i <- [0..3]
                          ]
                return $ matrix m

        dial = Spartan3eSimulator $ lift $ do
                let switch 'd' (Dial b p) = Dial (not b) p
                    switch 's' (Dial b p) = Dial b (pred p)
                    switch 'f' (Dial b p) = Dial b (succ p)
                    switch _   other      = other

                ss <- inSimulator (Dial False 0) switch
                outSimulator DIAL ss

                let delta :: Stream U2 -> Stream (Enabled Bool)
                    delta xs = Nothing `S.cons` S.zipWith (\ a b -> f (a - b))
                                                          xs
                                                          (S.tail xs)

                    f 0 = Nothing
                    f 1 = Just True
                    f 2 = Nothing       -- should never happen
                    f 3 = Just False


                return ( mkShallowXS
                             $ fmap pureX
                             $ fmap (\ (Dial b _) -> b)
                             $ ss
                       , mkShallowXS
                             $ fmap pureX
                             $ delta
                             $ fmap (\ (Dial _ p) -> p)
                             $ ss
                       )

        lcd _ bus = do
                CHAN lcd_out lcd_in :: CHAN Spartan3eClock ((Sized 2,Sized 16),U8) <- channel
                spark $ do
                        lab <- STEP
                        takeBus bus lcd_in $ GOTO lab

                let ss :: [Maybe (Enabled ((Sized 2,Sized 16),U8))] = fromS lcd_out

                Spartan3eSimulator $ lift $ outSimulatorEvents
                        $ fmap (\ ss -> case ss of
                            Nothing -> Nothing -- should not happen; consider X'ing the LCD
                            Just Nothing -> Nothing
                            Just (Just ((x,y),ch)) -> Just (LCD (x,y) (Char.chr (fromIntegral ch))))
                        $ S.fromList
                        $ fromS
                        $ lcd_out

                return ()

        rs232rx port baud = do
                let portname = show port
                    rx = portname ++ "/rx"

                xs :: [Maybe U8] <- Spartan3eSimulator $ lift $ do
                        readSocketSimulator
                                ("dev/" ++ portname)
                                rx
                                100      -- for now, should really compute this

                latchBus (toS xs)

        rs232tx port baud bus = do
                let portname = show port
                    tx = portname ++ "/tx"

                htz <- clkSpeed
                -- right now, we pay no attention to the speed
                -- sending everything at full speed

                CHAN rs_out rs_in :: CHAN Spartan3eClock U8 <- channel

                spark $ do
                        lab <- STEP
                        -- insert pauses for speed /slowdown here
                        takeBus bus rs_in $ GOTO lab


                let ss = [ case s of
                            Nothing -> Nothing
                            Just Nothing -> Nothing
                            Just (Just c) -> Just c
                         | s <- fromS rs_out
                         ]

                Spartan3eSimulator $ lift $ do
                        writeSocketSimulator
                                ("dev/" ++ portname)
                                tx
                                ss
                return ()

        probe nm ss = Spartan3eSimulator $ lift $ do
                outSimulatorDebugging $ fmap toUnit $ fromS $ probeS nm ss
           where
                toUnit _ = ()   -- why does this work?

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

-------------------------------------------------------------
-- data structures
-------------------------------------------------------------

data Serial = DCE | DTE deriving (Eq, Ord, Show)

serialName :: Serial -> String
serialName DCE = "dce"
serialName DTE = "dte"

data Dial = Dial Bool U2

deriving instance Eq Dial
deriving instance Show Dial

-------------------------------------------------------------
-- ASCII board simulator
-------------------------------------------------------------


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
	= LED (Sized 8) (Maybe Bool)
        | INPUT Input Bool
--	| TOGGLE (Sized 4) Bool
        | CLOCK Integer
        | LCD (Sized 2,Sized 16) Char
        | BOARD
--        | BUTTON (Sized 4) Bool
        | DIAL Dial
        | RS232 DIR Serial Integer
        | DEBUG

deriving instance Eq Output
deriving instance Show Output

data Input
        = TOGGLE (Sized 4)
        | BUTTON (Sized 4)
        | DIAL_BUTTON
        | DIAL_TURN LR
        | QUIT


deriving instance Eq Input
deriving instance Show Input

data LR = LEFT | RIGHT
        deriving (Show,Eq,Ord)

data DIR  = RX | TX
        deriving (Show,Eq,Ord)

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

 drawGraphic (INPUT (TOGGLE x) b) = do
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
        PRINT boardASCII `at` (1,0)
        COLOR Red $ PRINT ['o'] `at` (2,4)
 drawGraphic (INPUT (BUTTON x) b) =
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
 drawGraphic (DEBUG) = return () -- perhaps flash a light?

----------------------------------------------------------------------------------
-- local tests
--main :: (W (Vector 32 Bool) ~ X32, W U32 ~ X32) => IO ()
main = do
        let fab = do
                let f x = if x then high else low

                VAR count :: VAR Spartan3eClock U32 <- initially 0

                let count' :: Signal Spartan3eClock (Vector 32 Bool)  = bitwise (count :: Signal Spartan3eClock U32)

                let m      ::  Vector 32 (Signal Spartan3eClock Bool) = unpack count'

                spark $ do
                        lab <- STEP
                        count := count + 1
                        GOTO lab

                sw <- switches

                buttons
                dial

                leds $ M.forAll $ \ i -> if i < 4 then sw ! (fromIntegral i)
                                                  else m ! (fromIntegral i + 4)

{-
                BUS lcd_bus lcd_wtr_bus :: BUS Spartan3eClock ((Sized 2,Sized 16),U8) <- bus

                let s = map (fromIntegral . ord) "Hello, Spartan3!"
                spark $ do
                        sequence_
                          [ putBus lcd_wtr_bus (pureS ((x,y),s !! fromIntegral y)) $ STEP
                          | x <- [0,1], y <- [0..15]
                          ]
                        return ()
                lcd lcd_bus


                rs232_out <- rs232rx DCE 9600

                probe "rs232_out" rs232_out -- (pack sw :: Seq (Vector4 Bool))

                rs_bus <- latchBus rs232_out

                BUS rs232_bus rs232_wrt_bus :: BUS Spartan3eClock U8 <- bus

                rs232tx DCE 9600 rs_bus

                spark $ sequence_
                        [ putBus rs232_wrt_bus (pureS (fromIntegral (Char.ord ch))) $ STEP
                        | ch <- "Hello, World\n"
                        ]
-}
                return () :: Spartan3eSimulator ()
        runSpartan3eSimulator fab

fab1 :: (Spartan3e m) => m ()
fab1 = do

        sw <- switches
        buttons
        dial

        leds $ M.forAll $ \ i -> if i < 4 then sw ! (fromIntegral i)
                                          else high -- m ! (fromIntegral i + 4)

data ConnectM i o a = ConnectM { runConnectM :: i -> (a,o -> o) }

readConnectM :: ConnectM i o i
readConnectM = ConnectM $ \ i -> (i,id)

writeConnectM :: (o -> o) -> ConnectM i o ()
writeConnectM u = ConnectM $ \ i -> ((),u)

instance Monad (ConnectM i o) where
        return a = ConnectM $ \ _ -> (a,id)
        (ConnectM m) >>= k = ConnectM $ \ i ->
                let (a,w1) = m i
                    (r,w2) = runConnectM (k a) i
                in (r,w1 . w2)

instance MonadFix (ConnectM i o)  where
        mfix f = ConnectM $ \ i ->
                   let (a,w) = runConnectM (f a) i
                   in (a,w)


data Spartan3eInput = Spartan3eInput
        { input_buttons :: Vector 4 (Signal Spartan3eClock Bool)
        }

data Spartan3eOutput = Spartan3eOutput
        { output_leds :: Vector 8 (Signal Spartan3eClock Bool)
        }

data Spartan3eSimulator' a = Spartan3eSimulator'
        { unSpartan3eSimulator' :: SuperFabric Spartan3eClock (Simulator2 Input Output) a }

instance Monad Spartan3eSimulator' where
        return a = Spartan3eSimulator' (return a)
        (Spartan3eSimulator' m) >>= k = Spartan3eSimulator' $ do
                                v <- m
                                unSpartan3eSimulator' (k v)

instance MonadFix Spartan3eSimulator' where
        mfix f = Spartan3eSimulator' $ do
                        rec a <- unSpartan3eSimulator' (f a)
                        return a

instance LocalM Spartan3eSimulator' where
        type LocalClock Spartan3eSimulator' = Spartan3eClock
        newSignalVar           = Spartan3eSimulator' $ newSignalVar
        writeSignalVar var sig = Spartan3eSimulator' $ writeSignalVar var sig
        readSignalVar  var fn  = Spartan3eSimulator' $ readSignalVar var fn

instance Spartan3e Spartan3eSimulator' where
        clkSpeed = return $ clockRate

        leds mat = Spartan3eSimulator' $ lift $ sequence_
                [ simOutput (changed $  fmap (LED x) $ shallowS light)
                | (x,light) <- assocs mat
                ]

        switches = Spartan3eSimulator' $ lift $ do
                m <- sequence
                          [ do ss0 <- simInput (TOGGLE i `elem`)
                               let ss1 = flipper $ fmap tick ss0
                               simOutput (changed $ fmap (INPUT $ TOGGLE i) $ ss1)
                               return $ mkShallowS $ fmap pure $ ss1
                          | i <- [0..3]
                          ]
                return $ matrix m

        buttons = Spartan3eSimulator' $ lift $ do
                m <- sequence
                          [ do ss0 <- simInput (BUTTON i `elem`)
                               let ss1 = flipper $ fmap tick ss0
                               simOutput (changed $ fmap (INPUT $ BUTTON i) $ ss1)
                               return $ mkShallowS $ fmap pure $ ss1
                          | i <- [0..3]
                          ]
                return $ matrix m

        dial = Spartan3eSimulator' $ lift $ do
                bs0 <- simInput (DIAL_BUTTON     `elem`)
                ls0 <- simInput (DIAL_TURN LEFT  `elem`)
                rs0 <- simInput (DIAL_TURN RIGHT `elem`)
                let bs1 = flipper $ fmap tick bs0

                let f False False st = st
                    f True  True  st = st
                    f True  False st = pred st
                    f False True  st = succ st

                let dirs0 = S.zipWith3 f
                                   ls0
                                   rs0
                                   (0 `S.cons` dirs0)

                simOutput $ changed $ fmap DIAL $ S.zipWith Dial bs1 dirs0

                let delta :: Stream U2 -> Stream (Enabled Bool)
                    delta xs = Nothing `S.cons` S.zipWith (\ a b -> f (a - b))
                                                          xs
                                                          (S.tail xs)

                    f 0 = Nothing
                    f 1 = Just True
                    f 2 = Nothing       -- should never happen
                    f 3 = Just False

                return ( mkShallowS
                             $ fmap pure
                             $ bs1
                       , mkShallowS
                             $ fmap pure
                             $ delta
                             $ dirs0
                       )

--        lcd :: Bus (LocalClock m) ((Sized 2,Sized 16),U8) -> m ()
{-
        lcd _ bus = do
                CHAN lcd_out lcd_in :: CHAN Spartan3eClock ((Sized 2,Sized 16),U8) <- channel
                spark $ do
                        lab <- STEP
                        takeBus bus lcd_in $ GOTO lab

                let ss :: [Maybe (Enabled ((Sized 2,Sized 16),U8))] = fromS lcd_out

                Spartan3eSimulator' $ lift $ simOutput
                        $ fmap (\ ss -> case ss of
                            Nothing -> Nothing -- should not happen; consider X'ing the LCD
                            Just Nothing -> Nothing
                            Just (Just ((x,y),ch)) -> Just (LCD (x,y) (Char.chr (fromIntegral ch))))
                        $ S.fromList
                        $ fromS
                        $ lcd_out

                return ()
-}

{-
        buttons = Spartan3eSimulator $ lift $ do
                let sw i ch old | key ! i == ch = not old       -- flip
                                | otherwise     = old           -- leave

                    key :: Vector 4 Char
                    key = matrix "aegx"

                m <- sequence
                          [ do ss <- inSimulator False (sw i)
                               outSimulator (BUTTON i) ss
                               return $ mkShallowS $ fmap pureX $ ss
                          | i <- [0..3]
                          ]
                return $ matrix m
-}
{-
class Simulator' (m :: * -> *) where
        readKey  :: Signal CLK (Maybe Char)
        readPort :: String -> m (Signal CLK (Maybe U8))
        writePort :: String -> Signal CLK (Maybe U8) -> m ()

--        writePort :: ()

runSpartan3eSimulator' :: (Simulator' m) => (forall m. (Spartan3e m) => m ()) -> m ()
runSpartan3eSimulator' = undefined
-}

-----------------------------------------------------------------------

instance SimulatorInput Input where
        simKeyboard 'l' = [TOGGLE 0]
        simKeyboard 'k' = [TOGGLE 1]
        simKeyboard 'j' = [TOGGLE 2]
        simKeyboard 'h' = [TOGGLE 3]
        simKeyboard 'a' = [BUTTON 0]
        simKeyboard 'e' = [BUTTON 1]
        simKeyboard 'g' = [BUTTON 2]
        simKeyboard 'x' = [BUTTON 3]
        simKeyboard 'd' = [DIAL_BUTTON]
        simKeyboard 's' = [DIAL_TURN LEFT]
        simKeyboard 'f' = [DIAL_TURN RIGHT]
        simKeyboard 'q' = error "abort simulator"
        simKeyboard _   = []
        simRead _ _ = []

instance SimulatorOutput Output where
        simBackground = BOARD
        simTerminal = drawGraphic
        simWrite = const Nothing


runSpartan3eSimulator2 :: Spartan3eSimulator' () -> Simulator2 Input Output ()
runSpartan3eSimulator2 (Spartan3eSimulator' m) = do
        ((),_) <- runFabric m []
        return ()


main2 = runSimulator2 P.Friendly 100 100 $ runSpartan3eSimulator2 fab1

