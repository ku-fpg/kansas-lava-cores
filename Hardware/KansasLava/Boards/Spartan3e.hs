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
import Data.Maybe
import Data.Array.IArray
import GHC.TypeLits
import Control.Monad.IO.Class
import Data.Word
import Control.Monad
import Data.Boolean
import Data.Monoid
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
import Data.Sized.Fin

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

class (LocalM m, LocalClock m ~ Spartan3eClock) => Spartan3e m where
        clkSpeed ::                             m Integer -- Hz
        leds     :: Vector 8 (Signal (LocalClock m) Bool)
                                             -> m ()
        switches ::                             m (Vector 4 (Signal (LocalClock m) Bool))
        buttons  ::                             m (Vector 4 (Signal (LocalClock m)  Bool))
        dial     ::                             m ( Signal (LocalClock m)  Bool
                                                  , Signal (LocalClock m)  (Enabled Bool)
                                                  )
        lcd      :: Int -> Bus (LocalClock m) ((Fin 2,Fin 16),U8) -> m ()

        rs232    :: Serial -> Int -> Bus (LocalClock m) U8      -> m (Bus (LocalClock m) U8)
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

type instance (2 + 16) = 18
type instance (18 + 8) = 26
type instance (1 + 26) = 27
type instance (1 + 32) = 33
type instance (1 + 16) = 17
type instance (1 + 1)  = 2

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
	= LED (Fin 8) (Maybe Bool)
        | INPUT Input Bool
        | CLOCK Integer
        | LCD (Fin 2,Fin 16) Char
        | BOARD
        | DIAL Dial
        | RS232 DIR Serial Integer      -- displaying counts
        | RS232_TX Serial U8
        | RS232_INIT Serial Int        -- initalialize the RS232
        | PROBE

deriving instance Eq Output
deriving instance Show Output

data Input
        = TOGGLE (Fin 4)
        | BUTTON (Fin 4)
        | DIAL_BUTTON
        | DIAL_TURN LR
        | QUIT
        | RS232_RX Serial U8

deriving instance Eq Input
deriving instance Show Input

data LR = LEFT | RIGHT
        deriving (Show,Eq,Ord)

data DIR  = RX | TX
        deriving (Show,Eq,Ord)

at = AT



data Spartan3eSimulator a = Spartan3eSimulator
        { unSpartan3eSimulator :: SuperFabric Spartan3eClock (Simulator Input Output) a }

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

instance Spartan3e Spartan3eSimulator where
        clkSpeed = return $ clockRate

        leds mat = Spartan3eSimulator $ lift $ sequence_
                [ simOutput (changed $ fmap (LED x) $ shallowS light)
                | (x,light) <- assocs mat
                ]

        switches = Spartan3eSimulator $ lift $ do
                m <- sequence
                          [ do ss0 <- simInput (TOGGLE i `elem`)
                               let ss1 = flipper $ fmap tick ss0
                               simOutput (changed $ fmap (INPUT $ TOGGLE i) $ ss1)
                               return $ mkShallowS $ fmap pure $ ss1
                          | i <- [0..3]
                          ]
                return $ matrix m

        buttons = Spartan3eSimulator $ lift $ do
                m <- sequence
                          [ do ss0 <- simInput (BUTTON i `elem`)
                               let ss1 = flipper $ fmap tick ss0
                               simOutput (changed $ fmap (INPUT $ BUTTON i) $ ss1)
                               return $ mkShallowS $ fmap pure $ ss1
                          | i <- [0..3]
                          ]
                return $ matrix m

        dial = Spartan3eSimulator $ lift $ do
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

        lcd wait bus = do
                CHAN lcd_out lcd_in :: CHAN Spartan3eClock ((Fin 2,Fin 16),U8) <- channel

                VAR count :: VAR Spartan3eClock U16 <- initially 0

                spark $ do
                        start <- STEP
                        count := pureS (fromIntegral wait)
                        loop <- STEP
                        count := count - 1
                        (count /=* (0 :: Signal Spartan3eClock U16)) :? GOTO loop
                        takeBus bus lcd_in $ GOTO start

                let ss :: [Maybe (Enabled ((Fin 2,Fin 16),U8))] = fromS lcd_out

                Spartan3eSimulator $ lift $ simOutput
                        $ fmap (\ ss -> case ss of
                            Nothing      -> Nothing -- should not happen; consider X'ing the LCD
                            Just Nothing -> Nothing
                            Just (Just ((x,y),ch)) -> Just (LCD (x,y) (Char.chr (fromIntegral ch))))
                        $ S.fromList
                        $ fromS
                        $ lcd_out

                return ()

        rs232 port baud bus = do

                htz <- clkSpeed

                let slowdown = (10 * fromIntegral htz) `div` baud

                -- Now, the TX
                -- right now, we pay no attention to the speed
                -- sending everything at full speed

                VAR w :: VAR Spartan3eClock Int <- initially 0

                CHAN rs_out rs_in :: CHAN Spartan3eClock U8 <- channel

                let sig :: forall c s . Signal c s -> Signal c s
                    sig s = s

                spark $ do
                        lab <- STEP
                        -- insert pauses for speed /slowdown here
                        w := pureS slowdown
                        lab2 <- STEP
                        w := w - 1
                        (sig w /=* 0) :? GOTO lab2
                        takeBus bus rs_in $ GOTO lab

                let ss = id
                       $ fmap join
                       $ (shallowS rs_out :: Stream (Maybe (Enabled U8)))


                -- Should really warn about invalid input to 232
                Spartan3eSimulator $ lift $ do
                        simOutput
                           $ fmap (fmap (RS232_TX port))
                           $ ss

                Spartan3eSimulator $ lift $ do
                        simOutput
                           $ fmap (fmap (RS232 TX port))
                           $ count
                           $ fmap (fmap (const ()))
                           $ ss

                -- Now, the RX

                Spartan3eSimulator $ lift $ do
                        simState $ once (RS232_INIT port $ slowdown)

                xs :: Stream (Maybe U8) <- Spartan3eSimulator $ lift $ do
                        simInput (\ xs -> case [ u8 | RS232_RX port' u8 <- xs, port' == port ] of
                                           []     -> Nothing
                                           (u8:_) -> Just u8)

                Spartan3eSimulator $ lift $ do
                        simOutput
                                $ fmap (fmap (RS232 RX port))
                                $ count
                                $ fmap (fmap (const ()))
                                $ xs

                latchBus (mkShallowS $ fmap pure xs)

        probe nm ss = do
                Spartan3eSimulator $ lift $ do
                        simOutput $ fmap toUnit $ shallowS $ probeS nm ss
                return ()
           where
                toUnit _ = return (PROBE)   -- why does this work?

-----------------------------------------------------------------------


runSpartan3eSimulator :: Spartan3eSimulator () -> Simulator Input Output ()
runSpartan3eSimulator (Spartan3eSimulator m) = do
        ((),_) <- runFabric m []
        return ()

instance Simulation Spartan3eSimulator where
        type SimulationInput Spartan3eSimulator = Input
        type SimulationOutput Spartan3eSimulator = Output
        simulation (Spartan3eSimulator m) = do
                (a,_) <- runFabric m []
                return a

------------------------------------------------

type Spartan3eDevice = Device Input Output

keyboard :: Spartan3eDevice
keyboard = keyboardInput $ \ c -> case c of
       'l' -> [TOGGLE 0]
       'k' -> [TOGGLE 1]
       'j' -> [TOGGLE 2]
       'h' -> [TOGGLE 3]
       'a' -> [BUTTON 0]
       'e' -> [BUTTON 1]
       'g' -> [BUTTON 2]
       'x' -> [BUTTON 3]
       'd' -> [DIAL_BUTTON]
       's' -> [DIAL_TURN LEFT]
       'f' -> [DIAL_TURN RIGHT]
       'q' -> error "abort simulator"
       _   -> []

ansi :: Spartan3eDevice
ansi = ansiOutput (do { CLEAR ; drawGraphic BOARD}) drawGraphic where
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
 drawGraphic (PROBE) = return () -- perhaps flash a light?
 drawGraphic (RS232_INIT {}) = return ()
 drawGraphic _ = return ()

{-
socketDevice :: ([o] -> Maybe Int)      -- speed
             -> ([o] -> Maybe Word8)    -- char to circuit (RX)
             -> (Word8 -> [i])          -- char from circuit (TX)
             -> String                  -- device filename
             -> Device i o
-}

rs232Device :: Device Input Output
rs232Device =
        let port = DCE
            speed xs = listToMaybe [ n | (RS232_INIT port' n) <- xs, port == port' ]
            name = serialName port
            rx xs = listToMaybe [ fromIntegral n | (RS232_TX port' n) <- xs, port == port' ]
            tx w8 = [RS232_RX port (fromIntegral w8)]
        in
            socketDevice speed rx tx name



