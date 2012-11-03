{-# LANGUAGE ScopedTypeVariables,TypeFamilies, FlexibleContexts, RecursiveDo, DoRec #-}

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
import Language.KansasLava.Universal
import Hardware.KansasLava.Simulators.Polyester as P
import System.Console.ANSI

{-

-}
--import Hardware.KansasLava.LCD.ST7066U
--import Hardware.KansasLava.RS232
--import Hardware.KansasLava.Rate
--import Hardware.KansasLava.Boards.UCF
--import Hardware.KansasLava.Peripherals
--import Hardware.KansasLava.Core
--import Hardware.KansasLava.Boards.Physical

import Control.Monad.Trans.Class
import Data.Sized.Unsigned
import Data.Sized.Ix hiding (all)
import Data.Sized.Matrix hiding (all)
import qualified Data.Sized.Matrix as M
import Data.Char
import System.IO
import Control.Applicative
import Control.Monad.Fix

------------------------------------------------------------
-- The Spartan3e Class
------------------------------------------------------------

class (SparkM m, Monad m) => Spartan3e m where
        clkSpeed :: m Integer           -- Hz
        leds     :: Matrix X8 (Seq Bool) -> m ()
        switches :: m (Matrix X4 (Seq Bool))
        buttons  :: m (Matrix X4 (Seq Bool))
        dial     :: m (Seq Bool, Seq (Enabled Bool))

------------------------------------------------------------
-- The Spartan3e Board Monad
------------------------------------------------------------

data Spartan3eBoard a = Spartan3eBoard
        { unSpartan3eBoard :: Integer -> Fabric a }

instance Monad Spartan3eBoard where
        return a = Spartan3eBoard $ \ _ -> return a
        (Spartan3eBoard m) >>= k = Spartan3eBoard $ \ env -> do
                                r <- m env
                                unSpartan3eBoard (k r) env

instance MonadFix Spartan3eBoard where
        mfix f = Spartan3eBoard $ \ env -> do
                        rec a <- unSpartan3eBoard (f a) env
                        return a

instance SparkM Spartan3eBoard where
        newSignalVar           = Spartan3eBoard $ \ _ -> newSignalVar
        writeSignalVar var sig = Spartan3eBoard $ \ _ -> writeSignalVar var sig
        readSignalVar  var fn  = Spartan3eBoard $ \ _ -> readSignalVar var fn

instance Spartan3e Spartan3eBoard where
        clkSpeed = Spartan3eBoard $ \ i -> return i


-- The Spartan3e Simulator Monad
------------------------------------------------------------


data Spartan3eSimulator a = Spartan3eSimulator
        { unSpartan3eSimulator :: SuperFabric Polyester a }

instance Monad Spartan3eSimulator where
        return a = Spartan3eSimulator (return a)
        (Spartan3eSimulator m) >>= k = Spartan3eSimulator $ do
                                v <- m
                                unSpartan3eSimulator (k v)


instance MonadFix Spartan3eSimulator where
        mfix f = Spartan3eSimulator $ do
                        rec a <- unSpartan3eSimulator (f a)
                        return a


instance SparkM Spartan3eSimulator where
        newSignalVar           = Spartan3eSimulator $ newSignalVar
        writeSignalVar var sig = Spartan3eSimulator $ writeSignalVar var sig
        readSignalVar  var fn  = Spartan3eSimulator $ readSignalVar var fn


-- runFabric :: (MonadFix m) => SuperFabric m a -> [(String,Pad)] -> m (a,[(String,Pad)])

instance Simulator Spartan3eSimulator where
        runPolyester (Spartan3eSimulator m) = do
                outPolyester (\ _ -> BOARD) [()]
                ((),_) <- runFabric m []
                return ()

instance Spartan3e Spartan3eSimulator where
        clkSpeed = Spartan3eSimulator $ lift getPolyesterClkSpeed
        leds mat = Spartan3eSimulator $ lift $ sequence_
                [ outPolyester (LED x) $ fromS light
                | (x,light) <- assocs mat
                ]

        switches = Spartan3eSimulator $ lift $ do
                let sw i ch old | key ! i == ch = not old       -- flip
                                | otherwise     = old           -- leave

                    key :: Matrix X4 Char
                    key = matrix "lkjh"

                m <- sequence
                          [ do ss <- inPolyester False (sw i)
                               outPolyester (TOGGLE i) ss
                               return $ toS ss
                          | i <- [0..3]
                          ]
                return $ matrix m

        buttons = Spartan3eSimulator $ lift $ do
                let sw i ch old | key ! i == ch = not old       -- flip
                                | otherwise     = old           -- leave

                    key :: Matrix X4 Char
                    key = matrix "aegx"

                m <- sequence
                          [ do ss <- inPolyester False (sw i)
                               outPolyester (BUTTON i) ss
                               return $ toS ss
                          | i <- [0..3]
                          ]
                return $ matrix m

        dial =  Spartan3eSimulator $ lift $ do
                let switch 'd' (Dial b p) = Dial (not b) p
                    switch 's' (Dial b p) = Dial b (pred p)
                    switch 'f' (Dial b p) = Dial b (succ p)
                    switch _   other      = other

                ss <- inPolyester (Dial False 0) switch
                outPolyester DIAL ss

                let delta :: [U2] -> [Enabled Bool]
                    delta xs = Nothing : [ f (a - b) | a <- xs, b <- tail xs ]

                    f 0 = Nothing
                    f 1 = Just True
                    f 2 = Nothing       -- should never happen
                    f 3 = Just False


                return ( toS $ map (\ (Dial b _) -> b) $ ss
                       , toS $ delta
                             $ map (\ (Dial _ p) -> p)
                             $ ss
                       )


{-

-}









--                 LED X8 (Maybe Bool)


--                return

--                mode <- getPolyesterExecMode
--                when (mode /= Fast) $ do
--                        outPolyester (clock) [0..]
--                return ()

{-

instance CoreMonad Spartan3e where
  core _ m = Spartan3e m

instance PhysicalMonad Spartan3e where
  run_physical (Spartan3e m) = m
  clk_physical = return "CLK_50MHZ"      -- wire the Spartan3e clock

-- | Backdoor to run the monad. Used by the simulator to share solutions.
runSpartan3e :: Spartan3e a -> STMT a
runSpartan3e (Spartan3e m) = m

------------------------------------------------------------
-- The Spartan3e classes and instances
------------------------------------------------------------

-- This abstraction is at the STMT level.
class (CoreMonad fab) => DialRotation fab where
        -- | 'dialRotation' gives Enabled packets when dial is rotated,
        -- and is Enabled True if the rotation was clockwise
        dialRotation :: fab (EXPR (Enabled Bool))
        dialButton   :: fab (EXPR Bool)


waitFor :: EXPR U32 -> STMT ()
waitFor n = do
        VAR i :: VAR U32 <- SIGNAL $ var 0
        i := OP1 (\ x -> x - 2) n
        loop <- LABEL
        i := i - 1
                ||| (OP2 (.>.) i 0) :? GOTO loop

-- add to a class at some point
debounce :: forall sample count . (Size sample, Size count) => Witness sample -> Witness count -> EXPR Bool -> STMT (EXPR Bool)
debounce sample count expr = do
        pulse      :: EXPR (Enabled ())    <- INPUT (return $ packEnabled (powerOfTwoRate sample) (pureS ()))
        VAR n      :: VAR (Unsigned count) <- SIGNAL $ var 0
        VAR result :: VAR Bool             <- SIGNAL $ var False

        let waitForPulse = do
                loop <- LABEL
                OP1 (bitNot . isEnabled) pulse :? GOTO loop

        let wait p = do
                n := 0
                loop <- LABEL
                waitForPulse
                IF p (do
                        n := n + 1
                  )(do
                        n := 0
                  )
                (OP2 (./=.) n (fromIntegral (maxBound :: Unsigned count))) :? GOTO loop

        SPARK $ \ start -> do
                wait expr
                result := OP0 high
                wait (OP1 bitNot expr)
                result := OP0 low
                GOTO start

        return $ result


str1 =         "............****************************************...........##....#...#...........................**************************************************......................................"
test = do
        inp <- INPUT (return $ toS $ map (== '*') $ str1)
        out <- debounce (Witness :: Witness X1) (Witness :: Witness X3) inp
        r <- OUTPUT (outStdLogic "o0" . enabledVal)
        always $ r := out

{-
test0 :: Pad
test0 = case runFabric (compileToFabric test) [] of
          (_,[(_,n)])-> n

str2 = take 200 [ if x then '*' else '.' | (Just x) <- fromS $ fromUni' test0 ]
-}

instance DialRotation Spartan3e where
        dialRotation = do
            core "dial" $ do
                (reg :: REG Bool,ev :: EXPR (Maybe Bool)) <- mkEnabled

                aI :: EXPR Bool <- INPUT (inStdLogic "ROT_A")
                bI :: EXPR Bool <- INPUT (inStdLogic "ROT_B")

                a <- debounce (Witness :: Witness X15) (Witness :: Witness X4) aI
                b <- debounce (Witness :: Witness X15) (Witness :: Witness X4) bI

                -- These are a and b on the previous cycle
                VAR a' <- SIGNAL $ var False
                VAR b' <- SIGNAL $ var False

                always $ a' := a
                always $ b' := b

                SPARK $ \ loop -> do
                        -- wait for both to be false (the diagram in manual is backwards!)
                        ((OP2 or2 a b) :? GOTO loop)
                                ||| a' :? reg := OP0 high
                                ||| b' :? reg := OP0 low
                        wait <- LABEL
                        -- now wait for both to go high again
                        ((OP1 bitNot $ OP2 or2 a b) :? GOTO wait)
                           ||| GOTO loop


                return $ ev
        dialButton   = return $ error "dialButton"

instance LEDs Spartan3e where
        type LEDCount Spartan3e = X8
        ledNames = return $ matrix ["LED<" ++ show i ++ ">" | i <- [0..maxBound :: LEDCount Spartan3e]]

instance Switches Spartan3e where
        type SwitchCount Spartan3e = X4
        switchNames = return $ matrix ["SW<" ++ show i ++ ">" | i <- [0..maxBound :: SwitchCount Spartan3e]]

instance Buttons Spartan3e where
        type ButtonCount Spartan3e = X4
        buttonNames = return $ matrix ["BTN_NORTH","BTN_EAST","BTN_SOUTH","BTN_WEST"]

instance RS232 Spartan3e where
        type RS232Count Spartan3e = Serial
--        rs232rx :: (RS232Count fab ~ x) => x -> Int -> fab (EXPR (Enabled U8))

        rs232rx port speed = do
                let portname = show port

                let patch = rs232in (fromIntegral speed) clockRate


                core ("rs232rx/" ++ portname) $ do
                        rs_rx :: EXPR Bool <- INPUT (inStdLogic $ "RS232_" ++ show port ++ "_RX")
                        (a_r,b_e,c_e,d_r) <- PATCH (patchLhsEnabled $$ patchLhsUnit $$ patch $$ patchRhsUnit)

                        always $
                                a_r := rs_rx

                        return b_e

instance LCD Spartan3e where
        type LCDSize Spartan3e = (X2,X16)

        lcd = do
                core ("lcd") $ do

                        rs   :: REG (U1,U4,Bool) <- OUTPUT $ \ out -> do
                                let (rs :: Seq U1,sf :: Seq U4,e :: Seq Bool) = unpack (enabledVal out)
                                let sf' :: Matrix X4 (Seq Bool) = unpack (bitwise sf :: Seq (Matrix X4 Bool))
                                let gate :: Seq Bool -> Seq Bool
                                    gate = registerEnabled False . packEnabled (isEnabled out)
                                outStdLogic "LCD_RS"   $ gate ((bitwise) rs)
                                outStdLogic "SF_D<11>" $ gate (sf' ! 3)
                                outStdLogic "SF_D<10>" $ gate (sf' ! 2)
                                outStdLogic "SF_D<9>"  $ gate (sf' ! 1)
                                outStdLogic "SF_D<8>"  $ gate (sf' ! 0)
                                outStdLogic "LCD_E"    $ gate e
                                outStdLogic "LCD_RW"   low
                                outStdLogic "SF_CE0"   high
                                return ()

                        -- and call the controller
                        st7066U_controller rs


instance Monitor Spartan3e where
        monitor = do
                let mon :: forall a . (Rep a, Size (W (Enabled a))) => String -> STMT (REG a)
                    mon probename  = OUTPUT (\ a -> outStdLogicVector probename (probeS probename a :: Seq (Enabled a)))
                return $ MONITOR mon

-- Utils (to move)

mkPatch :: forall a b c d e f
         . (Rep a, Rep b, Rep c, Rep d)
        => (REG a -> EXPR (Enabled c) -> e)
        -> (EXPR (Enabled b) -> REG d -> f)
        -> Patch (Seq (Enabled a)) (Seq (Enabled b))
                 (Seq (Enabled c)) (Seq (Enabled d))
        -> STMT (e, f)
mkPatch wtr rdr p = do
        (a_r,b_e,c_e,d_r) <- PATCH p

        return ( wtr a_r c_e
               , rdr b_e d_r
               )


-- Strange names till the switch-over.
patchLhsAck :: (sig ~ Signal clk)
            =>  Patch (sig a)             (sig a)
                      (sig (Enabled ()))  (sig Ack)
patchLhsAck = backwardP $ \ a -> packEnabled (fromAck a) (pureS ())

patchRhsAck :: (sig ~ Signal clk)
            =>  Patch (sig a)    (sig a)
                      (sig Ack)  (sig (Enabled ()))
patchRhsAck = backwardP $ toAck . isEnabled

-- hack, for now, allways issues an ack
patchLhsUnit :: (sig ~ Signal clk)
            =>  Patch (sig a)             (sig a)
                      (sig (Enabled ()))  ()
patchLhsUnit = backwardP $ \ a -> packEnabled high (pureS ())


patchRhsUnit :: (sig ~ Signal clk)
            =>  Patch (sig a)    (sig a)
                      ()         (sig (Enabled ()))
patchRhsUnit = backwardP $ const ()

patchLhsEnabled :: (Rep a, sig ~ Signal clk)
            =>  Patch (sig (Enabled a))   (sig a)
                      (sig b)             (sig b)
patchLhsEnabled = forwardP $ enabledVal

patchRhsEnabled :: (Rep a, sig ~ Signal clk)
            =>  Patch (sig a)             (sig (Enabled a))
                      (sig b)             (sig b)
patchRhsEnabled = forwardP $ packEnabled high

{-
f a =
            g = toAck . isEnabled
        (wt :: WriteAckBox Int,rd :: ReadAckBox Int) <- patchAA
                WriteAckBox
                ReadAckBox
                (backwardP f $$ fifo1 $$ backwardP g)
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

-------------------------------------------------------------
-- data structures
-------------------------------------------------------------
-}
data Serial = DCE | DTE deriving (Eq, Ord, Show)

serialName :: Serial -> String
serialName DCE = "dce"
serialName DTE = "dte"

data Dial = Dial Bool U2
        deriving Eq

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
	= LED X8 (Maybe Bool)
	| TOGGLE X4 Bool
        | CLOCK Integer
        | LCD (X2,X16) Char
        | BOARD
        | BUTTON X4 Bool
        | DIAL Dial
        | QUIT Bool
        | RS232 DIR Serial Integer
        | DEBUG

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
        PRINT boardASCII `at` (1,0)
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
 drawGraphic (DEBUG) = return () -- perhaps flash a light?

----------------------------------------------------------------------------------
-- local tests

main = do
        let fab = do
                let f x = if x then high else low

                VAR count :: VAR U32 <- initially 0

                let count' :: Seq (Matrix X32 Bool)  = bitwise count

                let m      ::  Matrix X32 (Seq Bool) = unpack count'

                spark $ do
                        lab <- STEP
                        count := count + 1
                        GOTO lab

                sw <- switches

                buttons
                dial

                leds $ M.forAll $ \ i -> if i < 4 then sw ! (fromIntegral i)
                                                  else m ! (fromIntegral i + 4)

                return () :: Spartan3eSimulator ()
        runSimulator P.Friendly 100 100 fab






