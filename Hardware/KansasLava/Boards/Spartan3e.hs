{-# LANGUAGE ScopedTypeVariables,TypeFamilies, FlexibleContexts #-}

module Hardware.KansasLava.Boards.Spartan3e (
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


import Language.KansasLava as KL
import Hardware.KansasLava.LCD.ST7066U
import Hardware.KansasLava.RS232
import Hardware.KansasLava.Rate
import Hardware.KansasLava.Boards.UCF
import Hardware.KansasLava.Peripherals
import Hardware.KansasLava.Core
import Hardware.KansasLava.Boards.Physical

import Data.Sized.Unsigned
import Data.Sized.Ix hiding (all)
import Data.Sized.Matrix hiding (all)
import Data.Char
import System.IO
import Control.Applicative
import Control.Monad.Fix

------------------------------------------------------------
-- The Spartan3e Monad
------------------------------------------------------------

data Spartan3e a = Spartan3e (STMT a)

instance Monad Spartan3e where
        return = Spartan3e . return 
        (Spartan3e m) >>= k = Spartan3e $ do
                                m >>= \ r -> let (Spartan3e o) = k r in o

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


instance DialRotation Spartan3e where
        dialRotation = do
            core "dial" $ do
                (reg :: REG Bool,ev :: EXPR (Maybe Bool)) <- mkEnabled
                
                a :: EXPR Bool <- INPUT (inStdLogic "ROT_A")
                b :: EXPR Bool <- INPUT (inStdLogic "ROT_B")

                -- These are a and b on the previous cycle
                VAR a' <- SIGNAL $ var False
                VAR b' <- SIGNAL $ var False
                
                always $ a' := a
                always $ b' := b

                SPARK $ \ loop -> do
                        -- wait for both to be true
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
                let patch = probeAckBoxP "in1" $$ mm_LCD_Inst $$ probeAckBoxP "in2" $$ init_LCD $$ probeAckBoxP "in3" $$ phy_Inst_4bit_LCD

                core ("lcd") $ do

--RS, SF_D[11:8], LCD_E
-- assuming LCD_RW is set always low
                        
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
                        (a_r,b_e,c_e,d_r) <- PATCH (patchLhsAck $$ patch $$ patchRhsUnit $$ patchRhsEnabled)

                        always $
                                rs := OP1 enabledVal b_e
{-

                        ([i0],[o0,o1]) <- GENERIC $ do
                                a <- inStdLogicVector "i0" :: Fabric (Seq (Enabled ((X2,X16),U8)))
                                let (b,c) = id (a,packEnabled high (pureS ()))
                                outStdLogicVector "o0" (probeS "o0" (b :: Seq (Enabled ((X2,X16),U8))))
                                outStdLogic "o1" (c :: Seq (Enabled ()))

--                        (r,w) <- newAckBox 
-}
--                        return $ WriteAckBox (R i0) (OP0 (packEnabled high $ pureS ()))
                                
                        return $ WriteAckBox a_r c_e


instance Monitor Spartan3e where
        monitor probename = monitor' where 
          --- Too allow the forall type to work with the instance 
          monitor' :: forall a . (Rep a, Size (W (Enabled a))) => Spartan3e (REG a)
          monitor' = do
            core "monitor" $ do
                OUTPUT (\ a -> outStdLogicVector ("monitor_" ++ probename) (probeS probename a :: Seq (Enabled a)))


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

data Serial = DCE | DTE deriving (Eq, Ord, Show)
 