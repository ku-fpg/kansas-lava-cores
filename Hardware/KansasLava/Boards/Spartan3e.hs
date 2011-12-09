{-# LANGUAGE ScopedTypeVariables,TypeFamilies, FlexibleContexts #-}

module Hardware.KansasLava.Boards.Spartan3e (
        -- * The Spartan3e Monad
          Spartan3e(..)
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
                (reg :: REG Bool,ev :: EVENT Bool) <- mkEnabled
                
                a :: EXPR Bool <- INPUT (inStdLogic "ROT_A")
                b :: EXPR Bool <- INPUT (inStdLogic "ROT_B")

                -- These are a and b on the previous cycle
                VAR a' <- SIGNAL $ var False
                VAR b' <- SIGNAL $ var False
                
                always $ a' := a
                always $ b' := b

                SPARK $ \ loop -> do
                        -- wait for both to be true
                        ((OP1 bitNot $ OP2 and2 a b) :? GOTO loop)
                                ||| a' :? reg := OP0 high
                                ||| b' :? reg := OP0 low
                        wait <- LABEL
                        ((OP2 or2 a b) :? GOTO wait)
                           ||| GOTO loop


                return $ ev
        dialButton   = return $ error "dialButton"
        
instance LEDs Spartan3e where
        type LEDCount Spartan3e = X8
        ledNames = return $ matrix ["LED<" ++ show i ++ ">" | i <- [0..maxBound :: X8]]
        
instance Switches Spartan3e where
        type SwitchCount Spartan3e = X4
        switchNames = return $ matrix ["SW<" ++ show i ++ ">" | i <- [0..maxBound :: X4]]

instance Buttons Spartan3e where
        type ButtonCount Spartan3e = X4
        buttonNames = return $ matrix ["BTN_NORTH","BTN_EAST","BTN_SOUTH","BTN_WEST"]

instance RS232 Spartan3e where
        type RS232Count Spartan3e = Serial

instance LCD Spartan3e where
        type LCDSize Spartan3e = (X2,X16)


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
 