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
 