{-# LANGUAGE ScopedTypeVariables, GADTs, DeriveDataTypeable, RecursiveDo, KindSignatures, TypeFamilies #-}

module Hardware.KansasLava.Simulator
        ( -- * The (abstract) Fake Fabric Monad
          Simulator -- abstract
        , module Hardware.KansasLava.Simulator
        -- * Support for the (ANSI) Graphics
        , module Hardware.KansasLava.Simulator.ANSI
        -- * Support for streams
        , module Hardware.KansasLava.Simulator.Stream
        -- * Device support
        , module Hardware.KansasLava.Simulator.Device
        ) where


import Language.KansasLava hiding (Fast)
import Language.KansasLava.Fabric
import Language.KansasLava.Universal
import qualified Language.KansasLava.Stream as S

import Hardware.KansasLava.Simulator.ANSI
import Hardware.KansasLava.Simulator.Stream
import Hardware.KansasLava.Simulator.Device
import System.Console.ANSI
import Data.IORef
import System.IO
import Data.Typeable
import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.Monoid
import Control.Monad.Fix
import Data.Word
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Concurrent
import Network
import System.Directory

import Data.Sized.Unsigned



-----------------------------------------------------------------------
-- Exception Magic
-----------------------------------------------------------------------

data SimulatorException = SimulatorException String
     deriving Typeable

instance Show SimulatorException where
     show (SimulatorException msg) = msg

instance Exception SimulatorException

---------------------------------------------------------------
--data
---------------------------------------------------------------

data Simulator i o a = Simulator
        { unSimulator :: Stream [i] -> (a,[Stream (Maybe o)],[String])
        }

instance Monad (Simulator i o) where
        return a = Simulator $ \ _ -> (a,[],[])
        (Simulator m) >>= k = Simulator $ \ i ->
                let (a,o1,d1) = m i
                    (r,o2,d2) = unSimulator (k a) i
                in (r,o1 ++ o2,d1 ++ d2)

instance MonadFix (Simulator i o)  where
        mfix f = Simulator $ \ i ->
                   let (a,o,d) = unSimulator (f a) i
                   in (a,o,d)

simInput :: ([i] -> a) -> Simulator i o (Stream a)
simInput f = Simulator $ \ i -> (fmap f i,[],[])

simOutput :: Stream (Maybe o) -> Simulator i o ()
simOutput o = Simulator $ \ _ -> ((),[Nothing `S.cons` doubleRate o],[])

-- The state is used for outputing things that depend of the
-- internal state *only*, and none of the inputs.
simState :: Stream (Maybe o) -> Simulator i o ()
simState o = Simulator $ \ _ -> ((),[doubleRate o],[])

-- | state that a particual 'dev' is required
simDevice :: String -> Simulator i o  ()
simDevice d = Simulator $ \ _ -> ((),[],[d])

class Simulation (m :: * -> *) where
        type SimulationInput m :: *
        type SimulationOutput m :: *
        simulation :: m a -> Simulator (SimulationInput m) (SimulationOutput m) a

runDeviceSimulator :: (Simulation sim) => Device (SimulationInput sim) (SimulationOutput sim) -> sim () -> IO ()
runDeviceSimulator (Device fn) sim = do

        let simulator ss = rr
              where
                Simulator s = simulation sim

                ((),outs0,_) = s ss

                transpose1 :: forall a . [Stream a] -> Stream [a]
                transpose1 = foldr (S.zipWith (:)) (pure [])

                rr =     fmap concat
                       $ fmap (fmap (maybe [] (: [])))
                       $ transpose1
                       $ outs0


        let session = fn (processorToCircuit simulator)

        let circuit (Circuit m) = do
                (_,rest) <- m
                needInput rest
            needInput (NeedInput m) = do
                f <- m
                nowOutput (f [])
            nowOutput (NowOutput m) = do
                (_,rest) <- m
                circuit rest

        circuit session
