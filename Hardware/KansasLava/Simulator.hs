{-# LANGUAGE ScopedTypeVariables, GADTs, DeriveDataTypeable, DoRec, KindSignatures, TypeFamilies #-}

module Hardware.KansasLava.Simulator
{-
        ( -- * The (abstract) Fake Fabric Monad
          Simulator -- abstract
          -- * The Simulator non-proper morphisms
        , outSimulator
        , outSimulatorEvents
        , outSimulatorCount
        , outSimulatorDebugging
        , writeSocketSimulator
        , inSimulator
        , readSocketSimulator
        , getSimulatorExecMode
        , getSimulatorClkSpeed
        , getSimulatorSimSpeed
        -- * Running the Fake Simulator
        , runSimulator
        , ExecMode(..)
        -- * Support for building fake Boards
--        , generic_init
        -- * Support for the (ANSI) Graphics
        , module Hardware.KansasLava.Simulator.ANSI
        -- * Support for streams
        , module Hardware.KansasLava.Simulator.Stream
        ) -} where

import Language.KansasLava hiding (Fast)
import Language.KansasLava.Fabric
import Language.KansasLava.Universal
import qualified Language.KansasLava.Stream as S

import Hardware.KansasLava.Simulator.ANSI
import Hardware.KansasLava.Simulator.Stream
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
-- Steping version of hGetContent, never blocks, returning
-- a stream of nothing after the end file.
-----------------------------------------------------------------------

hGetContentsStepwise :: Handle -> IO [Maybe Char]
hGetContentsStepwise h = do
        opt_ok <- try (hReady h)
        case opt_ok of
           Right ok -> do
                   out <- if ok then do
                             ch <- hGetChar h
                             return (Just ch)
                           else do
                             return Nothing
                   rest <- unsafeInterleaveIO $ hGetContentsStepwise h
                   return (out : rest)
           Left (e :: IOException) -> return (repeat Nothing)



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
simOutput o = Simulator $ \ _ -> ((),[o],[])

-- | state that a particual 'dev' is required
simDevice :: String -> Simulator i o  ()
simDevice d = Simulator $ \ _ -> ((),[],[d])

class Simulation (m :: * -> *) where
        type SimulationInput m :: *
        type SimulationOutput m :: *
        simulation :: m a -> Simulator (SimulationInput m) (SimulationOutput m) a

newtype Steps a = Steps (IO (a,Steps a))

--newtype Outputs a = Outputs (IO (a -> Outputs a))

streamToSteps :: Stream a -> Steps a
streamToSteps ss = Steps
                   $ return
                   $ case S.uncons ss of
                       (s,ss') -> (s,streamToSteps ss')

stepsToStreams :: Steps a -> IO (Stream a)
stepsToStreams (Steps m) = do
        (a,rest) <- m
        ss <- unsafeInterleaveIO $ stepsToStreams rest
        return (a `S.cons` ss)

mapSteps :: (a -> IO b) -> Steps a -> Steps b
mapSteps f (Steps m) = Steps $ do
        (a,rest) <- m
        b <- f a
        return (b,mapSteps f rest)

newtype Device i o = Device ((Steps [i] -> Steps [o]) -> (Steps [i] -> Steps [o]))

instance Monoid (Device i o) where
        mempty = Device id
        mappend (Device a) (Device b) = Device $ a . b

runDeviceSimulator :: (Simulation sim) => Device (SimulationInput sim) (SimulationOutput sim) -> sim () -> IO ()
runDeviceSimulator (Device fn) sim = do

        let input = Steps $ return ([],input)

        let simulator ins = Steps $ do

                ss <- stepsToStreams ins

                let Simulator s = simulation sim

                let ((),outs0,_) = s ss

                let transpose1 :: forall a . [Stream a] -> Stream [a]
                    transpose1 = foldr (S.zipWith (:)) (pure [])

                let (Steps m) = streamToSteps
                       $ fmap concat
                       $ fmap (fmap (maybe [] (: [])))
                       $ transpose1
                       $ outs0

                m


        let output = fn simulator input

        -- never end, just consume the output list
        let loop (Steps m) = do
                (_,rest) <- m
                loop rest

        loop output

-----------------------------------------------------------------------
-- Device builders
-----------------------------------------------------------------------

inputDevice :: IO [i] -> Device i o
inputDevice i_fn = Device $ \ fn inp -> fn (mapSteps input inp)
    where
        input xs = do
           ys <- i_fn
           return (ys ++ xs)

outputDevice :: ([o] -> IO ()) -> Device i o
outputDevice o_fn = Device $ \ fn inp -> mapSteps output (fn inp)
 where
         output xs = do
                 o_fn xs
                 return xs

-- The book device runs an IO action when building the
-- device stack, and you can use the result to customize
-- a device. Examples include the use of a Handle when
-- reading files.

bootDevice :: IO a -> (a -> Device i o) -> Device i o
bootDevice boot k = Device $ \ fn inp -> Steps $ do
        -- Run the boot effect
        a <- boot
        let Device k_dev = k a
        let Steps m = k_dev fn inp
        m

ansiOutput :: ANSI () -> (o -> ANSI ()) -> Device i o
ansiOutput start fn = mconcat
        [ bootDevice (showANSI start) $ \ () ->
          outputDevice $ \ os -> do sequence_ [ showANSI (fn o) | o <- os ]
                                    hFlush stdout
        ]

-- Lists clock number on the screen
ansiTick :: (Int,Int) -> Device i o
ansiTick pos = bootDevice (newIORef (0 :: Int)) $ \ var ->
               outputDevice $ \ _ ->
                 do n <- readIORef var
                    let n' = succ n
                    writeIORef var n'
                    showANSI $ PRINT (show n) `AT` pos

-- Slow down each clock cycle, to make the process friendly to
-- other things. The number is the suggested clock rate.
nice :: Int -> Device i o
nice n = inputDevice $ do
        threadDelay $ ((1000 * 1000) `div` n)
        return []

-- Read the keyboard, and turn the keys into board commands.
keyboardInput :: (Show i) => (Char -> [i]) -> Device i o
keyboardInput interp = inputDevice $ do
        opt_ok <- try (hReady stdin)
        case opt_ok of
           Right True -> do
                ch <- hGetChar stdin
                return (interp ch)
           Right False -> do
                return []
           Left (e :: IOException) -> do
                return []

--        -- create the virtual device directory
--        createDirectoryIfMissing True "dev"

