{-# LANGUAGE ScopedTypeVariables, GADTs, DeriveDataTypeable, DoRec, KindSignatures #-}

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
        , ANSI(..)
        , showANSI
        , Color(..)     -- from System.Console.ANSI
        ) -} where

import Language.KansasLava hiding (Fast)
import Language.KansasLava.Fabric
import Language.KansasLava.Universal
import Language.KansasLava.Stream (Stream)
import qualified Language.KansasLava.Stream as S

import System.Console.ANSI
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


changed :: (Eq a) => Stream a -> Stream (Maybe a)
changed xs = Just (S.head xs) `S.cons` f (S.head xs) (S.tail xs)
    where
        f x xs | x == S.head xs = Nothing `S.cons` f x (S.tail xs)
               | otherwise      = Just (S.head xs) `S.cons` f (S.head xs) (S.tail xs)

freeze :: (Eq a) => a -> Stream (Maybe a) -> Stream a
freeze a = loop a
  where
        loop n ss = case S.uncons ss of
                      (Nothing,xs) -> n `S.cons` loop n xs
                      (Just v,xs)  -> v `S.cons` loop v xs

flipper :: Stream (Maybe ()) -> Stream Bool
flipper =  loop False
  where
        loop n ss = case S.uncons ss of
                      (Nothing,xs) -> n       `S.cons` loop n xs
                      (Just _,xs)  -> (not n) `S.cons` loop (not n) xs

tick :: Bool -> Maybe ()
tick True = Just ()
tick False = Nothing

-----------------------------------------------------------------------
-- Running the Simulator
-----------------------------------------------------------------------

data ExecMode
        = Fast          -- ^ run as fast as possible, and do not display the clock
        | Friendly      -- ^ run in friendly mode, with 'threadDelay' to run slower, to be CPU friendly.
  deriving (Eq, Show)

-----------------------------------------------------------------------
-- Helpers for printing to the screen
-----------------------------------------------------------------------

data ANSI a where
        REVERSE :: ANSI ()                 -> ANSI ()
        COLOR   :: Color -> ANSI ()        -> ANSI ()
        PRINT   :: String                  -> ANSI ()
        AT      :: ANSI () -> (Int,Int)    -> ANSI ()
        BIND'    :: ANSI b -> (b -> ANSI a) -> ANSI a
        RETURN'  :: a                       -> ANSI a
        CLEAR    ::                           ANSI ()

instance Monad ANSI where
        return a = RETURN' a
        m >>= k  = BIND' m k

showANSI :: ANSI a -> IO a
showANSI (REVERSE ascii) = do
        setSGR [SetSwapForegroundBackground True]
        showANSI ascii
        setSGR []
        hFlush stdout
showANSI (COLOR col ascii) = do
        setSGR [SetColor Foreground Vivid col]
        showANSI ascii
        setSGR []
        hFlush stdout
showANSI (PRINT str) = do
        putStr str
showANSI (AT ascii (row,col)) = do
        setCursorPosition row col
        showANSI ascii
        setCursorPosition 24 0
        hFlush stdout
showANSI (RETURN' a) = return a
showANSI (BIND' m k) = do
        a <- showANSI m
        showANSI (k a)
showANSI (CLEAR) = do
        clearScreen
        hFlush stdout

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

class SimulatorInput i where
        simKeyboard :: Char -> [i]
        simRead     :: String -> Word8 -> [i]

class (Eq o) => SimulatorOutput o where
        simBackground :: o
        simTerminal   :: o -> ANSI ()
        simWrite      :: o -> Maybe (String,Word8)



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

runDeviceSimulator :: (Show o) => Device i o -> Simulator i o () -> IO ()
runDeviceSimulator (Device fn) (Simulator sim) = do

        let input = Steps $ return ([],input)

        let simulator ins = Steps $ do

                ss <- stepsToStreams ins

                let ((),outs0,_) = sim $ ss

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

inputDevice :: (Show i) => IO [i] -> Device i o
inputDevice i_fn = Device $ \ fn inp -> fn (mapSteps input inp)
    where
        input xs = do
           ys <- i_fn
           return (ys ++ xs)

outputDevice :: (Show o) => ([o] -> IO ()) -> Device i o
outputDevice o_fn = Device $ \ fn inp -> mapSteps output (fn inp)
 where
         output xs = do
                 o_fn xs
                 return xs

bootDevice :: IO () -> Device i o
bootDevice boot = Device $ \ fn inp -> once (fn inp)
   where
           once (Steps m) = Steps (boot >> m)

ansiOutput :: (Show o) => ANSI () -> (o -> ANSI ()) -> Device i o
ansiOutput start fn = mconcat
        [ bootDevice $ (showANSI start)
        , outputDevice $ \ os -> do sequence_ [ showANSI (fn o) | o <- os ]
                                    hFlush stdout
        ]

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

deviceSupport :: (SimulatorInput i, SimulatorOutput o) => Simulator i o () -> Stream [i] -> IO (Stream [o])
deviceSupport (Simulator fab) ins = do

        -- create the virtual device directory
        createDirectoryIfMissing True "dev"

        let ((),outs0,devs) = fab $ ins

        let outs2 = transpose1
                  $ outs0
        let outs3 = fmap (fmap (maybe [] (: [])))
                  $ outs2
        let outs4 = fmap concat outs3

        return $ outs4

runSimulator :: forall i o . (Show o, Show i, SimulatorInput i, SimulatorOutput o)
              => ExecMode -> Integer -> Integer -> Simulator i o () -> IO ()
runSimulator _ _ _ = runDeviceSimulator
                         (  keyboardInput simKeyboard
                         <> ansiOutput (simTerminal' simBackground) simTerminal'
                         )
  where simTerminal' = simTerminal

transpose1 :: [Stream a] -> Stream [a]
transpose1 = foldr (S.zipWith (:)) (pure [])

ex1 = [s1,s2,s3]
 where
        s1 = S.fromList [1..10]
        s2 = S.fromList [5,10,15,20]
        s3 = S.fromList [99..]


