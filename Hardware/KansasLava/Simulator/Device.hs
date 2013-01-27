{-# LANGUAGE ScopedTypeVariables, GADTs, DeriveDataTypeable, DoRec, KindSignatures, TypeFamilies #-}

module Hardware.KansasLava.Simulator.Device where

import qualified Language.KansasLava.Stream as S

import Hardware.KansasLava.Simulator.ANSI
import Hardware.KansasLava.Simulator.Stream

import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Monoid
import Control.Exception
import System.IO
import Control.Concurrent
import Data.IORef
import Control.Applicative


{-
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
-}
-- Steps [i] -> (Steps [s],Steps [o]))

newtype Device i o = Device (Circuit [i] [o] -> Circuit [i] [o])

instance Monoid (Device i o) where
        mempty = Device id
        mappend (Device a) (Device b) = Device $ a . b

-----------------------------------------------------------------------
-- Circuits
-----------------------------------------------------------------------

newtype Circuit i o   = Circuit   (IO (o,NeedInput i o))

newtype NeedInput i o = NeedInput (IO (i -> NowOutput i o))

newtype NowOutput i o = NowOutput (IO (o,Circuit i o))

processorToCircuit :: (Stream i -> Stream o) -> Circuit i o
processorToCircuit f = Circuit $ do
        c_i <- newEmptyMVar
        c_o <- newEmptyMVar

        -- The one bit of uglyness; at least it is hidden
        let input = unsafeInterleaveIO $ do
                        x <- takeMVar c_i
                        xs <- input
                        return (x `S.cons` xs)
        forkIO $ do
                xs <- input
                sequence_ [ putMVar c_o v | v <- S.toList $ f $ xs ]

        let loop = do
                i <- takeMVar c_o
                return (i, NeedInput $ do
                        return $ \ a -> NowOutput $ do
                          putMVar c_i a
                          i <- takeMVar c_o
                          return (i,Circuit $ loop))

        loop

-----------------------------------------------------------------------
-- Device builders
-----------------------------------------------------------------------
{-
newtype Circuit i o   = Circuit   (IO (o,NeedInput i o))
newtype NeedInput i o = NeedInput (IO (i -> NowOutput i o))
newtype NowOutput i o = NowOutput (IO (o,Circuit i o))
-}

inputDevice :: IO [i] -> Device i o
inputDevice i_fn = Device circuit
    where
        circuit (Circuit m) = Circuit $ do
                (a,rest) <- m
                return (a,needInput rest)
        needInput (NeedInput m) = NeedInput $ do
                f <- m
                i' <- i_fn
                return $ \ i -> nowOutput (f (i' ++ i))
        nowOutput (NowOutput m) = NowOutput $ do
                (a,rest) <- m
                return (a,circuit rest)

outputDevice :: ([o] -> IO ()) -> Device i o
outputDevice o_fn = Device circuit
    where
        circuit (Circuit m) = Circuit $ do
                (a,rest) <- m
                o_fn a
                return (a,needInput rest)
        needInput (NeedInput m) = NeedInput $ do
                f <- m
                return $ \ i -> nowOutput (f i)
        nowOutput (NowOutput m) = NowOutput $ do
                (a,rest) <- m
                o_fn a
                return (a,circuit rest)


-- The book device runs an IO action when building the
-- device stack, and you can use the result to customize
-- a device. Examples include the use of a Handle when
-- reading files.

bootDevice :: IO a -> (a -> Device i o) -> Device i o
bootDevice boot k = Device $ \ cir -> Circuit $ do
        -- Run the boot effect
        a <- boot
        let Device k_dev = k a
        let Circuit m = k_dev cir
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
keyboardInput interp =
        bootDevice (do
           hSetBuffering stdin NoBuffering
           hSetEcho stdin False) $ \ () ->
        inputDevice $ do
        opt_ok <- try (hReady stdin)
        case opt_ok of
           Right True -> do
                ch <- hGetChar stdin
                return $! interp ch
           Right False -> do
                return []
           Left (e :: IOException) -> do
                return []

--        -- create the virtual device directory
--        createDirectoryIfMissing True "dev"

-- Utilties

-- socketDevice :: String -> Device i o


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

