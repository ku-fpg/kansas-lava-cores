{-# LANGUAGE ScopedTypeVariables, GADTs, DeriveDataTypeable, DoRec, KindSignatures, TypeFamilies, BangPatterns #-}
{-# LANGUAGE FlexibleInstances, TypeOperators, LiberalTypeSynonyms #-}
module Hardware.KansasLava.Simulator.Device where

import qualified Language.KansasLava.Stream as S

import Hardware.KansasLava.Simulator.ANSI
import Hardware.KansasLava.Simulator.Stream

import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Monoid
import Control.Exception
import Control.Monad
import System.IO
import Control.Concurrent
import Data.IORef
import Data.Char
import Control.Applicative
import Data.Word
import Network
import System.Directory
import qualified Control.Exception as E

{-
newtype Device i o = Device (Circuit [i] [o] -> Circuit [i] [o])

instance Monoid (Device i o) where
        mempty = Device id
        mappend (Device a) (Device b) = Device $ a . b
-}
-----------------------------------------------------------------------
-- Somewhere else??
-----------------------------------------------------------------------

type (f :: * -> *) $ (a :: *) = f a
infixr 9 $      -- needs re-stated

-----------------------------------------------------------------------
-- Circuits
-----------------------------------------------------------------------

type Device' i o = Circuit [o] [i]

type RECV a m = [a] -> m
type SEND a m = ([a],m)

newtype Device i o = Device (IO $ RECV o
                           $ IO $ SEND i
                           $ IO $ RECV o
                           $ Device i o)

instance Monoid (Device i o) where
        mempty = Device $ return $ \ _os1 -> return ([], return $ \ _os2 -> mempty)
        mappend (Device d1) (Device d2) = Device $ do
                f1 <- d1
                f2 <- d2
                return $ \ os1 -> do
                        (os11,rest11) <- f1 os1
                        (os12,rest12) <- f2 os1
                        return (os11 ++ os12, do
                                f1' <- rest11
                                f2' <- rest12
                                return $ \ os2 -> mappend (f1' os2) (f2' os2))

instance Monoid (Circuit [i] [o]) where
        mempty = Circuit $ return ([],mempty)
        mappend (Circuit c1) (Circuit c2) = Circuit $ do
                (a1,n1) <- c1
                (a2,n2) <- c2
                return (a1 ++ a2,mappend n1 n2)

instance Monoid (NeedInput [i] [o]) where
        mempty = NeedInput $ return $ const mempty
        mappend (NeedInput c1) (NeedInput c2) = NeedInput $ do
                f1 <- c1
                f2 <- c2
                return $ \ os -> mappend (f1 os) (f2 os)

instance Monoid (NowOutput [i] [o]) where
        mempty = NowOutput $ return ([],mempty)
        mappend (NowOutput c1) (NowOutput c2) = NowOutput $ do
                (a1,n1) <- c1
                (a2,n2) <- c2
                return (a1 ++ a2,mappend n1 n2)

newtype Circuit i o   = Circuit   (IO (o,NeedInput i o))

newtype NeedInput i o = NeedInput (IO (i -> NowOutput i o))

newtype NowOutput i o = NowOutput (IO (o,Circuit i o))

runDeviceAndCircuit :: Device i o -> (Stream [i] -> Stream [o]) -> IO ()
runDeviceAndCircuit dev f = do
        c_i <- newEmptyMVar
        c_o <- newEmptyMVar

        -- The one bit of uglyness; at least it is hidden
        let input = unsafeInterleaveIO $ do
                        x <- takeMVar c_i
                        xs <- input
                        return (x `S.cons` xs)
        tid <- myThreadId

        forkIO ((do
            xs <- input
            sequence_ [ putMVar c_o v | v <- S.toList $ f $ xs ])
                                `E.catch` (\ (e :: E.SomeException) -> throwTo tid e))


        let loop (Device m0) = do
                f0 <- m0                -- step A
                o0 <- takeMVar c_o
                (i0,m1) <- f0 o0        -- step C
                putMVar c_i i0
                f1 <- m1                -- step C
                o1 <- takeMVar c_o
                loop (f1 o1)

        loop dev

        return ()
--newtype Circuit i o   = Circuit   (IO (o,NeedInput i o))
--newtype NeedInput i o = NeedInput (IO (i -> NowOutput i o))
--newtype NowOutput i o = NowOutput (IO (o,Circuit i o))
{-
        let runDevice (Circuit m) = do
                (is,rest) <- m
                putMVar c_i is
            runNeedInput (NeedInput m) = do

                i <- takeMVar c_o
                return (i, NeedInput $ do
                        return $ \ a -> NowOutput $ do
                          putMVar c_i a
                          i <- takeMVar c_o
                          return (i,Circuit $ loop))

        loop
-}
-----------------------------------------------------------------------
-- Device builders
-----------------------------------------------------------------------
{-
device :: IO s
       -> (s -> IO ([i],s))
       -> (s -> [o] -> IO s)
       -> Device i o
device start input output = Device $ \ cir -> Circuit $ do
        s <- start
        let Circuit m = circuit s cir
        m
    where
        circuit !s (Circuit m) = Circuit $ do
                (a,rest) <- m
                s' <- output s a
                return (a,needInput s' rest)
        needInput !s (NeedInput m) = NeedInput $ do
                f <- m
                (i',s') <- input s
                return $ \ i -> nowOutput s' (f (i' ++ i))
        nowOutput !s (NowOutput m) = NowOutput $ do
                (a,rest) <- m
                s' <- output s a
                return (a,circuit s rest)
-}
device :: IO s
       -> (s -> IO ([i],s))
       -> (s -> [o] -> IO s)
       -> Device i o
device start input output = Device $ do
        s <- start
        loop s
    where
        loop !s0 = return $ \ os -> do
                s1 <- output s0 os
                (is,s2) <- input s1
                return $ (is, return $ \ os' -> Device $ do
                        s3 <- output s2 os'
                        loop s3)

device' :: IO s
       -> (s -> IO ([i],s))
       -> (s -> [o] -> IO s)
       -> Device' i o
device' start input output = Circuit $ do
        s <- start
        circuit s
    where
        circuit !s = do
                (is,s') <- input s
                return (is,NeedInput $ needInput s')
        needInput !s = do
                return $ \ os -> NowOutput $ do
                        s' <- output s os
                        nowOutput s'
        nowOutput !s = do
                (is,s') <- input s
                return (is,Circuit $ circuit s')


inputDevice :: IO [i] -> Device i o
inputDevice i_fn = device
        (return ())
        (\ () -> liftM (\ xs -> (xs,())) i_fn)
        (\ () _ -> return ())

outputDevice :: ([o] -> IO ()) -> Device i o
outputDevice o_fn = device
        (return ())
        (\ () -> return ([],()))
        (\ () os -> o_fn os >> return ())

ansiOutput :: ANSI () -> (o -> ANSI ()) -> Device i o
ansiOutput start fn = device
        (showANSI start >> return ())
        (\ () -> return ([],()))
        (\ () os -> do sequence_ [ showANSI (fn o) | o <- os ]
                       hFlush stdout
                       return ())

-- Lists clock number on the screen
ansiTick :: (Int,Int) -> Device i o
ansiTick pos = device
        (return 0)
        (\ n -> return ([],n))
        (\ n _ -> do showANSI $ PRINT (show (n `div` 2) ++ (if (n `mod` 2) == 0 then "^" else "$")) `AT` pos
                     return (n + 1))

stopAt :: Integer -> Device i o
stopAt n = device
        (return n)
        (\ n -> return ([],n-1))
        (\ n _ -> do if n== 0 then error "STOP" else return ()
                     return n)



-- Slow down each clock cycle, to make the process friendly to
-- other things. The number is the suggested maximum clock frequency.
nice :: Int -> Device i o
nice n = inputDevice $ do
        threadDelay $ ((1000 * 1000) `div` n)
        return []

-- Read the keyboard, and turn the keys into board commands.
keyboardInput :: (Char -> [i]) -> Device i o
keyboardInput interp = device
        (do hSetBuffering stdin NoBuffering
            hSetEcho stdin False
            return ())
        (\ () -> do
                opt_ok <- try (hReady stdin)
                xs <- case opt_ok of
                        Right True -> do
                                ch <- hGetChar stdin
                                return $! interp ch
                        Right False -> do
                                return []
                        Left (e :: IOException) -> do
                                return []
                return (xs,()))
        (\ () _ -> return ())


-- This makes inputs happen on the first cycle.
-- It can be used to configure the position of toggle switches, etc.

initialDevice :: [i] -> Device i o
initialDevice start = device
        (return start)
        (\ xs -> return (xs,[]))
        (\ xs os -> return xs)

traceOutputDevice :: (Show o) => Device i o
traceOutputDevice = device
        (return (0 :: Integer))
        (\ n -> return ([],n))
        (\ n os -> do
                     when (not (null os)) $ do
                        putStrLn (show (n `div` 2) ++ " : " ++ show os)
                     return (succ n))

--        -- create the virtual device directory
--        createDirectoryIfMissing True "dev"

-- Utilties

data Socket i o = Socket
        { socketSpeed :: [o] -> Maybe Int
        , rxWord8     :: [o] -> Maybe Word8
        , txWord8     :: Word8 -> [i]
        , socketMsg   :: String -> [o]
        }

data SocketInfo
   = NoSocketInfo
   | BootingSocketInfo
        { s_mvar    :: MVar SocketInfo -- nothing r/w until this is full.
        }
   | SocketInfo
        { s_hd      :: Handle
        , s_speed   :: Int        -- how many cycles between word8's?
        , s_tx_wait :: Int
        , s_rx_wait :: Maybe Int
        }
   deriving Show

instance Show (MVar a) where show _ = "<MVAR>"

socketDevice :: ([o] -> Maybe Int)      -- speed
             -> ([o] -> Maybe Word8)    -- char to circuit (RX)
             -> (Word8 -> [i])          -- char from circuit (TX)
             -> String                  -- device filename
             -> Device i o
socketDevice speed rx tx name = device
        (return NoSocketInfo)
        inputs
        outputs
  where

        inputs s0@NoSocketInfo = return ([],s0)
        inputs s0@(BootingSocketInfo v) = do
                res <- tryTakeMVar v
                case res of
                  Just s1 -> inputs s1
                  Nothing -> return ([],s0)
        inputs s0@(SocketInfo h speed 0 tx_wait) = do
                opt_ok <- try (hReady h)
                case opt_ok of
                  Right ok -> do
                     if ok then do
                             ch <- hGetChar h
                             return (tx (fromIntegral(ord ch)),s0 { s_tx_wait = s_speed s0})
                           else do
                             return ([],s0)
                  Left (e :: IOException) -> return ([],NoSocketInfo)
        inputs s0@(SocketInfo h speed n tx_wait) = do
                return ([], SocketInfo h speed (n - 1) tx_wait)


        outputs s xs = case speed xs of
                         Just n -> setup n
                         Nothing -> case rx xs of
                                      Just w8 -> send w8 s
                                      Nothing -> return s
        setup n = do v <- newEmptyMVar
                     forkIO $ finally (do
                                   removeFile name `E.catch` (\ (e :: E.SomeException) -> return ())
                                   sock <- listenOn $ UnixSocket name
                                   (h,_,_) <- accept sock
                                   hSetBuffering h NoBuffering
                                   forkIO $ putMVar v (SocketInfo h n 0 Nothing)
                                   return ()
                                 -- If anything goes wrong, just do not connect
                                 -- This should go into some sort of log
                                `E.catch` (\ (e :: E.SomeException) -> do
                                                print ("EXCEPTION",e)
                                                return ()))
                                        (removeFile name)
                     return $ BootingSocketInfo v
        send w8 s0@(BootingSocketInfo v) = do
                res <- tryTakeMVar v
                case res of
                  Just s1 -> send w8 s1
                  Nothing -> return s0
        send w8 s0@(SocketInfo hd _ _ _) = do
                hPutChar hd (chr (fromIntegral w8))
                hFlush hd
                return s0

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

