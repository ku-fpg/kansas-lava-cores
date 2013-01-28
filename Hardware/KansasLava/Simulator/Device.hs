{-# LANGUAGE ScopedTypeVariables, GADTs, DeriveDataTypeable, DoRec, KindSignatures, TypeFamilies, BangPatterns #-}

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
import Control.Applicative
import Data.Word
import Network
import System.Directory
import qualified Control.Exception as E

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
                     if n==1000 then error "DONE" else return ()
                     return (n + 1))

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
        , s_tx_wait :: Maybe Int
        , s_rx_wait :: Maybe Int
        }

socketDevice :: ([o] -> Maybe Int)      -- speed
             -> ([o] -> Maybe Word8)    -- char to circuit (RX)
             -> (Word8 -> [i])          -- char from circuit (TX)
             -> String                  -- device filename
             -> Device i o
socketDevice speed rx tx name = device
        (return NoSocketInfo)
        (\ xs -> return ([],xs))
        outputs
  where
        outputs s xs = case speed xs of
                         Just n -> do
                            v <- newEmptyMVar
                            forkIO $ finally
                              ((do sock <- listenOn $ UnixSocket name
                                   (h,_,_) <- accept sock
                                   hSetBuffering h NoBuffering
                                   putMVar v (SocketInfo h n Nothing Nothing))
                                 -- If anything goes wrong, just do not connect
                                 -- This should go into some sort of log
                                `E.catch` (\ (e :: E.SomeException) -> return ()))
                              (removeFile name)
                            return $ BootingSocketInfo v

                         Nothing -> return s

{-

                        putMVar sockDB $ (nm,h) : sock_map
-}

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

