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
        , Graphic(..)
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

-----------------------------------------------------------------------


{-
-- SimulatorMonad implements a simulator for FPGA boards.
-- Perhaps call it the SimulatorMonad
class CoreMonad fab => SimulatorMonad fab where
        polyester :: Simulator a -> fab a      -- escape into the simulator
        circuit   :: fab () -> Simulator ()    -- extact the sim circuit, for interpretation
        gen_board :: fab ()

-- call this polyesterBoard?
board     :: Fabric a -> Simulator a
board stmt = Simulator $ \ _ env st ->
                                let Pure (a,outs) = runFabric stmt (pOutNames env)
                                in return (a,[SimulatorOutput outs],st)
-}
-----------------------------------------------------------------------
-- Monad
-----------------------------------------------------------------------

data SimulatorConfiguration i o = SimulatorConfiguration
        { sc_reading :: IO (Maybe i)
        , sc_writing :: [o] -> IO ()
        , sc_execMode :: ExecMode
        , sc_clkSpeed :: Integer
        }

-- | The simulator uses its own 'Fabric', which connects not to pins on the chip,
-- but rather an ASCII picture of the board.

data SimulatorEnv = SimulatorEnv
                        { pExecMode   :: ExecMode
                        , pStdin      :: [Maybe Char]
                        , pFindSocket :: String -> IO Handle
                        , pClkSpeed   :: Integer                -- clock speed, in Hz
                        , pSimSpeed   :: Integer                -- how many cycles are we *actually* doing a second
                        }

data SimulatorState = SimulatorState
                        { pCores        :: [String]             --registered cores
                        }
                deriving Show

data SimulatorOut = SimulatorStepper Stepper
                  | SimulatorTrace [()]                         -- evaluated to force a tracer
                  | SimulatorWriteSocket String String [Enabled U8]
                                                                -- socket name, (fabric) port name,
                                                                -- what to output (if any)
                                                                                                                                        -- (AppendMode and ReadWriteMode not supported)


-- Sigh, the *only* use of IO is for SimulatorReadSocket.
-- Once its in, it may be used by others.

data Simulator a = Simulator (SimulatorEnv
                               -> SimulatorState
                               -> IO (a,[SimulatorOut],SimulatorState))


instance Monad Simulator where
        return a = Simulator $ \ _ st -> return (a,mempty,st)
        (Simulator f) >>= k = Simulator $ \ env st0 -> do
                                (a,w1,st1)  <- f env st0
                                let Simulator g = k a
                                (b,w2,st2)  <- g env st1
                                return (b,w1 `mappend` w2,st2)
        fail msg = error msg

instance MonadFix Simulator where
        -- TODO: check this
        mfix f = Simulator $ \ env st ->
                        mfix (\ ~(r,_,_) -> let (Simulator g) = f r in g env st)

instance MonadIO Simulator where
        liftIO m = Simulator $ \ env st -> do
                        r <- m
                        return $ (r,[],st)

getSimulatorExecMode :: Simulator ExecMode
getSimulatorExecMode = Simulator $ \ env st -> return (pExecMode env,mempty,st)

getSimulatorClkSpeed :: Simulator Integer
getSimulatorClkSpeed = Simulator $ \ env st -> return (pClkSpeed env,mempty,st)

getSimulatorSimSpeed :: Simulator Integer
getSimulatorSimSpeed = Simulator $ \ env st -> return (pSimSpeed env,mempty,st)

initializedCores :: Simulator [String]
initializedCores = Simulator $ \ env st -> return (pCores st,mempty,st)

-----------------------------------------------------------------------
-- Ways out outputing from the Simulator
-----------------------------------------------------------------------

-- | Checks an input list for diffences between adjacent elements,
-- and for changes, maps a graphical event onto the internal stepper.
-- The idea is that sending a graphical event twice should be
-- idempotent, but internally the system only writes events
-- when things change.
outSimulator :: (Eq a, Graphic g) => (a -> g) -> Stream a -> Simulator ()
outSimulator f = outSimulatorEvents . fmap (fmap f) . changed

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


-- | Turn a list of graphical events into a 'Simulator', without processing.
outSimulatorEvents :: (Graphic g) => Stream (Maybe g) -> Simulator ()
outSimulatorEvents = scheduleConsumption (maybe (return ()) (showANSI . drawGraphic))

-- | Schedule the runing of a stepper.
scheduleStepper :: Stepper -> Simulator ()
scheduleStepper ss = Simulator $ \ _ st -> return ((),[SimulatorStepper $ ss],st)

-- | creates single graphical events, based on the number of Events,
-- when the first real event is event 1, and there is a beginning of time event 0.
-- Example of use: count the number of bytes send or recieved on a device.
outSimulatorCount :: (Graphic g) => (Integer -> g) -> Stream (Maybe ()) -> Simulator ()
outSimulatorCount f = outSimulator f . S.fromList . loop 0 . S.toList
  where
        loop n (Nothing:xs) = n : loop n xs
        loop n (Just _:xs)  = n : loop (succ n) xs

-- | write a socket from a clocked list input. Example of use is emulating
-- RS232 (which only used empty or singleton strings), for the inside of a list.

writeSocketSimulator :: String -> String -> [Maybe U8] -> Simulator ()
writeSocketSimulator filename portname ss = Simulator $ \ _ st ->
        return ((),[SimulatorWriteSocket filename portname ss],st)


-----------------------------------------------------------------------
-- Ways out inputting to the Simulator
-----------------------------------------------------------------------

-- | Turn an observation of the keyboard into a list of values.
inSimulator :: a                               -- ^ initial 'a'
         -> (Char -> a -> a)                   -- ^ how to interpreate a key press
         -> Simulator (Stream a)
inSimulator a interp = Simulator $ \ env st -> do
        let f' a' Nothing = a'
            f' a' (Just c) = interp c a'
            vals = scanl f' a (pStdin env)
        return (S.fromList vals,mempty,st)

-- | 'readSocketSimulator' reads from a socket.
-- The stream is on-demand, and is not controlled by any clock
-- inside the function. Typically would be read one cons per
-- clock, but slower reading is acceptable.
-- This does not make any attempt to register
-- what is being observed on the screen; another
-- process needs to do this.

readSocketSimulator :: String -> String -> Int -> Simulator [Maybe U8]
readSocketSimulator filename portname speed = Simulator $ \ env st -> do
        sock <- pFindSocket env $ filename
        op_ch <- hGetContentsStepwise sock
        let f Nothing = [Nothing]
            f (Just x) = Just x : take speed (repeat Nothing)
        let ss = concatMap f $ fmap (fmap (fromIntegral . ord)) op_ch
        return (ss,[],st)

-----------------------------------------------------------------------
-- Debugging Wiring
-----------------------------------------------------------------------

outSimulatorDebugging :: [()] -> Simulator ()
outSimulatorDebugging us = Simulator $ \ _ st ->
        return ((),[SimulatorTrace us],st)

-----------------------------------------------------------------------
-- Running the Simulator
-----------------------------------------------------------------------

data ExecMode
        = Fast          -- ^ run as fast as possible, and do not display the clock
        | Friendly      -- ^ run in friendly mode, with 'threadDelay' to run slower, to be CPU friendly.
  deriving (Eq, Show)

-----------------------------------------------------------------------
-- Utils for building boards
-----------------------------------------------------------------------

-- | 'generic_init' builds a generic board_init, including
-- setting up the drawing of the board, and printing the (optional) clock.
{-
generic_init :: (Graphic g1,Graphic g2) => g1 -> (Integer -> g2) -> Simulator ()
generic_init board clock = do
        -- a bit of a hack; print the board on the first cycle
        outSimulator (\ _ -> board) [()]
        mode <- getSimulatorExecMode
        when (mode /= Fast) $ do
                outSimulator (clock) $ S.fromList [0..]
        return ()
-}
-----------------------------------------------------------------------
-- Abstaction for output (typically the screen)
-----------------------------------------------------------------------

class Graphic g where
        drawGraphic :: g -> ANSI ()

-----------------------------------------------------------------------
-- Internal: The Stepper abstraction, which is just the resumption monad
-----------------------------------------------------------------------

-- The idea in the future is we can common up the changes to the
-- screen, removing needless movement of the cursor, allowing
-- a slight pause before updating, etc.

-- Do something, and return.
data Stepper = Stepper (IO (Stepper))

runStepper :: Stepper -> IO Stepper
runStepper (Stepper m) = m

-- | 'runSteppers' runs several steppers concurrently.
runSteppers :: [Stepper] -> IO ()
runSteppers ss = do
        ss' <- sequence [ runStepper m
                        | m <- ss
                        ]
        runSteppers ss'

-- Stepper could be written in terms of ioStepper
stepper :: (Graphic g) => [Maybe g] -> Stepper
stepper = ioStepper
        . map (\ o -> case o of
                         Nothing -> do { return () }
--                         Just g  -> return ()
                         Just g -> do { showANSI (drawGraphic g) }
                )

ioStepper :: [IO ()] -> Stepper
ioStepper (m:ms)      = Stepper (do m ; return (ioStepper ms))
ioStepper other       = Stepper (return $ ioStepper other)

cycleStepper :: IO () -> Stepper
cycleStepper m = Stepper (do m ; return (cycleStepper m))

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

-- | Rather than use a data-structure for each action,
-- ANSI can be used instead. Not recommended, but harmless.
instance Graphic (ANSI a) where
        drawGraphic g = do g ; return ()

instance Graphic () where
        drawGraphic () = return ()

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

runSimulator  :: forall fab . ExecMode -> Integer -> Integer -> Simulator () -> IO ()
runSimulator mode clkSpeed simSpeed fab = do
        setTitle "Kansas Lava"
        putStrLn "[Booting Spartan3e simulator]"
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False

        -- create the virtual device directory
        createDirectoryIfMissing True "dev"

        inputs <- hGetContentsStepwise stdin

        putStrLn "[Starting simulation]"
        clearScreen
        setCursorPosition 0 0
        hFlush stdout


--        let -- clockOut | mode == Fast = return ()
--            clockOut | mode == Friendly =
--                        outSimulator clock [0..]

        sockDB <- newMVar []
        let findSock :: String -> IO Handle
            findSock nm = do
                sock_map <- takeMVar sockDB
                case lookup nm sock_map of
                  Just h -> do
                        putMVar sockDB sock_map
                        return h
                  Nothing -> do
                        h <- finally
                              (do sock <- listenOn $ UnixSocket nm
                                  putStrLn $ "* Waiting for client for " ++ nm
                                  (h,_,_) <- accept sock
                                  putStrLn $ "* Found client for " ++ nm
                                  return h)
                              (removeFile nm)
                        hSetBuffering h NoBuffering
                        putMVar sockDB $ (nm,h) : sock_map
                        return h

        let env = SimulatorEnv
                        { pExecMode   = mode
                        , pStdin      = inputs
                        , pFindSocket = findSock
                        , pClkSpeed   = clkSpeed
                        , pSimSpeed   = simSpeed
                        }

        let st0 = SimulatorState {}

        let extras :: Simulator () = do
                quit <- inSimulator False (\ c _ -> c == 'q')
                outSimulator (\ b -> if b
                                  then error "Simulation Quit"
                                  else return () :: ANSI ()) quit

        let Simulator circuit = fab >> extras

        ((),output, st1) <- circuit env st0

        let steps = [ o | SimulatorStepper o <- output ]

        socket_steppers :: [Stepper] <- sequence
              [ do sock <- findSock $ filename
                   let f (Just ch) = do
                               -- TODO: should throw away if going to block
                               hPutStr sock [chr $ fromIntegral ch]
                               hFlush sock
                       f Nothing          = return ()

                   return $ ioStepper $ map f $ ss
              | SimulatorWriteSocket filename portname ss <- output
              ]

{-
        socket_read_steppers :: [Stepper] <- sequence
              [ do sock <- findSock $ filename
                   var_sync <- newEmptyMVar
                   forkIO $ forever $ do
                         print "start loop"
                         opt_ok <- try (hReady sock)
                         print ("read sock",opt_ok)
                         case opt_ok of
                           Right ok -> do
                                 if ok then do ch <- hGetChar sock
                                               putMVar var (Just (fromIntegral (ord ch)))
                                        else do print "A"
                                                putMVar var Nothing
                                                print "B"
                           Left (e :: IOException) -> putMVar var Nothing
                   return $ cycleStepper $ do { print "X";  putMVar var_sync () ; print "Y" }
              | SimulatorReadSocket filename portname var <- output
              ]
-}
        -- we put debugging into ./dev/log.XX

        monitor_steppers :: [Stepper] <- sequence
              [ do let f () = return ()
                   return $ ioStepper $ map f os
              | SimulatorTrace os <- output
              ]



        setProbesAsTrace $ appendFile "LOG"

        print (length [ () | SimulatorStepper _ <- output ])

        let slowDown | mode == Fast = []
                     | mode == Friendly =
                         [ ioStepper [ do { threadDelay (20 * 1000) }
                                     | _ <- [(0 :: Integer)..] ]]

        runSteppers (steps ++ slowDown ++ socket_steppers ++ monitor_steppers)

        return ()

------------------------------------------------------------------------------------

-- API
{-
newInPort :: [a] -> InPort a

readPort :: InPort a -> M (Maybe a)

writePort :: OutPort a -> a -> M (Maybe a)

scheduleStepper :: (a -> M b) -> InPort a -> Fab (OutPort b)
scheduleStepper_ :: M () -> Fab ()

state :: (s -> M s) -> M ()

writeSocketSimulator :: String -> String -> Stream (Maybe U8) -> Simulator ()
writeSocketSimulator filename portname ss = Simulator $ \ _ st ->
        return ((),[SimulatorWriteSocket filename portname ss],st)

scheduleOnce :: IO () -> Simulator ()


readSocketSimulator :: String -> String -> Int -> Simulator [Maybe U8]
-}

scheduleConsumption :: (a -> IO ()) -> Stream a -> Simulator ()
scheduleConsumption f = scheduleStepper . ioStepper . map f . S.toList

scheduleProduction :: IO a -> Simulator (Stream a)
scheduleProduction m = liftIO $ loop
  where
          loop = unsafeInterleaveIO $ do
                        hd <- m
                        tl <- loop
                        return $ S.cons hd tl

---------------------------------------------------------------


data Simulator2 i o a = Simulator2
        { unSimulator2 :: Stream [i] -> (a,[Stream o],[String])
        }

instance Monad (Simulator2 i o) where
        return a = Simulator2 $ \ _ -> (a,[],[])
        (Simulator2 m) >>= k = Simulator2 $ \ i ->
                let (a,o1,d1) = m i
                    (r,o2,d2) = unSimulator2 (k a) i
                in (r,o1 ++ o2,d1 ++ d2)

instance MonadFix (Simulator2 i o)  where
        mfix f = Simulator2 $ \ i ->
                   let (a,o,d) = unSimulator2 (f a) i
                   in (a,o,d)

simInput :: ([i] -> a) -> Simulator2 i o (Stream a)
simInput f = Simulator2 $ \ i -> (fmap f i,[],[])

simOutput :: Stream o -> Simulator2 i o ()
simOutput o = Simulator2 $ \ _ -> ((),[o],[])

-- | state that a particual 'dev' is required
simDevice :: String -> Simulator2 i o  ()
simDevice d = Simulator2 $ \ _ -> ((),[],[d])

class SimulatorInput i where
        simKeyboard :: Char -> [i]
        simRead     :: String -> Word8 -> [i]

class (Eq o) => SimulatorOutput o where
        simBackground :: o
        simTerminal   :: o -> ANSI ()
        simWrite      :: o -> Maybe (String,Word8)

runSimulator2 :: forall i o . (Show o, Show i, SimulatorInput i, SimulatorOutput o)
              => ExecMode -> Integer -> Integer -> Simulator2 i o () -> IO ()
runSimulator2 mode clkSpeed simSpeed (Simulator2 fab) = do
        setTitle "Kansas Lava"
        putStrLn "[Booting Spartan3e simulator]"
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False

        -- create the virtual device directory
        createDirectoryIfMissing True "dev"

        inputs <- hGetContentsStepwise stdin

        putStrLn "[Starting simulation]"
        clearScreen
        setCursorPosition 0 0
        hFlush stdout

        showANSI $ simTerminal (simBackground :: o)

        -- fab :: Stream [i] -> (a,[Stream o],[String])

        let ins = [ maybe [] (simKeyboard) ch
                  | ch <- inputs
                  ]

--        print $ concat ins

        let ((),outs0,devs) = fab $ S.fromList $ ins

        let outs1 = fmap changed outs0

        let outs2 = transpose1 outs1
        let outs3 = fmap (fmap (maybe [] (: []))) outs2
        let outs4 = fmap concat outs3

        sequence_ [ do mapM_ (showANSI . simTerminal) s
                       setCursorPosition 0 0
                       putStrLn $ show i
                       hFlush stdout
                  | (i,s) <- zip [0..] $ S.toList $ outs4
                  ]


{-


--        let -- clockOut | mode == Fast = return ()
--            clockOut | mode == Friendly =
--                        outSimulator clock [0..]

        sockDB <- newMVar []
        let findSock :: String -> IO Handle
            findSock nm = do
                sock_map <- takeMVar sockDB
                case lookup nm sock_map of
                  Just h -> do
                        putMVar sockDB sock_map
                        return h
                  Nothing -> do
                        h <- finally
                              (do sock <- listenOn $ UnixSocket nm
                                  putStrLn $ "* Waiting for client for " ++ nm
                                  (h,_,_) <- accept sock
                                  putStrLn $ "* Found client for " ++ nm
                                  return h)
                              (removeFile nm)
                        hSetBuffering h NoBuffering
                        putMVar sockDB $ (nm,h) : sock_map
                        return h

        let env = SimulatorEnv
                        { pExecMode   = mode
                        , pStdin      = inputs
                        , pFindSocket = findSock
                        , pClkSpeed   = clkSpeed
                        , pSimSpeed   = simSpeed
                        }

        let st0 = SimulatorState {}

        let extras :: Simulator () = do
                quit <- inSimulator False (\ c _ -> c == 'q')
                outSimulator (\ b -> if b
                                  then error "Simulation Quit"
                                  else return () :: ANSI ()) quit

        let Simulator circuit = fab >> extras

        ((),output, st1) <- circuit env st0

        let steps = [ o | SimulatorStepper o <- output ]

        socket_steppers :: [Stepper] <- sequence
              [ do sock <- findSock $ filename
                   let f (Just ch) = do
                               -- TODO: should throw away if going to block
                               hPutStr sock [chr $ fromIntegral ch]
                               hFlush sock
                       f Nothing          = return ()

                   return $ ioStepper $ map f $ ss
              | SimulatorWriteSocket filename portname ss <- output
              ]

{-
        socket_read_steppers :: [Stepper] <- sequence
              [ do sock <- findSock $ filename
                   var_sync <- newEmptyMVar
                   forkIO $ forever $ do
                         print "start loop"
                         opt_ok <- try (hReady sock)
                         print ("read sock",opt_ok)
                         case opt_ok of
                           Right ok -> do
                                 if ok then do ch <- hGetChar sock
                                               putMVar var (Just (fromIntegral (ord ch)))
                                        else do print "A"
                                                putMVar var Nothing
                                                print "B"
                           Left (e :: IOException) -> putMVar var Nothing
                   return $ cycleStepper $ do { print "X";  putMVar var_sync () ; print "Y" }
              | SimulatorReadSocket filename portname var <- output
              ]
-}
        -- we put debugging into ./dev/log.XX

        monitor_steppers :: [Stepper] <- sequence
              [ do let f () = return ()
                   return $ ioStepper $ map f os
              | SimulatorTrace os <- output
              ]



        setProbesAsTrace $ appendFile "LOG"

        print (length [ () | SimulatorStepper _ <- output ])

        let slowDown | mode == Fast = []
                     | mode == Friendly =
                         [ ioStepper [ do { threadDelay (20 * 1000) }
                                     | _ <- [(0 :: Integer)..] ]]

        runSteppers (steps ++ slowDown ++ socket_steppers ++ monitor_steppers)
-}
        return ()

transpose1 :: [Stream a] -> Stream [a]
transpose1 = foldr (S.zipWith (:)) (pure [])

ex1 = [s1,s2,s3]
 where
        s1 = S.fromList [1..10]
        s2 = S.fromList [5,10,15,20]
        s3 = S.fromList [99..]


