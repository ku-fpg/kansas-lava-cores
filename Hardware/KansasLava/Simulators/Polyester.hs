{-# LANGUAGE ScopedTypeVariables, GADTs, DeriveDataTypeable, DoRec, KindSignatures #-}

module Hardware.KansasLava.Simulators.Polyester where
{-
          -- * The (abstract) Fake Fabric Monad
          Polyester -- abstract
        , board
          -- * The Polyester non-proper morphisms
        , outPolyester
        , outPolyesterEvents
        , outPolyesterCount
        , writeSocketPolyester
        , inPolyester
        , readSocketPolyester
        , getPolyesterExecMode
        , getPolyesterClkSpeed
        , getPolyesterSimSpeed
        -- * Running the Fake Polyester
        , runPolyester
        , ExecMode(..)
        -- * Support for building fake Boards
        , generic_init
        -- * Support for the (ANSI) Graphics
        , ANSI(..)
        , Color(..)     -- from System.Console.ANSI
        , Graphic(..)
        , CoreMonad(..)
        , PolyesterMonad(..)
        , initializedCores
        ) where
-}

import Language.KansasLava hiding (Fast)
import Language.KansasLava.Fabric
import Language.KansasLava.Universal

import Hardware.KansasLava.Core

import System.Console.ANSI
import System.IO
import Data.Typeable
import Control.Exception
import Control.Concurrent
import Control.Monad
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
-- PolyesterMonad implements a simulator for FPGA boards.
-- Perhaps call it the SimulatorMonad
class CoreMonad fab => PolyesterMonad fab where
        polyester :: Polyester a -> fab a      -- escape into the simulator
        circuit   :: fab () -> Polyester ()    -- extact the sim circuit, for interpretation
        gen_board :: fab ()

-- call this polyesterBoard?
board     :: Fabric a -> Polyester a
board stmt = Polyester $ \ _ env st ->
                                let Pure (a,outs) = runFabric stmt (pOutNames env)
                                in return (a,[PolyesterOutput outs],st)
-}
-----------------------------------------------------------------------
-- Monad
-----------------------------------------------------------------------

-- | The simulator uses its own 'Fabric', which connects not to pins on the chip,
-- but rather an ASCII picture of the board.

data PolyesterEnv = PolyesterEnv
                        { pExecMode   :: ExecMode
                        , pStdin      :: [Maybe Char]
                        , pFindSocket :: String -> IO Handle
                        , pClkSpeed   :: Integer                -- clock speed, in Hz
                        , pSimSpeed   :: Integer                -- how many cycles are we *actually* doing a second
                        }

data PolyesterState = PolyesterState
                        { pCores        :: [String]             --registered cores
                        }
                deriving Show

data PolyesterOut = PolyesterStepper Stepper
                  | PolyesterOutput [(String,Pad)]
                  | PolyesterSocket String String Int IOMode    -- socket name, (fabric) port name,
                                                                -- speed, and ReadMode vs WriteMode
                                                                -- (AppendMode and ReadWriteMode not supported)

data Polyester a = Polyester (PolyesterEnv
                               -> PolyesterState
                               -> Pure (a,[PolyesterOut],PolyesterState))


instance Monad Polyester where
        return a = Polyester $ \ _ st -> return (a,mempty,st)
        (Polyester f) >>= k = Polyester $ \ env st0 -> do
                                (a,w1,st1)  <- f env st0
                                let Polyester g = k a
                                (b,w2,st2)  <- g env st1
                                return (b,w1 `mappend` w2,st2)
        fail msg = error msg

instance MonadFix Polyester where
        -- TODO: check this
        mfix f = Polyester $ \ env st ->
                        mfix (\ ~(r,_,_) -> let (Polyester g) = f r in g env st)

getPolyesterExecMode :: Polyester ExecMode
getPolyesterExecMode = Polyester $ \ env st -> return (pExecMode env,mempty,st)

getPolyesterClkSpeed :: Polyester Integer
getPolyesterClkSpeed = Polyester $ \ env st -> return (pClkSpeed env,mempty,st)

getPolyesterSimSpeed :: Polyester Integer
getPolyesterSimSpeed = Polyester $ \ env st -> return (pSimSpeed env,mempty,st)

initializedCores :: Polyester [String]
initializedCores = Polyester $ \ env st -> return (pCores st,mempty,st)

getBoardExecMode :: (Simulator f) => Board f ExecMode
getBoardExecMode = Board $ \ _ env -> return (return (pExecMode env,mempty))

getBoardClkSpeed :: (Simulator f) => Board f Integer
getBoardClkSpeed = Board $ \ _ env -> return (return (pClkSpeed env,mempty))

getBoardSimSpeed :: (Simulator f) => Board f Integer
getBoardSimSpeed = Board $ \ _ env -> return (return (pSimSpeed env,mempty))

-----------------------------------------------------------------------
-- Ways out outputing from the Polyester
-----------------------------------------------------------------------

-- | Checks an input list for diffences between adjacent elements,
-- and for changes, maps a graphical event onto the internal stepper.
-- The idea is that sending a graphical event twice should be
-- idempotent, but internally the system only writes events
-- when things change.
outPolyester :: (Eq a, Graphic g) => (a -> g) -> [a] -> Polyester ()
outPolyester f = outPolyesterEvents . map (fmap f) . changed

changed :: (Eq a) => [a] -> [Maybe a]
changed (a:as) = Just a : f a as
    where
        f x (y:ys) | x == y    = Nothing : f x ys
                   | otherwise = Just y : f y ys
        f _ [] = []

-- | Turn a list of graphical events into a 'Polyester', without processing.
outPolyesterEvents :: (Graphic g) => [Maybe g] -> Polyester ()
outPolyesterEvents ogs = Polyester $ \ _ st -> return ((),[PolyesterStepper $ stepper ogs],st)


-- | creates single graphical events, based on the number of Events,
-- when the first real event is event 1, and there is a beginning of time event 0.
-- Example of use: count the number of bytes send or recieved on a device.
outPolyesterCount :: (Graphic g) => (Integer -> g) -> [Maybe ()] -> Polyester ()
outPolyesterCount f = outPolyester f . loop 0
  where
        loop n (Nothing:xs) = n : loop n xs
        loop n (Just _:xs)  = n : loop (succ n) xs

-- | write a socket from a clocked list input. Example of use is emulating
-- RS232 (which only used empty or singleton strings), for the inside of a list.

writeSocketPolyester :: String -> String -> Int -> Polyester ()
writeSocketPolyester filename portname speed = Polyester $ \ _ st ->
        return ((),[PolyesterSocket filename portname speed WriteMode],st)


-----------------------------------------------------------------------
-- Ways out inputting to the Polyester
-----------------------------------------------------------------------

-- | Turn an observation of the keyboard into a list of values.
inPolyester :: a                           -- ^ initial 'a'
         -> (Char -> a -> a)            -- ^ how to interpreate a key press
         -> Polyester [a]
inPolyester a interp = Polyester $ \ env st -> do
        let f' a' Nothing = a'
            f' a' (Just c) = interp c a'
            vals = scanl f' a (pStdin env)
        return (vals,mempty,st)


-- | 'readSocketPolyester' reads from a socket.
-- The stream is on-demand, and is not controlled by any clock
-- inside the function. Typically would be read one cons per
-- clock, but slower reading is acceptable.
-- This does not make any attempt to register
-- what is being observed on the screen; another
-- process needs to do this.

readSocketPolyester :: String -> String -> Int -> Polyester ()
readSocketPolyester filename portname speed = Polyester $ \ _ st ->
        return ((),[PolyesterSocket filename portname speed ReadMode],st)

-----------------------------------------------------------------------
-- Running the Polyester
-----------------------------------------------------------------------

data ExecMode
        = Fast          -- ^ run as fast as possible, and do not display the clock
        | Friendly      -- ^ run in friendly mode, with 'threadDelay' to run slower, to be CPU friendly.
  deriving (Eq, Show)
{-
-- | 'runPolyester' executes the Polyester, never returns, and ususally replaces 'reifyFabric'.
runPolyester :: (PolyesterMonad circuit) => ExecMode -> Integer -> Integer -> circuit () -> IO ()
runPolyester mode clkSpeed simSpeed f = do

        setTitle "Kansas Lava"
        putStrLn "[Booting Spartan3e simulator]"
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False

        -- create the virtual device directory
        createDirectoryIfMissing True "dev"

        inputs <- hGetContentsStepwise stdin

--        let -- clockOut | mode == Fast = return ()
--            clockOut | mode == Friendly =
--                        outPolyester clock [0..]

        let extras = do
                quit <- inPolyester False (\ c _ -> c == 'q')
                outPolyester (\ b -> if b
                                  then error "Simulation Quit"
                                  else return () :: ANSI ()) quit

        let Polyester h = do
                extras
                circuit (f >> gen_board)
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

------------------------------------------------------------------------------
{- This is compiling the Fabric, and the board that goes with it.


 [E Ch] +-------+  in_names   +-----------+
------->|       |------------>|           |
        | Board |             | Generated |
        |       |             |  Fabric   |
        |       |<------------| (STMT)    |
        +-------+  out_names  +-----------+
           IO |
              +-----------------------------------------------> Panel

-}

        rec let fab :: SuperFabric Pure ((),[PolyesterOut],PolyesterState)
                fab = compileToFabric $ h inputs env st0


                env = PolyesterEnv
                        { pExecMode   = mode
                        , pFindSocket = findSock
                        , pClkSpeed   = clkSpeed
                        , pSimSpeed   = simSpeed
                        , pOutNames   = out_names
                        }

                st0 = PolyesterState
                        { pCores = []
                        }

                -- break the abstaction, building the (virtual) board
                Pure (((),output,st1),in_names',out_names) = unFabric fab in_names

            in_names :: [(String,Pad)] <- do
                    -- later abstract out
                    socket_reads <- sequence
                              [ do sock <- findSock $ filename
                                   ss <- hGetContentsStepwise sock
                                   let ss' = concatMap (\ x -> x : replicate (speed - 1) Nothing) ss
                                   return ( portname
                                           , toUni (toS (fmap (fmap (fromIntegral . ord)) ss') :: Seq (Maybe U8))
                                           )
                                | PolyesterSocket filename portname speed ReadMode <- output
                                ]
                    return $ [ o | PolyesterOutput os <- output, o <- os ]
                          ++ socket_reads

        let steps = [ o | PolyesterStepper o <- output ]

        socket_steppers :: [Stepper] <- sequence
              [ do sock <- findSock $ filename
                   let ss :: Seq (Enabled U8)
                       ss = case lookup portname out_names of
                           Nothing -> error $ "can not find output " ++ show portname
                           Just p -> case fromUni p of
                               Nothing -> error $ "type error in port " ++ show portname
                               Just s -> s
                       f (Just (Just ch)) = do
                               hPutStr sock [chr $ fromIntegral ch]
                               hFlush sock
                       f (Just Nothing)   = return ()
                       f Nothing          = error "socket gone wrong (undefined output)"

                   return $ ioStepper $ map f $ fromS ss
                | PolyesterSocket filename portname speed WriteMode <- output
                ]

        monitor_steppers :: [Stepper] <- sequence
              [ do let ss :: Seq (Enabled ())
                       ss = case fromUni p of
                               Nothing -> error $ "type error in port " ++ show o
                               Just s -> s
                       f Nothing          = return ()
                       f (Just Nothing)   = return ()
                       f (Just (Just ())) = return ()    -- perhaps a visual ping?
                   return $ ioStepper $ map f $ fromS ss
              | (o,p) <- out_names
              , "monitor/" `isPrefixOf` o
              ]

        putStrLn "[Starting simulation]"
        clearScreen
        setCursorPosition 0 0
        hFlush stdout

--	putStr "\ESC[2J\ESC[1;1H"

        let slowDown | mode == Fast = []
                     | mode == Friendly =
                         [ ioStepper [ threadDelay (20 * 1000)
                                     | _ <- [(0 :: Integer)..] ]]

        return ()
        runSteppers (steps ++ slowDown ++ socket_steppers ++ monitor_steppers)

-}

-----------------------------------------------------------------------
-- Utils for building boards
-----------------------------------------------------------------------

-- | 'generic_init' builds a generic board_init, including
-- setting up the drawing of the board, and printing the (optional) clock.

generic_init :: (Graphic g1,Graphic g2) => g1 -> (Integer -> g2) -> Polyester ()
generic_init board clock = do
        -- a bit of a hack; print the board on the first cycle
        outPolyester (\ _ -> board) [()]
        mode <- getPolyesterExecMode
        when (mode /= Fast) $ do
                outPolyester (clock) [0..]
        return ()

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
                         Just g -> do { showANSI (drawGraphic g) })

ioStepper :: [IO ()] -> Stepper
ioStepper (m:ms)      = Stepper (do m ; return (ioStepper ms))
ioStepper other       = Stepper (return $ ioStepper other)

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

data PolyesterException = PolyesterException String
     deriving Typeable

instance Show PolyesterException where
     show (PolyesterException msg) = msg

instance Exception PolyesterException

data Board (f :: * -> *) a = Board
        { unBoard :: [Maybe Char]
                  -> PolyesterEnv
                  -> IO (f (a,[PolyesterOut]))
        }
{-
data Polyester a = Polyester ([Maybe Char]
                               -> PolyesterEnv
                               -> PolyesterState
                               -> STMT (a,[PolyesterOut],PolyesterState))
-}


instance Monad (Board f) where {}
instance MonadFix (Board f) where {}

--class Reify fab => Simulator fab where
--        simulatedBoard :: SuperFabric (Board fab) ()

class Monad fab => Simulator fab where
        runPolyester :: fab () -> Polyester ()

runSimulator  :: forall fab . (Simulator fab) => ExecMode -> Integer -> Integer -> fab () -> IO ()
runSimulator mode clkSpeed simSpeed fab = do
        setTitle "Kansas Lava"
        putStrLn "[Booting Spartan3e simulator]"
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False

        -- create the virtual device directory
        createDirectoryIfMissing True "dev"

        inputs <- hGetContentsStepwise stdin

--        let -- clockOut | mode == Fast = return ()
--            clockOut | mode == Friendly =
--                        outPolyester clock [0..]

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

        let env = PolyesterEnv
                        { pExecMode   = mode
                        , pStdin      = inputs
                        , pFindSocket = findSock
                        , pClkSpeed   = clkSpeed
                        , pSimSpeed   = simSpeed
                        }

        let st0 = PolyesterState {}

        let extras :: Polyester () = do
                quit <- inPolyester False (\ c _ -> c == 'q')
                outPolyester (\ b -> if b
                                  then error "Simulation Quit"
                                  else return () :: ANSI ()) quit

        let Polyester circuit = runPolyester fab >> extras

{-

Polyester ([Maybe Char]
                               -> PolyesterEnv
                               -> PolyesterState
                               -> IO (a,[PolyesterOut],PolyesterState))
-}

        let Pure ((),output, st1) = circuit env st0
{-
            in_names :: [(String,Pad)] <- do
                    -- later abstract out
                    socket_reads <- sequence
                              [ do sock <- findSock $ filename
                                   ss <- hGetContentsStepwise sock
                                   let ss' = concatMap (\ x -> x : replicate (speed - 1) Nothing) ss
                                   return ( portname
                                           , toUni (toS (fmap (fmap (fromIntegral . ord)) ss') :: Seq (Maybe U8))
                                           )
                                | PolyesterSocket filename portname speed ReadMode <- output
                                ]
                    return $ [ o | PolyesterOutput os <- output, o <- os ]
                          ++ socket_reads
-}
        let steps = [ o | PolyesterStepper o <- output ]

{-
        socket_steppers :: [Stepper] <- sequence
              [ do sock <- findSock $ filename
                   let ss :: Seq (Enabled U8)
                       ss = case lookup portname out_names of
                           Nothing -> error $ "can not find output " ++ show portname
                           Just p -> case fromUni p of
                               Nothing -> error $ "type error in port " ++ show portname
                               Just s -> s
                       f (Just (Just ch)) = do
                               hPutStr sock [chr $ fromIntegral ch]
                               hFlush sock
                       f (Just Nothing)   = return ()
                       f Nothing          = error "socket gone wrong (undefined output)"

                   return $ ioStepper $ map f $ fromS ss
                | PolyesterSocket filename portname speed WriteMode <- output
                ]

        monitor_steppers :: [Stepper] <- sequence
              [ do let ss :: Seq (Enabled ())
                       ss = case fromUni p of
                               Nothing -> error $ "type error in port " ++ show o
                               Just s -> s
                       f Nothing          = return ()
                       f (Just Nothing)   = return ()
                       f (Just (Just ())) = return ()    -- perhaps a visual ping?
                   return $ ioStepper $ map f $ fromS ss
              | (o,p) <- out_names
              , "monitor/" `isPrefixOf` o
              ]
-}

        putStrLn "[Starting simulation]"
        clearScreen
        setCursorPosition 0 0
        hFlush stdout

        print (length [ () | PolyesterStepper _ <- output ])

        let slowDown | mode == Fast = []
                     | mode == Friendly =
                         [ ioStepper [ do { threadDelay (20 * 1000) }
                                     | _ <- [(0 :: Integer)..] ]]

        runSteppers (steps ++ slowDown {- ++ socket_steppers ++ monitor_steppers-})

{-
        return ()
        let brd = simulatedBoard

        let liftMe :: fab a -> Board fab a
            liftMe f = Board $ \ _ _ -> return (fmap (\ a -> (a,[])) f)

        let Board m = do
                runFabricWithDriver (liftFabric liftMe fab) brd


        -- build the
        r <- m undefined undefined
        let Pure ((),output) = purify r


-}


        return ()





