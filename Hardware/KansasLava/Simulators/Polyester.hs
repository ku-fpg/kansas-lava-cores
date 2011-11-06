{-# LANGUAGE ScopedTypeVariables, GADTs, DeriveDataTypeable #-}
-- | * Remember to call init_board for your specific board.

module Hardware.KansasLava.Simulators.Polyester (
          -- * The (abstract) Fake Fabric Monad
          Polyester -- abstract
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
        ) where
        
import System.Console.ANSI
import System.IO
import Data.Typeable
import Control.Exception
import Control.Concurrent
import Control.Monad
import Data.Char
import Control.Monad.Fix
import Data.Word
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Concurrent
import Network
import System.Directory

-----------------------------------------------------------------------
-- Monad
-----------------------------------------------------------------------

-- | The simulator uses its own 'Fabric', which connects not to pins on the chip, 
-- but rather an ASCII picture of the board.

data PolyesterEnv = PolyesterEnv 
                        { pExecMode   :: ExecMode
                        , pFindSocket :: String -> IO Handle
                        , pClkSpeed   :: Integer                -- clock speed, in Hz
                        , pSimSpeed   :: Integer                -- how many cycles are we *actually* doing a second
                        }
                        
data Polyester a = Polyester ([Maybe Char] 
                               -> PolyesterEnv 
                               -> IO (a,[Stepper]))


instance Monad Polyester where
        return a = Polyester $ \ _ _ -> return (a,[])
        (Polyester f) >>= k = Polyester $ \ inp st -> do
                                (a,s1)  <- f inp st
                                let Polyester g = k a
                                (b,s2)  <- g inp st
                                return (b,s1 ++ s2)
        fail msg = error msg

instance MonadFix Polyester where
        -- TODO: check this
        mfix f = Polyester $ \ inp st -> 
                        mfix (\ r ->  let (Polyester g) = f (fst r) 
                                      in g inp st)

getPolyesterExecMode :: Polyester ExecMode
getPolyesterExecMode = Polyester $ \ _ st -> return (pExecMode st,[])

getPolyesterClkSpeed :: Polyester Integer
getPolyesterClkSpeed = Polyester $ \ _ st -> return (pClkSpeed st,[])

getPolyesterSimSpeed :: Polyester Integer
getPolyesterSimSpeed = Polyester $ \ _ st -> return (pSimSpeed st,[])

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
outPolyesterEvents ogs = Polyester $ \ _ _ -> return ((),[stepper ogs])

-- | creates single graphical events, based on the number of Events,
-- when the first real event is event 1, and there is a beginning of time event 0.
-- Example of use: count the number of bytes send or recieved on a device.
outPolyesterCount :: (Graphic g) => (Integer -> g) -> [Maybe a] -> Polyester ()
outPolyesterCount f = outPolyester f . loop 0
  where
        loop n (Nothing:xs) = n : loop n xs
        loop n (Just _:xs)  = n : loop (succ n) xs

-- | write a socket from a clocked list input. Example of use is emulating
-- RS232 (which only used empty or singleton strings), for the inside of a list.

writeSocketPolyester :: String -> [Maybe String] -> Polyester ()
writeSocketPolyester filename contents = Polyester $ \ _ st -> do
        h <- pFindSocket st filename
        return ((),[ ioStepper (map (f h) contents) ])
    where
        f :: Handle -> Maybe String -> IO ()
        f _ Nothing   = return ()
        f h (Just bs) = do
                hPutStr h bs
                hFlush h

{-
writeSocketPolyester :: String -> [Maybe String] -> Polyester ()
writeSocketPolyester socketname contents = Polyester $ \ _ _ -> do
-}

-----------------------------------------------------------------------
-- Ways out inputting to the Polyester
-----------------------------------------------------------------------

-- | Turn an observation of the keyboard into a list of values.
inPolyester :: a                           -- ^ initial 'a'
         -> (Char -> a -> a)            -- ^ how to interpreate a key press
         -> Polyester [a]
inPolyester a interp = Polyester $ \ inp _ -> do
        let f' a' Nothing = a'
            f' a' (Just c) = interp c a'
            vals = scanl f' a inp
        return (vals,[]) 


-- | 'readSocketPolyester' reads from a socket.
-- The stream is on-demand, and is not controlled by any clock
-- inside the function. Typically would be read one cons per
-- clock, but slower reading is acceptable.
-- This does not make any attempt to register
-- what is being observed on the screen; another
-- process needs to do this.
readSocketPolyester :: String -> Polyester [Maybe Word8]
readSocketPolyester filename = Polyester $ \ inp st -> do
        h <- pFindSocket st filename
        ss <- hGetContentsStepwise h
        return (map (fmap (fromIntegral . ord)) ss,[])

-----------------------------------------------------------------------
-- Running the Polyester
-----------------------------------------------------------------------

data ExecMode
        = Fast          -- ^ run as fast as possible, and do not display the clock
        | Friendly      -- ^ run in friendly mode, with 'threadDelay' to run slower, to be CPU friendly.
  deriving (Eq, Show)

-- | 'runPolyester' executes the Polyester, never returns, and ususally replaces 'reifyPolyester'.
runPolyester :: ExecMode -> Integer -> Integer -> Polyester () -> IO ()
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
        
        let Polyester h = (do extras ; f)
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

        (_,steps) <- h inputs $ PolyesterEnv 
                        { pExecMode   = mode
                        , pFindSocket = findSock
                        , pClkSpeed   = clkSpeed
                        , pSimSpeed   = simSpeed
                        }
        putStrLn "[Starting simulation]"
	putStr "\ESC[2J\ESC[1;1H"

        let slowDown | mode == Fast = []
                     | mode == Friendly =
                         [ ioStepper [ threadDelay (20 * 1000) 
                                     | _ <- [(0 :: Integer)..] ]]

        runSteppers (steps ++ slowDown)

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
--        threadDelay (10 * 1000)
        runSteppers ss'

-- Stepper could be written in terms of ioStepper
stepper :: (Graphic g) => [Maybe g] -> Stepper
stepper = ioStepper 
        . map (\ o -> case o of
                         Nothing -> return ()
                         Just g -> showANSI (drawGraphic g))

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
        BIND    :: ANSI b -> (b -> ANSI a) -> ANSI a
        RETURN  :: a                       -> ANSI a
        
instance Monad ANSI where
        return a = RETURN a
        m >>= k  = BIND m k

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
showANSI (PRINT str) = putStr str
showANSI (AT ascii (row,col)) = do
        setCursorPosition row col
        showANSI ascii
        setCursorPosition 24 0
        hFlush stdout
showANSI (RETURN a) = return a
showANSI (BIND m k) = do
        a <- showANSI m
        showANSI (k a)

-- | Rather than use a data-structure for each action,
-- ANSI can be used instead. Not recommended, but harmless.
instance Graphic (ANSI a) where 
        drawGraphic g = do g ; return ()

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
