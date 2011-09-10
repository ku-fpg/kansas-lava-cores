{-# LANGUAGE ScopedTypeVariables, GADTs, DeriveDataTypeable #-}
-- | * Remember to call init_board for your specific board.

module Hardware.KansasLava.Simulators.Fabric (
          -- * The Fake Fabric Monad
          Fabric               -- abstract
          -- * The Fabric non-proper morphisms
        , outFabric
        , outFabricEvents
        , outFabricCount
        , writeFileFabric
        , inFabric
        , readFileFabric
        -- * Running the Fake Fabric
        , runFabric
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

-----------------------------------------------------------------------
-- Monad
-----------------------------------------------------------------------

-- | The simulator uses its own 'Fabric', which connects not to pins on the chip, 
-- but rather an ASCII picture of the board.

data Fabric a = Fabric ([Maybe Char] -> ExecMode -> IO (a,[Stepper]))


instance Monad Fabric where
        return a = Fabric $ \ _ _ -> return (a,[])
        (Fabric f) >>= k = Fabric $ \ inp mode -> do
                                (a,s1)  <- f inp mode
                                let Fabric g = k a
                                (b,s2)  <- g inp mode
                                return (b,s1 ++ s2)
        fail msg = error msg

instance MonadFix Fabric where
        -- TODO: check this
        mfix f = Fabric $ \ inp mode -> mfix (\ r ->  let (Fabric g) = f (fst r) 
                                                      in g inp mode)

getFabricExecMode :: Fabric ExecMode
getFabricExecMode = Fabric $ \ _ mode -> return (mode,[])

-----------------------------------------------------------------------
-- Ways out outputing from the Fabric
-----------------------------------------------------------------------

-- | Checks an input list for diffences between adjacent elements,
-- and for changes, maps a graphical event onto the internal stepper.
-- The idea is that sending a graphical event twice should be 
-- idempotent, but internally the system only writes events
-- when things change.
outFabric :: (Eq a, Graphic g) => (a -> g) -> [a] -> Fabric ()
outFabric f = outFabricEvents . map (fmap f) . changed

changed :: (Eq a) => [a] -> [Maybe a]
changed (a:as) = Just a : f a as
    where
        f x (y:ys) | x == y    = Nothing : f x ys
                   | otherwise = Just y : f y ys
        f _ [] = []

-- | Turn a list of graphical events into a 'Fabric', without processing.
outFabricEvents :: (Graphic g) => [Maybe g] -> Fabric ()
outFabricEvents ogs = Fabric $ \ _ _ -> return ((),[stepper ogs])

-- | creates single graphical events, based on the number of Events,
-- when the first real event is event 1, and there is a beginning of time event 0.
-- Example of use: count the number of bytes send or recieved on a device.
outFabricCount :: (Graphic g) => (Integer -> g) -> [Maybe a] -> Fabric ()
outFabricCount f = outFabric f . loop 0
  where
        loop n (Nothing:xs) = n : loop n xs
        loop n (Just _:xs)  = n : loop (succ n) xs

-- | write a file from a clocked list input. Example of use is emulating
-- RS232.
writeFileFabric :: String -> [Maybe Word8] -> Fabric ()
writeFileFabric filename contents = Fabric $ \ _ _ -> do
        opt_h <- try (openBinaryFile filename WriteMode)
        case opt_h of 
          Right h -> do
                  hSetBuffering h NoBuffering
                  return ((),[ ioStepper (map (f h) contents) ])
          Left (_::IOException) -> throw (FabricException $ 
                                    "Failed to open " ++ filename ++ " for writing, " ++ 
                                    "(perhaps fifo with no writer?)")

    where
        f _ Nothing   = return ()
        f h (Just ch) = do
                hPutChar h (chr (fromIntegral ch))
                hFlush h

-----------------------------------------------------------------------
-- Ways out inputting to the Fabric
-----------------------------------------------------------------------

-- | Turn an observation of the keyboard into a list of values.
inFabric :: a                           -- ^ initial 'a'
         -> (Char -> a -> a)            -- ^ how to interpreate a key press
         -> Fabric [a]
inFabric a interp = Fabric $ \ inp _ -> do
        let f' a' Nothing = a'
            f' a' (Just c) = interp c a'
            vals = scanl f' a inp
        return (vals,[]) 


-- | 'inFabricIO' reads the contents of a file.
-- The stream is on-demand, and is not controlled by any clock
-- inside the function. Typically would be read one cons per
-- clock, but slower reading is acceptable.
-- This does not make any attempt to register
-- what is being observed on the screen; another
-- process needs to do this.
readFileFabric :: String -> Fabric [Maybe Word8]
readFileFabric filename = Fabric $ \ inp _ -> do
        h <- openBinaryFile filename ReadMode
        hSetBuffering h NoBuffering  
        ss <- hGetContentsStepwise h
        return (map (fmap (fromIntegral . ord)) ss,[])

-----------------------------------------------------------------------
-- Running the Fabric
-----------------------------------------------------------------------

data ExecMode
        = Fast          -- ^ run as fast as possible, and do not display the clock
        | Friendly      -- ^ run in friendly mode, with 'threadDelay' to run slower, to be CPU friendly.
  deriving (Eq, Show)

-- | 'runFabric' executes the Fabric, never returns, and ususally replaces 'reifyFabric'.
runFabric :: ExecMode -> Fabric () -> IO ()
runFabric mode f = do
        
        setTitle "Kansas Lava"
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False
        inputs <- hGetContentsStepwise stdin

--        let -- clockOut | mode == Fast = return ()
--            clockOut | mode == Friendly =
--                        outFabric clock [0..]

        let extras = do 
                quit <- inFabric False (\ c _ -> c == 'q')
                outFabric (\ b -> if b 
                                  then error "Simulation Quit" 
                                  else return () :: ANSI ()) quit
        
        let Fabric h = (do extras ; f)
        (_,steps) <- h inputs mode
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

generic_init :: (Graphic g1,Graphic g2) => g1 -> (Integer -> g2) -> Fabric ()
generic_init board clock = do
        -- a bit of a hack; print the board on the first cycle
        outFabric (\ _ -> board) [()]
        mode <- getFabricExecMode
        when (mode /= Fast) $ do
                outFabric (clock) [0..]
        return ()

-----------------------------------------------------------------------
-- Abstaction for output (typically the screen)
-----------------------------------------------------------------------

class Graphic g where
        drawGraphic :: g -> ANSI ()

-- ACT is basically a back-door to perform IO, using the 
-- graphics handler (which is really an effects handler).
data ACT = ACT (IO ())

--instance Graphic ACT where
--        drawGraphic (ACT m) = m

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
showANSI (COLOR col ascii) = do
        setSGR [SetColor Foreground Vivid col]
        showANSI ascii
        setSGR []
showANSI (PRINT str) = putStr str
showANSI (AT ascii (row,col)) = do
        setCursorPosition row col
        showANSI ascii
        setCursorPosition 24 0
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

data FabricException = FabricException String
     deriving Typeable

instance Show FabricException where
     show (FabricException msg) = msg

instance Exception FabricException
