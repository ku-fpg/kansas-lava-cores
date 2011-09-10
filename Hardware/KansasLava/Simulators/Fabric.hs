{-# LANGUAGE ScopedTypeVariables, GADTs, DeriveDataTypeable #-}
-- | * Remember to call init_board for your specific board.

module Hardware.KansasLava.Simulators.Fabric (
          -- * The Fake Fabric Monad, and its NPM.
          Fabric               -- abstract
        , outFabric
        , outFabricEvents
        , outFabricCount
        , writeFileFabric
        , inFabric
        , readFileFabric
        -- * running the fake Fabric
        , friendlyFabric
        , runFabric
        -- * Support for the (ANSI) Graphics
        , ANSI(..)
        , Color(..)     -- from System.Console.ANSI
        , Graphic(..)
--        , at
--        , green
--        , red
--        , reverse_video
        ) where
        
import System.Console.ANSI
import System.IO
import Data.Typeable
import Control.Exception
import Control.Concurrent
import Data.Char
import Control.Monad.Fix
import Data.Word
import System.IO.Unsafe (unsafeInterleaveIO)

-----------------------------------------------------------------------
-- Monad
-----------------------------------------------------------------------

-- | The simulator uses its own 'Fabric', which connects not to pins on the chip, 
-- but rather an ASCII picture of the board.

data Fabric a = Fabric ([Maybe Char] -> IO (a,[Stepper]))


instance Monad Fabric where
        return a = Fabric $ \ _ -> return (a,[])
        (Fabric f) >>= k = Fabric $ \ inp -> do
                                (a,s1)  <- f inp
                                let Fabric g = k a
                                (b,s2)  <- g inp
                                return (b,s1 ++ s2)
        fail msg = error msg

instance MonadFix Fabric where
        -- TODO: check this
        mfix f = Fabric $ \ inp -> mfix (\ r ->  let (Fabric g) = f (fst r) 
                                                 in g inp)
-----------------------------------------------------------------------
-- Ways out outputing from the Fabric
-----------------------------------------------------------------------

-- | Checks an input list for diffences between adjacent elements,
-- and for changes, maps a graphical event onto the internal stepper.
-- The idea is that sending the graphical event twice, it should be 
-- idempotent.
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
outFabricEvents ogs = Fabric $ \ _ -> return ((),[stepper ogs])

-- creates single graphical events, based on the number of Events,
-- when the first real event is event 1, and there is a beginning of time event 0.
outFabricCount :: (Graphic g) => (Integer -> g) -> [Maybe a] -> Fabric ()
outFabricCount f = outFabric f . loop 0
  where
        loop n (Nothing:xs) = n : loop n xs
        loop n (Just _:xs)  = n : loop (succ n) xs

writeFileFabric :: String -> [Maybe Word8] -> Fabric ()
writeFileFabric filename contents = Fabric $ \ _ -> do
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

-- | Turn a observation of the keyboard into a list of values.
inFabric :: a                           -- ^ initial 'a'
         -> (Char -> a -> a)            -- ^ how to interpreate a key press
         -> Fabric [a]
inFabric a interp = Fabric $ \ inp -> do
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
readFileFabric filename = Fabric $ \ inp -> do
        h <- openBinaryFile filename ReadMode
        hSetBuffering h NoBuffering  
        ss <- hGetContentsStepwise h
        return (map (fmap (fromIntegral . ord)) ss,[])

-----------------------------------------------------------------------
-- Running the Fabric
-----------------------------------------------------------------------

-- | 'runFabric' executes the Fabric, never returns, and ususally replaces 'reifyFabric'.

runFabric :: Fabric () -> IO ()
runFabric (Fabric f) = do
        setTitle "Kansas Lava"
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False
        inputs <- hGetContentsStepwise stdin
        (_,steps) <- f inputs
	putStr "\ESC[2J\ESC[1;1H"
        runSteppers steps

-- | 'friendlyFabric' slows things down, to be CPU friendly.
friendlyFabric :: Fabric ()
friendlyFabric = Fabric $ \ _ ->
        return ((),[ioStepper [ threadDelay (20 * 1000) | _ <- [(0 :: Integer)..]]])

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
