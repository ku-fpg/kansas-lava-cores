-- | * Remember to call init_board for your specific board.

module Hardware.KansasLava.Simulators.Fabric (
          -- * The Fake Fabric Monad, and its constructor
          Fabric        -- abstract
        , outFabric
        , inFabric
        -- * running the fake Fabric
        , runFabric
        -- * Support for the (ASCII) Graphics
        , Graphic(..)
        , at
        , green
        , reverse_video
        ) where
        
import System.IO
import Control.Monad.Fix
import System.IO.Unsafe (unsafeInterleaveIO)

-- | The simulator uses its own 'Fabric', which connects not to pins on the chip, but rather an ASCII picture of the board.
data Fabric a = Fabric ([Maybe Char] -> (a,[Stepper]))


-- | Turn a list of graphical events into a 'Fabric'.
outFabric :: (Graphic g) => [Maybe g] -> Fabric ()
outFabric ogs = Fabric $ \ _ -> ((),[stepper ogs])


-- | Turn a observation of the keyboard into a list of values.
inFabric :: (Eq a, Graphic g) 
         => a                           -- ^ initial 'a'
         -> (a -> g)                    -- ^ how to print 'a' (when changed)
         -> (Char -> a -> a)            -- ^ how to interpreate a key press
         -> Fabric [a]
inFabric a pr interp = Fabric $ \ inp ->
        let f' a' Nothing = a'
            f' a' (Just c) = interp c a'
            vals = scanl f' a inp
        in (vals,[stepper $ map (fmap pr) $ changed vals])
        
  where
        changed :: (Eq a) => [a] -> [Maybe a]
        changed (a:as) = Just a : f a as
            where
                f x (y:ys) | x == y    = Nothing : f x ys
                           | otherwise = Just y : f y ys
                f _ [] = []


instance Monad Fabric where
        return a = Fabric $ \ _ -> (a,[])
        (Fabric f) >>= k = Fabric $ \ inp -> 
                                let (a,s1)   = f inp
                                    Fabric g = k a
                                    (b,s2)   = g inp
                                in
                                    (b,s1 ++ s2)
        fail msg = error msg

instance MonadFix Fabric where
        mfix f = Fabric $ \ inp -> let Fabric g = f a
                                       (a,ss) = g inp
                                   in (a,ss)
        
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

class Graphic g where
        drawGraphic :: g -> IO ()

stepper :: (Graphic g) => [Maybe g] -> Stepper
stepper (Nothing:ms) = Stepper (do return (stepper ms))
stepper (Just o:ms)  = Stepper (do drawGraphic o ; return (stepper ms))
stepper other        = Stepper (return $ stepper other)


-- | 'runFabric' executes the Fabric, never returns, and ususally replaces 'reifyFabric'.

runFabric :: Fabric () -> IO ()
runFabric (Fabric f) = do
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False
        let loop :: IO [Maybe Char]
            loop = do
                ok <- hReady stdin
                out <- if ok then do
                        ch <- getChar
                        return (Just ch)
                       else return Nothing
                rest <- unsafeInterleaveIO $ loop
                return (out : rest)
        inputs <- loop 
        let (_,steps) = f inputs
	putStr "\ESC[2J\ESC[1;1H"
        runSteppers steps


-- Helpers for printing to the screen

-- | Do an IO (print) in the context of a green pen.
green :: IO () -> IO ()
green m = do
        putStr $ "\ESC[32m"
        m
        putStr $ "\ESC[0m"


reverse_video :: IO () -> IO ()
reverse_video m = do
        putStr $ "\ESC[7m"
        m
        putStr $ "\ESC[0m"        

-- | Do an  IO (print) at a specific location on the screen.
at :: IO () -> (Int,Int) -> IO ()
at m (row,col) = do
	putStr $ "\ESC[" ++ show row ++ ";" ++ show col ++ "H"
        m
	putStr $ "\ESC[24;1H"
	hFlush stdout
