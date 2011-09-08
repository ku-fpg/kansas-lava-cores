module Hardware.KansasLava.Simulators.Fabric where
        
import System.IO
import Control.Monad.Fix
import System.IO.Unsafe (unsafeInterleaveIO)


-- | The simulator uses its own Fabric, which connects not to pins on the chip, but rather.
data Fabric a = Fabric ([Maybe Char] -> (a,[Stepper]))

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

green :: IO () -> IO ()
green m = do
        putStr $ "\ESC[32m"
        m
        putStr $ "\ESC[0m"

at :: IO () -> (Int,Int) -> IO ()
at m (row,col) = do
	putStr $ "\ESC[" ++ show row ++ ";" ++ show col ++ "H"
        m
	putStr $ "\ESC[22;1H"
	hFlush stdout
