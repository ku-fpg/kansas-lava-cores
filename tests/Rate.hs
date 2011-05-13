{-# LANGUAGE ScopedTypeVariables #-}
module Rate (tests) where

import Language.KansasLava
import Hardware.KansasLava.Rate (rate)
import Data.Sized.Ix
import Data.Maybe 
--import Debug.Trace

tests :: TestSeq -> IO ()
tests test = do
        let t1 :: (Size w) => String -> Witness w -> Rational -> Rational -> IO ()
            t1 str = testRate test str
            
	t1 "0.01" (Witness :: Witness X16) 0.01 0.0
	t1 "0.05" (Witness :: Witness X16) 0.05 0.0
	t1 "0.1"  (Witness :: Witness X16) 0.1  0.0
	t1 "0.2"  (Witness :: Witness X16) 0.2  0.0
	t1 "0.3"  (Witness :: Witness X16) 0.3  0.0
	t1 "0.33" (Witness :: Witness X16) 0.33 0.0
	t1 "0.4"  (Witness :: Witness X16) 0.4  0.001
	t1 "0.5"  (Witness :: Witness X16) 0.5  0.0
	t1 "0.6"  (Witness :: Witness X16) 0.6  0.001
	t1 "0.66" (Witness :: Witness X16) 0.66 0.001
	t1 "0.7"  (Witness :: Witness X16) 0.7  0.0
	t1 "0.8"  (Witness :: Witness X16) 0.8  0.0
	t1 "0.9"  (Witness :: Witness X16) 0.9  0.0
	t1 "0.95" (Witness :: Witness X16) 0.95 0.0
	t1 "1"    (Witness :: Witness X16) 1    0.0

        -- And some *real* examples.
        t1 "baud" (Witness :: Witness X16) (115200 / (50 * 1000 * 1000)) 0.001

        -- And others
	t1 "1/16"    (Witness :: Witness X16) (1/16)    0.001
	t1 "1/32"    (Witness :: Witness X16) (1/32)    0.001	
	t1 "1/64"    (Witness :: Witness X16) (1/64)    0.001	
	t1 "1/65"    (Witness :: Witness X16) (1/65)    0.001		
	t1 "2/65"    (Witness :: Witness X16) (2/65)    0.001

        -- And others
	t1 "2/65@8"  (Witness :: Witness X8) (2/65)    0.001
	t1 "2/65@7"  (Witness :: Witness X7) (2/65)    0.001
	t1 "2/65@6"  (Witness :: Witness X6) (2/65)    0.001

	

        return ()

testRate :: forall w . 
             (Size w) 
          => TestSeq
          -> String
          -> Witness w
          -> Rational
          -> Rational
          -> IO ()
testRate (TestSeq test _) nm w r limit = do
        let dut = do
                let o0 :: Seq Bool
                    o0 = rate w r
                outStdLogic "o0" (o0 :: CSeq () Bool)
            driver = do
                ans <- inStdLogic "o0"
                let vs = fromSeq ans
                return $ \ n -> 
                        let sofar :: [Rational]
                            sofar = [ fromIntegral (length (filter (== Just True) (take i vs))) / fromIntegral i
                                    | i <- [n `div` 10,n]
                                    ]
                            delta :: [String]
                            delta = [ "testRate failure: " ++ show (s,r,abs(s-r),limit)
                                    | s <- sofar
                                   , abs (s - r) > limit
                                   ]
                        in listToMaybe delta

        test ("rate/" ++ nm) 10000 dut driver

