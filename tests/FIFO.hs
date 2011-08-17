{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module FIFO (tests) where

import Language.KansasLava
import Hardware.KansasLava.FIFO (fifo)

import Data.Sized.Unsigned
import Data.Sized.Arith
import Data.Sized.Ix
import System.Random
import Data.Ratio
--import Data.Maybe 
import Debug.Trace

tests :: TestSeq -> IO ()
tests test = do
        -- testing FIFOs


        let fifoTest :: forall w sz . (Rep (ADD sz X1),
                      Rep sz,
                      Rep w,
                      Eq w,
                      Size sz,
                      Size (ADD sz X1),
                      Num sz,
                      Num (ADD sz X1)) => Witness sz -> StreamTest w w
            fifoTest wit = StreamTest
                        { theStream = fifo wit low $$ ackToReadyBridge
				:: Patch (Seq (Enabled w)) (Seq (Enabled w))
				         (Seq Ack)	      (Seq Ready)
                        , correctnessCondition = \ ins outs -> -- trace (show ("cc",length ins,length outs)) $
                                case () of
                                  () | outs /= take (length outs) ins -> return "in/out differences"
                                  () | length outs < fromIntegral count 
     								      -> return ("to few transfers: " ++ show (length outs))
                                  () | length ins - length outs > size (undefined :: sz) 
								      -> return ("missing items?" ++ show (length ins,length outs,size (undefined :: sz)))
                                     | otherwise -> Nothing

	    		, theStreamTestCount  = count
	    		, theStreamTestCycles = 
				if size (undefined :: sz) <= 2
				then 40000
				else 30000
                        , theStreamName = "fifo/" ++ show (size (error "witness" :: sz))
                        }
	   	where
			count = 1000

        let t :: forall w sz sz1 .
                 (Eq w, Rep w, Show w,
                               Size (W w),
                  sz1 ~ ADD sz X1,
                               Size (ADD (W w) X1),     --- Hmm
                  Size sz, Size sz1,
                  Rep sz, Rep sz1,
                  Num w, Num sz, Num sz1)
                 => String -> Gen (Maybe w) -> Witness sz -> IO ()
            t str arb w = testStream test str (fifoTest w) (dubGen arb)

        t "U5"  (arbitrary :: Gen (Maybe U5)) (Witness :: Witness X1)
        t "U5"  (arbitrary :: Gen (Maybe U5)) (Witness :: Witness X2)
        t "U5"  (arbitrary :: Gen (Maybe U5)) (Witness :: Witness X3)
        t "U5"  (arbitrary :: Gen (Maybe U5)) (Witness :: Witness X4)
        t "U5"  (arbitrary :: Gen (Maybe U5)) (Witness :: Witness X5)
        t "U5"  (arbitrary :: Gen (Maybe U5)) (Witness :: Witness X6)
        t "U5"  (arbitrary :: Gen (Maybe U5)) (Witness :: Witness X7)
        t "U5"  (arbitrary :: Gen (Maybe U5)) (Witness :: Witness X8)


	return ()
