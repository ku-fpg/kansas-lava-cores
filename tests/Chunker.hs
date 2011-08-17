{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module Chunker (tests) where

import Language.KansasLava
import Hardware.KansasLava.Chunker

import Data.Sized.Unsigned
import Data.Sized.Signed
import Data.Sized.Arith
import Data.Sized.Ix
import Data.Ratio
import System.Random
--import Data.Maybe 
import Debug.Trace
import Data.Word
import Data.List as L

tests :: TestSeq -> IO ()
tests test = do
        -- testing Chunker

        let waitForItTest :: (Size x, Size y, Rep w, Show w) => Unsigned x -> Witness y -> StreamTest w (Unsigned x)
            waitForItTest mx w = StreamTest
                        { theStream = waitForIt mx w
                        , correctnessCondition = \ ins outs -> 
--                                 trace (show ("cc",length ins,length outs)) $
--                                 trace (show ("ins",map show (take 100 ins))) $
--                                 trace (show ("outs",map show (take 100 outs))) $
				case (length ins, sum $ map fromIntegral outs) of
				   (i,o) | maximum outs > mx
						  -> Just ("packet to large " ++ show (outs,mx))
					 | any (== 0) outs
						  -> Just ("found empty packet " ++ show outs)
					 | i == o -> Nothing
				         | otherwise -> Just ("found " ++ show i ++ " elements, tagged " ++ show o ++ show (ins,outs))
			, theStreamTestCount  = count
			, theStreamTestCycles = count * 100
                        , theStreamName = "chunker/waitForIt"
                        }
	    count = 1000

	-- Need to think about 0.
	let t :: forall w . (Size w) => Witness w -> IO ()
	    t w = sequence_ [ 
		testStream test ("U8/" ++ show n ++ "/" ++ show (size (undefined :: w)))
			    	  (waitForItTest (n :: U8) w) 
			          (dubGen arbitrary :: Gen (Maybe S11)) | n <- [1,10,16,100,255] ]
			
	t (Witness :: Witness X1)
	t (Witness :: Witness X2)
	t (Witness :: Witness X3)
	t (Witness :: Witness X4)
	t (Witness :: Witness X5)
	t (Witness :: Witness X10)

        let chunkCounterTest :: forall x y .
				(Size x, Size y, Rep y, Rep x, Num y, Num x)
			     => Witness x -> StreamTest (Unsigned y) Bool
            chunkCounterTest w = StreamTest
                        { theStream = chunkCounter w
                        , correctnessCondition = \ ins outs -> 
				let xs = L.group outs
				    hds = map length $ filter (\ (x:_) -> x) xs
			     	    tls = map length $ filter (\ (x:_) -> not x) xs
				in case () of
				     _ | not (L.all (== (size (undefined :: x))) hds) ->
						Just $ "header length error " ++ show hds
				     _ | tls /= map fromIntegral ins -> Just $
						"bad length of lows " ++ show (tls,ins)
				     _ | otherwise -> Nothing
			, theStreamTestCount  = count
			, theStreamTestCycles = count * 500
                        , theStreamName = "chunker/chunkCounter"
                        }
	    count = 10

	let t :: forall x . (Rep x, Size x, Num x) => Witness x -> IO ()
	    t w = testStream test ("U8/" ++ show (size (undefined :: x)))
			    	  (chunkCounterTest w)
			          (dubGen arbitrary :: Gen (Maybe U8))

	t (Witness :: Witness X1)
	t (Witness :: Witness X2)
	t (Witness :: Witness X3)
	t (Witness :: Witness X4)
	t (Witness :: Witness X5)
	t (Witness :: Witness X6)

	return ()
