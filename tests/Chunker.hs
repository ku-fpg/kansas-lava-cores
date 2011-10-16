{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module Chunker (tests) where

import Language.KansasLava
import Hardware.KansasLava.Chunker

import Data.Sized.Unsigned
import Data.Sized.Signed
import Data.Sized.Arith
import Data.Sized.Matrix (Matrix,(!), matrix)
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
		testStream test ("U4/" ++ show n ++ "/" ++ show (size (undefined :: w)))
			    	  (waitForItTest n w :: StreamTest S11 U4) 
			     | n <- [1,2,3,4,8,15] ]

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
				let msgs = fn outs
				    x = size (undefined :: x)

--				    fn xs | trace (show $ take 10 xs) False = undefined
				    fn [] = []
				    fn xs | L.all (== True) (take x xs) = n : fn (drop n xs')
					 where n = length (takeWhile (== False) xs')
					       xs' = drop x xs
				    fn _ = error "bad stream from chunkCounter"

				    ins' = ins ++ [0]	-- always will have a zero, because of the pre-issue
				in case () of
				     _ | msgs /= map fromIntegral ins' -> Just $
						"bad length of lows " ++ show (msgs,ins')
				     _ | otherwise -> Nothing
			, theStreamTestCount  = count
			, theStreamTestCycles = count * 500
                        , theStreamName = "chunker/chunkCounter"
                        }
	    count = 20

	let t :: forall x . (Rep x, Size x, Num x) => Witness x -> IO ()
	    t w = testStream test ("U4/" ++ show (size (undefined :: x)))
			    	  (chunkCounterTest w :: StreamTest U4 Bool)
			
	t (Witness :: Witness X1)
	t (Witness :: Witness X2)
	t (Witness :: Witness X3)
	t (Witness :: Witness X4)
	t (Witness :: Witness X5)
	t (Witness :: Witness X6)

        let chunkSplitJoinTest :: StreamTest U4 U4
            chunkSplitJoinTest = StreamTest
                        { theStream = chunkSplitHeader f $$
					-- here we
					-- (1) Turn header ABC into headder CBA, where B is the length;
					-- (2) Add one to every member of the payload.
				      stackP (forwardP (mapEnabled 
							    (\ ms -> let m = unpack ms
								   in pack (matrix [m ! 2,m ! 1,m ! 0]))))
					    (forwardP (mapEnabled (+1))) $$
				      chunkJoinHeader f
                        , correctnessCondition = \ ins outs -> 
--                                 trace (show ("cc",length ins,length outs)) $
--                                 trace (show ("ins",map show (take 100 ins))) $
--                                 trace (show ("outs",map show (take 100 outs))) $
				 let readPackets (a:b:c:d) = (a,b,c,take (fromIntegral b) d) 
							   : readPackets (drop (fromIntegral b) d)
				     readPackets [] = [] -- hack
				     readPackets _ = error "bad packet!"

				     xs = map (\(a,b,c,d) -> (c,b,a,map (+1) d)) $ readPackets (take (length outs) ins)
				     ys = readPackets outs
				 in case () of
--				       _ | trace (show xs) False -> Nothing
--				       _ | trace (show ys) False -> Nothing
				       _ | length xs < 100 -> Just $ "too few packets ???" ++ show (length xs)
				       _ | length xs /= length ys -> Just $ "# of packets different " ++ show ( length xs, length ys )
				       _ | xs /= ys -> Just $ "bad join + split: " ++ show (zip xs ys)
				       _ | otherwise -> Nothing

			, theStreamTestCount  = count
			, theStreamTestCycles = count * 4
                        , theStreamName = "chunker/join-split"
                        }

	    f :: forall comb . Signal comb (Matrix X3 U4) -> Signal comb U4
	    f m = (unpack m :: Matrix X3 (Signal comb U4)) ! 1

	    count = 2000

	testStream test ("U4")
			(chunkSplitJoinTest)

	return ()
