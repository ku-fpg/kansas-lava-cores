{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module LCD (tests) where

import Language.KansasLava
import Language.KansasLava.Test
import Hardware.KansasLava.FIFO (fifo)
--import Hardware.KansasLava.LCD (lcdBootP)

import Data.Sized.Unsigned
import Data.Sized.Arith
import Data.Sized.Ix
import Data.Ratio
import System.Random
--import Data.Maybe 
import Debug.Trace
import Data.Word

tests :: TestSeq -> IO ()
tests _test = do

      return ()
{-
        -- testing The LCD

	let f n = [ (g (n' `div` 16),50)
                  , (g (n' `mod` 16),
		     if n <= 0x03 then 100000 else 2000)
                  ]
              where n'  = n `mod` 256
	      	    g m = if n > 0xff then (16 + fromIntegral m) 
		      	       	      else (0  + fromIntegral m) 

	-- What the boot sequence is
	let bootSeq =
	        [ (0x3,205000)
		, (0x3, 5000)
		, (0x3, 2000)
		, (0x2, 2000)
		] ++ concatMap f 
		          [ 0x28, 0x06, 0x0C, 0x1
                          ]

	-- Test boot sequence generator
        let lcdTest1 :: StreamTest U9 (U5,U18)
	    lcdTest1 = StreamTest
                        { theStream = 
				  lcdBootP
                        , correctnessCondition = \ ins outs -> 
--                                 trace (show ("cc",length ins,length outs)) $
--                                 trace (show ("ins",map show (take 100 ins))) $
--                                 trace (show ("outs",map show (take 100 outs))) $
                                case () of
				  () | length outs <= 0 -> 
				           return ("sequence out too short")
                                     | take (length bootSeq) outs /= bootSeq -> 
				       	   return ("sequence problem " ++ 
					   	 show (zip outs bootSeq))
                                     | concatMap f ins /= drop (length bootSeq) outs ->
				       	   return ("proceeded sequence problem " ++ 
					   	 show (zip (concatMap f ins)
						      	   (drop (length bootSeq) outs)))
				     | otherwise -> Nothing
			, theStreamTestCount  = count
			, theStreamTestCycles = 1000
                        , theStreamName = "lcdBootP1"
                        }
	    count = 100

        testStream test 
		     "lcd"
	  	     lcdTest1


	runlcdBootP test

	return ()

runlcdBootP :: TestSeq -> IO ()
runlcdBootP (TestSeq test _)  = do
	let cir :: Seq (Enabled U9) -> Seq (Enabled (U5,U18))
	    cir ins = out
	      where 
	        (_,out) = enabledToAckBox $$ 
			  lcdBootP $$ 
			  unitClockP $$
			  ackBoxToEnabled $ (ins,())

	    driver = do
	    	   outStdLogicVector "i0" (disabledS :: Seq (Enabled U9))

	    dut = do
	    	i0 <- inStdLogicVector "i0"
		let o0 = cir i0
		outStdLogicVector "o0" o0

	-- Shallow always passes, but builds a reference
        test "runlcdBootP" 1000000 dut $ do
	      	   driver
		   inStdLogicVector "o0" :: Fabric (Seq (U5,U18))
		   return (const Nothing)

	return ()

-}
