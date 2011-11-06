{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module RS232 (tests) where

import Language.KansasLava
import Language.KansasLava.Test
import Hardware.KansasLava.FIFO (fifo)
import Hardware.KansasLava.RS232 (rs232in,rs232out)

import Data.Sized.Unsigned
import Data.Sized.Arith
import Data.Sized.Ix
import Data.Ratio
import System.Random
--import Data.Maybe 
import Debug.Trace
import Data.Word

tests :: TestSeq -> IO ()
tests test = do
        -- testing RS232s

        let clockRate = 50 * 1000
        
        let rs232Test :: Integer -> Rational -> StreamTest U8 U8
            rs232Test baud scale = StreamTest
                        { theStream = 
				  rs232out baud clockRate $$ 
				  forwardP noise $$
				  rs232in baud (floor (toRational clockRate * scale)) $$
				  enabledToAckBox $$
				  fifo (Witness :: Witness X16) low
                        , correctnessCondition = \ ins outs -> 
--                                 trace (show ("cc",length ins,length outs)) $
--                                 trace (show ("ins",map show (take 100 ins))) $
--                                 trace (show ("outs",map show (take 100 outs))) $
                                case () of
                                  () | outs /= take (length outs) ins -> return ("in/out differences: "  
											++ show ins ++ show outs)
                                  () | length outs < count -> return $ "to few transfers (" ++ show (length outs) ++ ")"
                                     | otherwise -> Nothing
			, theStreamTestCount  = count
			, theStreamTestCycles = floor ((fromIntegral clockRate / 4) * (1000 / fromIntegral baud))
                        , theStreamName = "rs232"
                        }
	    count = 20

            noise = id
--		  . fromS 
--		  . toS


        let t :: String -> Integer -> IO ()
            t str baud = sequence_
                [ testStream test (str ++ "/" ++ wib) (rs232Test baud scale)
                | (wib,scale) <- [ ("1",1), ("0.99",0.99), ("1.01",1.01) ]
                ]

        t "100"  100
        t "200"  200
        t "300"  300

	return ()