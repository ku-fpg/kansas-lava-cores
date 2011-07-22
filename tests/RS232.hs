{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module RS232 (tests) where

import Language.KansasLava
import Hardware.KansasLava.FIFO (fifo)
import Hardware.KansasLava.RS232 (rs232in,rs232out)

import FIFO hiding (tests)-- reuse the FIFO tester                            

import Data.Sized.Unsigned
import Data.Sized.Arith
import Data.Sized.Ix
import System.Random
--import Data.Maybe 
import Debug.Trace
import Data.Word

tests :: TestSeq -> IO ()
tests test = do
        -- testing RS232s

        let clockRate = 50 * 1000
        
        let baudRate = 1000

        let rs232Test :: Integer -> Rational -> FIFO U8 U8
            rs232Test baud scale = FIFO
                        { theFIFO = \ (en_w,ackOut) ->
                                  let (ackIn,wire) = rs232out baud clockRate en_w
                                      wire'        = noise wire
                                      en_wdOut     = rs232in baud (floor (toRational clockRate * scale)) (wire')
				      (_,en_wdOut') = fifo (Witness :: Witness X16) low (en_wdOut,ackOut)
                                  in (ackIn,en_wdOut')
                        , correctnessCondition = \ ins outs -> 
--                                 trace (show ("cc",length ins,length outs)) $
--                                 trace (show ("ins",map show (take 100 ins))) $
--                                 trace (show ("outs",map show (take 100 outs))) $
                                case () of
                                  () | outs /= take (length outs) ins -> return "in/out differences"
                                  () | length outs < (fromIntegral baud `div` 10) -> return $ "to few transfers (" ++ show (length outs) ++ ")"
                                     | otherwise -> Nothing
                        , theFIFOName = "rs232"
                        }

            noise = id


        let t :: String -> Integer -> IO ()
            t str baud = sequence_
                [ testFIFO test (str ++ "/" ++ wib) (rs232Test baud scale) (dubGen arbitrary :: Gen (Maybe U8))
                | (wib,scale) <- [ ("1",1), ("0.99",0.99), ("1.01",1.01) ]
                ]

        t "1000"  1000
        t "2000"  2000
        t "3000"  3000
