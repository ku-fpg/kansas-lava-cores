{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Language.KansasLava
import Data.Default

import Rate as Rate
import FIFO as FIFO
import RS232 as RS232
import Chunker as Chunker
import LCD as LCD

main :: IO ()
main = do
        let opt = def { verboseOpt = 4  -- 4 == show cases that failed
	    	      , genSim     = True
--                      , testOnly = return ["fifo/rs232"]
--		      , testOnly = return ["fifo/vanilla/1/U5"]
                      }
        testDriver opt $ take 5 $ drop 0
                [ Rate.tests
                , FIFO.tests
                , RS232.tests 
                , Chunker.tests
		, LCD.tests
                ]

