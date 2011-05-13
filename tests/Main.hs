{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Language.KansasLava
import Data.Default

import Rate as Rate
import FIFO as FIFO

main :: IO ()
main = do
        let opt = def { verboseOpt = 4  -- 4 == show cases that failed
                      }
        testDriver opt
                [ Rate.tests
                , FIFO.tests
                ]

