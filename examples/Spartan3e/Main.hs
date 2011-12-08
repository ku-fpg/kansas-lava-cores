{-# LANGUAGE ScopedTypeVariables, TypeFamilies, NoMonomorphismRestriction, DeriveDataTypeable, RankNTypes, ImpredicativeTypes #-}
module Main where

import qualified Language.KansasLava as KL
import Language.KansasLava 
import Language.KansasLava.Wakarusa
import Language.KansasLava hiding (Fabric)
import Hardware.KansasLava.RS232
import Hardware.KansasLava.FIFO
import Hardware.KansasLava.LCD.ST7066U
import Hardware.KansasLava.Text
import Hardware.KansasLava.Rate

import Hardware.KansasLava.Core
import Hardware.KansasLava.Peripherals
--import qualified Hardware.KansasLava.VGA as VGA
--import Hardware.KansasLava.VGA (Attr(..), fg, bg)

import Control.Applicative
import Data.Bits
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Arith
import Data.Sized.Matrix as M
import qualified Data.Default as Default
import System.CPUTime
import Data.Char as C
import Control.Concurrent

import System.Console.CmdArgs as CmdArgs hiding ((:=))

import qualified Hardware.KansasLava.Boards.Spartan3e as Board 

import qualified Hardware.KansasLava.Simulators.Polyester as Sim

import Hardware.KansasLava.Boards.Spartan3e
import Hardware.KansasLava.Simulators.Spartan3e



data Opts = Opts { demoFabric :: String, fastSim :: Bool, beat :: Integer, vhdl :: Bool }
        deriving (Show, Data, Typeable)

options = Opts { demoFabric = "leds"            &= help "demo fabric to be executed or built"
               , fastSim = False                &= help "if running board at full speed"
               , beat = (50 * 1000 * 1000)      &= help "approx number of clicks a second"
               , vhdl = False                   &= help "generate VDHL"

               } 
        &= summary "spartan3e-demo: run different examples for Spartan3e"
        &= program "spartan3e-demo"


main = do
        opts <- cmdArgs options
        Sim.runPolyester 
                (Sim.Friendly)
                (50 * 1000 * 1000)
                50
                (example (demoFabric opts))

-- This takes a left/right command, and outputs a number, based on this 

dialedValue :: forall ix . (Rep ix, Num ix, Ord ix) => (ix,ix) -> STMT (REG Bool, EXPR ix)
dialedValue (lo,hi) = do
        VAR number :: VAR ix <- SIGNAL $ var lo
        (sig_in,sig_out) <- mkEnabled

        let pred_inc =
                OP2 and2 (OP1 enabledVal sig_out) 
                         (OP1 (.<. pureS hi) number)
        let pred_dec =
                OP2 and2 (OP1 (bitNot . enabledVal) sig_out) 
                         (OP1 (.>. pureS lo) number)
        
        let action = (pred_inc :? number := number + 1)
                        |||
                     (pred_dec :? number := number - 1)
        
        always $ (OP1 isEnabled sig_out) :? action

        return (sig_in, number)

(<==) :: Rep a => REG a -> EXPR (Enabled a) -> STMT ()
(<==) reg expr = always $ 
        (OP1 isEnabled expr) :? reg := (OP1 enabledVal expr)

example :: String -> Spartan3eSimulator ()
example "leds" = do
        ls <- leds        
        rot <- dialRotation
        Sim.core "main" $ do
                VAR reg :: VAR U32 <- SIGNAL $ var 0
                VAR off :: VAR X32 <- SIGNAL $ var 0
                                
                (sig_in :: REG Bool, number :: EXPR U5) <- dialedValue (0,20)
                
                sig_in <== rot
                off    <== OP1 (enabledS) (OP1 (unsigned) number)

                SPARK $ \ loop -> do
                        sequence_ [ ls M.! i := OP2 (testABit) 
                                                    reg
                                                    (OP1 (+ pureS (fromIntegral i)) off)
                                  | i <- [0..7]
                                  ]
                        reg := reg + 1
                        GOTO loop



example _ = do
        ls <- leds
--        ss <- switches
        rot <- dialRotation
        lcd_wt <- lcd 

        rs232_in  <- rs232rx DCE (115200 * 100)
        rs232_out <- rs232tx DCE (115200 * 100)

        Sim.core "main" $ do
                VAR reg :: VAR U8 <- SIGNAL $ var 33

{-
                SPARK $ \ loop -> do
                        (OP1 isEnabled rot) :? 
                                (((OP1 enabledVal rot) :? reg := reg + 1)
                                   ||| ((OP1 (bitNot . enabledVal) rot) :? reg := reg - 1)
                                ) ||| GOTO loop
-}
                SPARK $ \ loop -> do
                        sequence_ [ ls M.! i := OP1 (flip testABit $ fromIntegral i) reg
                                  | i <- [0..7]
                                  ]
--                        reg := reg + 1
                        GOTO loop

                SPARK $ \ loop -> do
                        (OP1 isEnabled rs232_in) :? reg := OP1 enabledVal rs232_in
                                ||| GOTO loop

{-
                SPARK $ \ loop -> do
                        putAckBox lcd_wt $ tuple2 (tuple2 0 0) reg
                        reg := reg + 1
                        GOTO loop
-}      

                SPARK $ \ loop -> do
                        putAckBox rs232_out $ 0x31
                        putAckBox rs232_out $ 0x32
                        putAckBox rs232_out $ 0x33
                        putAckBox rs232_out $ 0x34
                        putAckBox rs232_out $ 0x35
                        putAckBox rs232_out $ 0x36
                        putAckBox rs232_out $ 0x10
                        GOTO loop


        Sim.init_board
        return ()


tuple2 :: (Rep a, Rep b) => EXPR a -> EXPR b -> EXPR (a,b)
tuple2 = OP2 (curry pack)

