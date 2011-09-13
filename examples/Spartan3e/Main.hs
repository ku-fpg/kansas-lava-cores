{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction, DeriveDataTypeable #-}
module Main where

import qualified Language.KansasLava as KL
import Language.KansasLava hiding (Fabric)
import Hardware.KansasLava.RS232
import Hardware.KansasLava.FIFO
import Hardware.KansasLava.LCD.ST7066U
import Hardware.KansasLava.Form
import Hardware.KansasLava.Rate

import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Arith
import Data.Sized.Matrix as M
import qualified Data.Default as Default
import System.CPUTime
import Data.Char as C
import Control.Concurrent

import System.Console.CmdArgs as CmdArgs

import qualified Hardware.KansasLava.Boards.Spartan3e as Board 

import qualified Hardware.KansasLava.Simulators.Fabric as Sim

import Hardware.KansasLava.Boards.Spartan3e
import Hardware.KansasLava.Simulators.Spartan3e

-------------------------------------------------
-- Only for Simulator mode


type Fabric = Sim.Fabric
useFabric = simUseFabric :: Opts -> Fabric () -> IO ()

-- Only in VHDL generation mode
{-

type Fabric = KL.Fabric
useFabric = vhdlUseFabric :: Opts -> Fabric () -> IO ()
-}
-------------------------------------------------

data Opts = Opts { demoFabric :: String, fastSim :: Bool, beat :: Integer }
        deriving (Show, Data, Typeable)

options = Opts { demoFabric = "lcd"             &= help "demo fabric to be executed or built"
               , fastSim = False                &= help "if running board at full speed"
               , beat = (50 * 1000 * 1000)      &= help "approx number of clicks a second"
               } 
        &= summary "spartan3e-demo: run different examples for Spartan3e"
        &= program "spartan3e-demo"

main = do
        opts <- cmdArgs options
        let fab = fabric opts (demoFabric opts)
        useFabric opts $ do
                board_init
                fab

-- The simulator's use of the Fabric
simUseFabric :: Opts -> Sim.Fabric () -> IO ()
simUseFabric opts fab = 
        Sim.runFabric (case fastSim opts of
                         True -> Sim.Fast
                         False -> Sim.Friendly) $ do
                 fab

-- The VHDL generators use of the Fabric
vhdlUseFabric :: Opts -> KL.Fabric () -> IO ()
vhdlUseFabric opts fab = do
        kleg <- reifyFabric fab
        Board.writeUCF "demo.ucf" kleg
        KL.writeVhdlCircuit "demo" "demo.vhd" kleg
        return ()

------------------------------------------------------------------------------
-- Sample fabrics

fabric :: (Spartan3e fabric) => Opts -> String -> fabric ()
fabric _ "leds" = do
        sw <- switches
        bu <- buttons
        leds (sw `M.append` bu)


fabric _ "lcd" = do
        runPatch $ neverAckPatch $$ appendPatch msg $$ pulse $$ mm_lcdP
 where
         msg :: Matrix X11 ((X2,X16),U8)
         msg = matrix [((0,i),fromIntegral (ord c))
                      | (c,i) <- zip ("Kansas Lava") [0..]
                      ]
        
fabric _ "rs232out" = do
        runPatch $ cyclePatch msg $$ rs232_txP DCE (115200 * 100)
 where
         msg :: Matrix X95 U8
         msg = matrix [ i
                      | i <- [32..126]
                      ]

        
---------------------------------------------------------------------------------
-- Utilties

pulse :: (sig ~ CSeq c, Clock c, Rep a)
      => Patch (sig (Enabled a)) (sig (Enabled a))
	       (sig Ack)         (sig Ack)
pulse = openPatch $$
	(top `stack` idPatch) $$ 
	zipPatch $$
	mapPatch (\ ab -> snd (unpack ab))
   where
	top = unitPatch (packEnabled (powerOfTwoRate (Witness :: Witness X5)) (pureS ())) $$
	      enabledToAckBox
