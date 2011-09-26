{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction, DeriveDataTypeable, RankNTypes, ImpredicativeTypes #-}
module Main where

import qualified Language.KansasLava as KL
import Language.KansasLava hiding (Fabric)
import Hardware.KansasLava.RS232
import Hardware.KansasLava.FIFO
import Hardware.KansasLava.LCD.ST7066U
import Hardware.KansasLava.Form
import Hardware.KansasLava.Rate
import qualified Hardware.KansasLava.VGA as VGA
import Hardware.KansasLava.VGA (Attr(..), fg, bg)

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

import qualified Hardware.KansasLava.Simulators.Polyester as Sim

import Hardware.KansasLava.Boards.Spartan3e
import Hardware.KansasLava.Simulators.Spartan3e

-------------------------------------------------
-- Only for Simulator mode


useFabric = simUseFabric :: Opts -> Sim.Polyester () -> IO ()

-- Only in VHDL generation mode
{-

type Fabric = KL.Fabric
useFabric = vhdlUseFabric :: Opts -> Fabric () -> IO ()
-}
-------------------------------------------------

data Opts = Opts { demoFabric :: String, fastSim :: Bool, beat :: Integer, vhdl :: Bool }
        deriving (Show, Data, Typeable)

options = Opts { demoFabric = "lcd"             &= help "demo fabric to be executed or built"
               , fastSim = False                &= help "if running board at full speed"
               , beat = (50 * 1000 * 1000)      &= help "approx number of clicks a second"
               , vhdl = False                   &= help "generate VDHL"

               } 
        &= summary "spartan3e-demo: run different examples for Spartan3e"
        &= program "spartan3e-demo"


main = do
        opts <- cmdArgs options
        let fab :: (Spartan3e fabric) => fabric () 
            fab = do
                board_init
                fabric opts (demoFabric opts)

        case vhdl opts of
          True ->  vhdlUseFabric opts fab
          False -> simUseFabric opts fab


-- The simulator's use of the Fabric
simUseFabric :: Opts -> Sim.Polyester () -> IO ()
simUseFabric opts fab = 
        Sim.runPolyester (case fastSim opts of
                         True -> Sim.Fast
                         False -> Sim.Friendly) $ do
                 fab

-- The VHDL generators use of the Fabric
vhdlUseFabric :: Opts -> KL.Fabric () -> IO ()
vhdlUseFabric opts fab = do
        kleg <- reifyFabric fab
        Board.writeUCF "main.ucf" kleg
        KL.writeVhdlCircuit "main" "main.vhd" kleg
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
        msg :: Matrix X30 ((X2,X16),U8)
        msg = matrix $
                      [((0,i),fromIntegral (ord c))
                      | (c,i) <- zip ("Example of using") [0..]
                      ] ++
                      [((1,i),fromIntegral (ord c))
                      | (c,i) <- zip ("the LCD driver") [1..]
                      ]
{-
fabric _ "vga" = do
        runPatch $ doP $$ mm_vgaP
 where
        doP :: Patch () (Seq (Enabled ((X40,X80),(Attr,U7))))
                     () (Seq Ack)
        doP = undefined
        
--        zipPatch (cyclePatch msg)
--                       (

        asciiP :: Patch () (Seq (Enabled U7))
                        () (Seq Ack)
        asciiP = cyclePatch (matrix [ 33 .. 122 ] :: MSize X91)


        -- just a list of positions
        posP :: Patch () (Seq (Enabled (X40,X80)))
                      () (Seq Ack)
        posP = cyclePatch (matrix [(a,b) | a <- [0..39], b <- [0..79]] :: Matrix (MUL X80 X40) (X40,X80))

-}


{-

        msg :: Matrix X30 ((X40,X80),(Attr,U7))
        msg = matrix $
                      [((0,i),(Attr { bg = VGA.White, fg = VGA.Black },fromIntegral (ord c)))
                      | (c,i) <- zip ("Example of using") [0..]
                      ] ++
                      [((1,i),(Attr { bg = VGA.White, fg = VGA.Red },fromIntegral (ord c)))
                      | (c,i) <- zip ("the VGA driver") [1..]
                      ]
-}
        
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
	top = unitPatch (packEnabled (powerOfTwoRate (Witness :: Witness X1)) (pureS ())) $$
	      enabledToAckBox
