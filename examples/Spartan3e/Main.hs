{-# LANGUAGE ScopedTypeVariables, TypeFamilies, NoMonomorphismRestriction, DeriveDataTypeable, RankNTypes, ImpredicativeTypes #-}
module Main where

import qualified Language.KansasLava as KL
import Language.KansasLava hiding (Fabric)
import Hardware.KansasLava.RS232
import Hardware.KansasLava.FIFO
import Hardware.KansasLava.LCD.ST7066U
import Hardware.KansasLava.Text
import Hardware.KansasLava.Rate
import qualified Hardware.KansasLava.VGA as VGA
import Hardware.KansasLava.VGA (Attr(..), fg, bg)

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

import System.Console.CmdArgs as CmdArgs

import qualified Hardware.KansasLava.Boards.Spartan3e as Board 

import qualified Hardware.KansasLava.Simulators.Polyester as Sim

import Hardware.KansasLava.Boards.Spartan3e
import Hardware.KansasLava.Simulators.Spartan3e



data Opts = Opts { demoFabric :: String, fastSim :: Bool, beat :: Integer, vhdl :: Bool }
        deriving (Show, Data, Typeable)

options = Opts { demoFabric = "lcd_inputs"             &= help "demo fabric to be executed or built"
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
                         False -> Sim.Friendly) 
              (2 * 1000 * 1000)
              (case fastSim opts of
                         True -> 1000
                         False -> 50)
            $ fab

-- The VHDL generators use of the Fabric
vhdlUseFabric :: Opts -> KL.Fabric () -> IO ()
vhdlUseFabric opts fab = do
        kleg <- reifyFabric fab
        Board.writeUCF "main.ucf" kleg
        KL.writeVhdlCircuit "main" "main.vhd" kleg
        return ()


matrixOf :: (Size x) => x -> [a] -> Matrix x a
matrixOf _ = matrix

------------------------------------------------------------------------------
-- Sample fabrics

fabric :: (Spartan3e fabric) => Opts -> String -> fabric ()
fabric _ "leds" = do
        sw <- switches
        bu <- buttons
        leds (sw `M.append` bu)

fabric _ "dial" = do
        d <- dial_button
        r <- dial_rot
        let val :: Seq U4
            val = register 0 $ val + cASE
                [ (isEnabled r .&&. enabledVal r, 1)
                , (isEnabled r .&&. bitNot (enabledVal r), -1)
                ] 0
        let ms :: Matrix X4 (Seq Bool)
            ms = unpack ((bitwise) val :: Seq (Matrix X4 Bool))

        leds (matrix $ [d, low] ++ M.toList ms ++ [low,low])

fabric _ "lcd" = do
        runF $ patchF (neverAckP $$ prependP msg $$ throttleP (powerOfTwoRate (Witness :: Witness X5))) |$| mm_lcdP
 where
         msg :: Matrix X32 ((X2,X16),U8)
         msg = boxU8 ["Example of Using", " the LCD driver "]

fabric _ "lcd_inputs" = do
        sw <- switches
        bu <- buttons
        runF $ patchF (patch sw bu) |$| mm_lcdP
 where
        patch sw bu = emptyP
             $$ forwardP (\ () -> pure ())
             $$ backwardP (const ())
             $$ matrixStackP (forAll $ \ (i::X4) ->
                                    (outputP (changeS (sw M.! i)))
                                 $$ enabledToAckBox
                                 $$ mapP (\ s -> pack (pureS (fromIntegral i + 0), mux s (33,34)))
                             )
             $$ matrixMergeP RoundRobinMerge
             $$ mm_text_driver msg active 
--             $$ throttleP (powerOfTwoRate (Witness :: Witness X1))

        msg :: Matrix (X2,X16) U8
        msg = boxU8' ["EXample of Using", " the LCD driver "]

        active :: X4 -> (X2,X16)
        active x = (0,fromIntegral x)

{-
fabric _ "vga" = do
        runP $ doP $$ mm_vgaP
 where
        doP :: Patch () (Seq (Enabled ((X40,X80),(Attr,U7))))
                     () (Seq Ack)
        doP = undefined
        
--        zipP (cycleP msg)
--                       (

        asciiP :: Patch () (Seq (Enabled U7))
                        () (Seq Ack)
        asciiP = cycleP (matrix [ 33 .. 122 ] :: MSize X91)


        -- just a list of positions
        posP :: Patch () (Seq (Enabled (X40,X80)))
                      () (Seq Ack)
        posP = cycleP (matrix [(a,b) | a <- [0..39], b <- [0..79]] :: Matrix (MUL X80 X40) (X40,X80))

-}

{-
fabric _ "rs232out" = do
        runP $ cycleP msg $$ rs232_txP DCE (115200 * 100)
 where
         msg :: Matrix X95 U8
         msg = matrix [ i
                      | i <- [32..126]
                      ]
-}

fabric _ "rs232in" = do
        rx <- rs232_rxP DCE 115200
{-
        let cps = 1000 * 1000   -- one mill per second
        let ticks = 10
        let speed = cps `div` ticks
        let pow2s = iterate (*2) 1
        let res = head (dropWhile (> speed)
-}
        ticks <- tickTock (Witness :: Witness X16) 6
        runF $ patchF (rx
                 $$ enabledToAckBox
                 $$ fifo1
                 $$ matrixDupP
                 $$ matrixStackP (matrixOf (0 :: X4) 
                        [ hexchain   $$ mapP (startAt 0)
                        , count      $$ mapP (startAt 18)
                        , asciichain $$ mapP (startAt 24)
                        , sinkAckP   $$ 
                          alwaysAckP () $$ 
                          throttleP ticks $$
                          aliveGlyph $$ mapP (startAt 16)
                        ])
                 $$ matrixMergeP RoundRobinMerge
                 $$ mm_text_driver msg active
                 $$ witnessP (Witness :: Witness (Enabled ((X2,X16),U8)))) 
            |$| mm_lcdP
 where
        startAt :: (Size w, Rep w, Rep a, a ~ U8) => Signal clk X32 -> Signal clk (w,a) -> Signal clk (X32,a)
        startAt pos inp = pack (pos + (unsigned) w,a)
             where
                 (w,a) = unpack inp
                 
        hexchain :: Patch (Seq (Enabled U8)) (Seq (Enabled (X16,U8)))
                          (Seq Ack)          (Seq Ack)
        hexchain =
                (mapP (\ ch -> packMatrix (matrixOf (0 :: X2) [hexVal ((unsigned) (ch `shiftR` 4)),hexVal ((unsigned) ch)]))
                     $$ matrixToElementsP
                     $$ scrollBar
                     $$ witnessP (Witness :: Witness (Enabled (X16,U8)))
                     )

        asciichain :: Patch (Seq (Enabled U8)) (Seq (Enabled (X8,U8)))
                          (Seq Ack)          (Seq Ack)
        asciichain =
                (mapP (\ ch -> mux (ch .>=. 32 .&&. ch .<=. 126) ((unsigned) $ pureS (ord '.'),ch))
                     $$ scrollBar
                     $$ witnessP (Witness :: Witness (Enabled (X8,U8)))
                     )
                     
        count :: Patch (Seq (Enabled U8)) (Seq (Enabled (X4,U8)))
                       (Seq Ack)          (Seq Ack)
        count = stateP adder (0 :: U16)
             $$ witnessP (Witness :: Witness (Enabled U16))
             $$ hexForm
             $$ witnessP (Witness :: Witness (Enabled (X4,U8)))

        adder :: forall clk . (Signal clk U16,Signal clk U8) -> (Signal clk U16,Signal clk U16)
        adder (a,_) = (a + 1,a + 1)
{-
                (hexForm
                     $$ 
                     $$ scrollBar
                     $$ witnessP (Witness :: Witness (Enabled (X16,U8)))
                     $$ mapP (startAt 16)
                     )
-}
        hexVal :: Signal clk U4 -> Signal clk U8
        hexVal = funMap (\ x -> if x >= 0 && x <= 9 
                     then return (0x30 + fromIntegral x)
                     else return (0x41 + fromIntegral x - 10))

        witnessP :: (Witness w) -> Patch (Seq w) (Seq w)
                                         (Seq a) (Seq a)
        witnessP _ = emptyP
         
        msg :: Matrix (X2,X16) U8
        msg = boxU8' ["                ","                "]
        
        active :: X32 -> (X2,X16)
        active x = (fromIntegral (x `div` 16),fromIntegral (x `mod` 16))

-- Remember when a value changes.
changeS :: forall c sig a . (Clock c, sig ~ Signal c, Eq a, Rep a) => sig a -> sig (Enabled a)
changeS sig = mux (start .||. diff) (disabledS,enabledS sig)
    where
        start :: sig Bool
        start = probeS "start" $ register True low

        diff :: sig Bool
        diff = probeS "diff" $ sig ./=. delay sig

---------------------------------------------------------------------------------
-- Utilties
{-
type Socket f a b 
              c d = (b,c) -> f (a,d)
              
applyPatch :: Socket fab a b c d -> Patch a b c d -> fab ()
applyPatch = undefined

rs232rx :: (MonadFix f) => Socket f (Seq (Enabled U8))  ()
                                    (Seq Ack)           ()

(|$|) :: Socket f a b c d -> Socket e f g h -> Socket (a
-}

    
-- later, this will use a sub-Clock.

stateP :: forall clk a b c sig . 
          (Rep a, Rep b, Rep c, Clock clk, sig ~ Signal clk)
       => (forall sig' clk' . (sig' ~ Signal clk') => (sig' a,sig' b) -> (sig' a,sig' c))
       -> a
       -> Patch (sig (Enabled b)) (sig (Enabled c))
                (sig Ack)         (sig Ack)
stateP st a = 
        loopP $ 
             fstP (prependP (matrixOf (0 :: X1) [a]))
          $$ zipP
          $$ mapP st'
          $$ unzipP
          $$ fstP (fifo1)
  where
        st' :: forall clk' . Signal clk' (a,b) -> Signal clk' (a,c)
        st' s = pack (st (unpack s) :: (Signal clk' a, Signal clk' c))


