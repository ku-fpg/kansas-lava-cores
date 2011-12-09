{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, NoMonomorphismRestriction, DeriveDataTypeable, RankNTypes, ImpredicativeTypes #-}
module Main where

import qualified Language.KansasLava as KL
import Language.KansasLava 
import Language.KansasLava.Fabric
import Language.KansasLava.Universal
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

options = Opts { demoFabric = "leds1"            &= help "demo fabric to be executed or built"
               , fastSim = False                &= help "if running board at full speed"
               , beat = (50 * 1000 * 1000)      &= help "approx number of clicks a second"
               , vhdl = False                   &= help "generate VDHL"

               } 
        &= summary "spartan3e-demo: run different examples for Spartan3e"
        &= program "spartan3e-demo"


main = do
        opts <- cmdArgs options
        let nm = demoFabric opts
        case vhdl opts of
          True ->  vhdlUseFabric opts $ example nm
          False -> simUseFabric opts $ example nm

simUseFabric :: Opts -> Spartan3eSimulator () -> IO ()
simUseFabric _ fab =
        Sim.runPolyester 
                (Sim.Fast)
                (50 * 1000 * 1000)
                50
                fab

vhdlUseFabric :: Opts -> Spartan3e () -> IO ()
vhdlUseFabric opts (Spartan3e fab) = do
        kleg <- reifyFabric (compileToFabric fab)
        Board.writeUCF "main.ucf" kleg
        KL.writeVhdlCircuit "main" "main.vhd" kleg
        return ()


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

example
 :: ( DialRotation m
    , LEDs m            , LEDCount m   ~ X8
    , RS232 m           , RS232Count m ~ Serial
    , LCD m             , LCDSize m    ~ (X2,X16)
    , Switches m        , SwitchCount m ~ X4
    ) => String 
    -> m ()
example "leds1" = do
        ls <- leds        
        rot <- dialRotation
        Sim.core "main" $ do
                (sig_in :: REG Bool, number :: EXPR U8) <- dialedValue (0,255)
                
                sig_in <== rot

                SPARK $ \ loop -> do
                        sequence_ [ ls M.! i := OP2 (testABit)
                                                    number
                                                    (OP0 (pureS (fromIntegral i)))
                                  | i <- [minBound..maxBound]
                                  ]
                        GOTO loop

example "leds2" = do
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
                                  | i <- [minBound..maxBound]
                                  ]
                        reg := reg + 1
                        GOTO loop

example "rs232in" = do
        rs232_in  <- rs232rx DCE (115200 * 100)
        lcd_wt <- lcd 
        rot <- dialRotation
        sw <- switches

        Sim.core "main" $ do
                (dialed, view_addr) <- dialedValue (0 :: X256, 62)
                dialed <== rot

                VAR reg :: VAR U8 <- SIGNAL $ var 0
                VAR addr :: VAR X256 <- SIGNAL $ var 0
--                VAR view_addr :: VAR X256 <- SIGNAL $ var 0
                mem :: Memory X256 U8 <- memory

                SPARK $ \ loop -> do
                        ((OP1 (bitNot . isEnabled) rs232_in) :? GOTO loop)
                                ||| writeM mem := tuple2 addr
                                                        (OP1 enabledVal rs232_in)
                        addr := addr + 1
                        -- perhaps signal ?
                        GOTO loop


                let toHex :: (Rep n, Num n) => Signal clk n -> Signal clk U8
                    toHex n = funMap (\ (x :: X16) -> return $ fromIntegral $ ord (hex !! fromIntegral x)) $
                                     (unsigned) n
                    hex = "0123456789ABCDEF"

                let showHex width (row :: EXPR X2,col :: EXPR X16) n = do
                        VAR tmp :: VAR U32 <- SIGNAL $ var 0
                        tmp := n
                        for 0 (width - 1) $ \ (i :: EXPR X16) -> do
                           putAckBox lcd_wt $ tuple2 (tuple2 row (col - i))
                                                     (OP1 toHex (OP1 (`mod` 16) tmp))
                           tmp := OP1 (`div` 16) tmp


                SPARK $ \ loop -> do
                        VAR view_addr' :: VAR X256 <- SIGNAL $ var 0
                        view_addr' := OP1 (unsigned) view_addr * 4

                        showHex 7 (0,6) $ OP1 (unsigned) view_addr'
                        showHex 7 (1,6) $ OP1 (unsigned) (view_addr' + 4)

                        let theRow :: Signal u X8 -> Signal u X2
                            theRow = funMap (return . f) where f n | n < 4     = 0
                                                                   | otherwise = 1
                        let theCol :: Signal u X8 -> Signal u X16
                            theCol = funMap (return . f) where f n = fromIntegral (n `mod` 4) * 2 + 9

                        let theCh  :: Signal u U8 -> Signal u U8
                            theCh = funMap (return . f) where f n | n < 0x20 = 0x2e
                                                                  | n > 0x7e = 0x2e
                                                                  | otherwise = n


                        for 0 7 $ \ (i :: EXPR X8) -> do
                          VAR tmp :: VAR U8 <- SIGNAL $ var 0
                          IF (OP2 (.>.) addr (OP1 (unsigned) view_addr')) (do
                                  readM mem := (OP1 (unsigned) view_addr') ||| tmp := valueM mem
                                  IF (sw ! 0) (do
                                        showHex 2 (OP1 theRow i,OP1 theCol i) (OP1 (unsigned) tmp)
                                      )(do 
                                        putAckBox lcd_wt $ tuple2 (tuple2 (OP1 theRow i) (OP1 theCol i))
                                                                  (OP1 theCh tmp)
                                        putAckBox lcd_wt $ tuple2 (tuple2 (OP1 theRow i) (OP1 theCol i - 1))
                                                            0x20                  
                                      )
                             )( do
                                  putAckBox lcd_wt $ tuple2 (tuple2 (OP1 theRow i) (OP1 theCol i))
                                                            0x20
                                  putAckBox lcd_wt $ tuple2 (tuple2 (OP1 theRow i) (OP1 theCol i - 1))
                                                            0x20                  
                             )
                          view_addr' := OP1 loopingIncS view_addr'

                        GOTO loop
{-

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
-}

tuple2 :: (Rep a, Rep b) => EXPR a -> EXPR b -> EXPR (a,b)
tuple2 = OP2 (curry pack)


ex1 = do
                (reg :: REG Bool,ev :: EVENT Bool) <- mkEnabled
                
                -- These are a and b on the previous cycle
                a :: EXPR Bool <- INPUT (inStdLogic "ROT_A")
                b :: EXPR Bool <- INPUT (inStdLogic "ROT_B")
                
                -- These are a and b on the previous cycle
                VAR a' <- SIGNAL $ var False
                VAR b' <- SIGNAL $ var False

                always $ a' := a
                always $ b' := b

                SPARK $ \ loop -> do
                        -- wait for both to be true
                        ((OP1 bitNot $ OP2 and2 a b) :? GOTO loop)
                                ||| a' :? reg := OP0 high
                                ||| b' :? reg := OP0 low
                        wait <- LABEL
                        ((OP2 or2 a b) :? GOTO wait)
                           ||| GOTO loop


                return $ ev
test = do
        let fab1 = compileToFabric $ do 
                res :: EXPR (Enabled Bool) <- ex1
                o0 :: REG Bool <- OUTPUT (outStdLogicVector "o0")
                o0 <== res

--        k <- reifyFabric fab1
--        print k
        let a, b :: Seq Bool
            a = toS $ map (== '1') $ f 00 $ cycle "0001010111111111111111111111111101000000000000000"
            b = toS $ map (== '1') $ f 10 $ cycle "0001010111111111111111111111111101000000000000000"
            f n xs = replicate n '0' ++ xs

        let (_,[("o0",out)])
                = runFabric fab1 [ ("ROT_A", toUni $ a)
                                 , ("ROT_B", toUni $ b)
                                 ]
        let o0 = fromUni' out :: Seq (Maybe Bool)
           
        print [ x | Just (Just x) <- fromS o0 ]
        