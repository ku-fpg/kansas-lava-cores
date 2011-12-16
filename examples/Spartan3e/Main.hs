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
import Hardware.KansasLava.Boards.Physical

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

options = Opts { demoFabric = "rs232in"            &= help "demo fabric to be executed or built"
               , fastSim = False                &= help "if running board at full speed"
               , beat = (50 * 1000 * 1000)      &= help "approx number of clicks a second"
               , vhdl = True                    &= help "generate VDHL"

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
simUseFabric opts fab = do
        writeFile "LOG" "-- starting log\n"
        setProbesAsTrace (appendFile "LOG")
        Sim.runPolyester 
                (if fastSim opts then Sim.Fast else Sim.Friendly)
                (50 * 1000 * 1000)
                50
                fab

vhdlUseFabric :: Opts -> Spartan3e () -> IO ()
vhdlUseFabric opts fab = do
        kleg <- reifyFabric $ 
                  do clk <- compileToFabric 
                          $ run_physical 
                          $ do fab 
                               clk_physical
                     theClk clk
        Board.writeUCF "main.ucf" kleg
        KL.writeVhdlCircuit "main" "main.vhd" kleg
        return ()


example
 :: ( DialRotation m
    , LEDs m            , LEDCount m   ~ X8
    , RS232 m           , RS232Count m ~ Serial
    , LCD m             , LCDSize m    ~ (X2,X16)
    , Switches m        , SwitchCount m ~ X4
    , Monitor m
    ) => String 
    -> m ()

example "leds0" = do
        ls <- leds        
        Sim.core "main" $ do
                SPARK $ \ loop -> do
                        ls ! 0 := OP0 high ||| GOTO loop
example "leds1" = do
        ls <- leds        
        debug <- monitor "M1"
        Sim.core "main" $ do
                VAR reg :: VAR U32 <- SIGNAL $ var 0
                SPARK $ \ loop -> do
                        sequence_ [ ls M.! i := OP2 (testABit) 
                                                    reg
                                                    (OP1 (+ pureS (fromIntegral i)) 0)
                                  | i <- [minBound..maxBound]
                                  ]
                        debug := reg
                        reg := reg + 1
                        GOTO loop
example "leds2" = do
        ls <- leds        
        rot <- dialRotation
        Sim.core "main" $ do
                (sig_in :: REG Bool, number :: EXPR U8) <- dialedValue (20,200)
                
                sig_in <== rot

                SPARK $ \ loop -> do
                        sequence_ [ ls M.! i := OP2 (testABit)
                                                    number
                                                    (OP0 (pureS (fromIntegral i)))
                                  | i <- [minBound..maxBound]
                                  ]
                        GOTO loop

example "leds3" = do
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

example "lcd0" = do
        ls <- leds



        Sim.core "main" $ do
                theLCD :: REG (U1,U4,Bool) <- OUTPUT $ \ out -> do
                        let (rs :: Seq U1,sf :: Seq U4,e :: Seq Bool) = unpack (enabledVal out)
                        let sf' :: Matrix X4 (Seq Bool) = unpack (bitwise sf :: Seq (Matrix X4 Bool))
                        let gate :: Seq Bool -> Seq Bool
                            gate = registerEnabled False . packEnabled (isEnabled out)
                        outStdLogic "LCD_RS"   $ gate ((bitwise) rs)
                        outStdLogic "SF_D<11>" $ gate (sf' ! 3)
                        outStdLogic "SF_D<10>" $ gate (sf' ! 2)
                        outStdLogic "SF_D<9>"  $ gate (sf' ! 1)
                        outStdLogic "SF_D<8>"  $ gate (sf' ! 0)
                        outStdLogic "LCD_E"    $ gate e
                        outStdLogic "LCD_RW"   low
                        outStdLogic "SF_CE0"   high

                let waitFor :: EXPR U32 -> STMT ()
                    waitFor n = do 
                            VAR i :: VAR U32 <- SIGNAL $ var 0
                            i := OP1 (\ x -> x - 2) n
                            loop <- LABEL
                            i := i - 1
                                ||| (OP2 (.>.) i 0) :? GOTO loop


                send_nibble <- do
                            (wr :: WriteAckBox (U1,U4,U18),rd :: ReadAckBox (U1,U4,U18)) <- newAckBox
                            VAR rs :: VAR U1 <- SIGNAL $ undefinedVar
                            VAR sf :: VAR U4 <- SIGNAL $ undefinedVar
                            VAR wait :: VAR U18 <- SIGNAL $ undefinedVar
                            SPARK $ \ loop -> do
                                    takeAckBox rd $ \ e -> 
                                                        rs   := OP1 (\e -> case unpack e of (x,_,_) -> x) e 
                                                    ||| sf   := OP1 (\e -> case unpack e of (_,x,_) -> x) e 
                                                    ||| wait := OP1 (\e -> case unpack e of (_,_,x) -> x) e 
                                    theLCD := OP2 (\ a b -> pack (a,b,low)) rs sf
                                    waitFor 2
                                    theLCD := OP2 (\ a b -> pack (a,b,high)) rs sf
                                    waitFor 12
                                    theLCD := OP2 (\ a b -> pack (a,b,low)) rs sf
                                    waitFor (OP1 (unsigned) wait)
                                    GOTO loop

                            return wr

                send_cmd <- do
                            (wr :: WriteAckBox U9,rd :: ReadAckBox U9) <- newAckBox
                            VAR cmd :: VAR U9 <- SIGNAL $ undefinedVar
                            SPARK $ \ loop -> do
                                    takeAckBox rd $ \ e -> cmd := e
                                    let isCmd :: EXPR U1
                                        isCmd = OP1 bitwise $ OP2 testABit cmd 8

                                        low_nibble :: EXPR U4
                                        low_nibble = OP1 (unsigned) cmd

                                        high_nibble :: EXPR U4
                                        high_nibble = OP1 (\ e -> unsigned (shiftR e 4)) cmd

                                        timing = OP3 (\ a b c -> mux a (b,c)) 
                                                        (OP2 (.<=.) cmd 0x03) 
                                                          2000
                                                        100000
                                        
                                    putAckBox send_nibble $ OP2 (\ a b -> pack (a,b,50)) isCmd high_nibble
                                    putAckBox send_nibble $ OP3 (\ a b c -> pack (a,b,c)) isCmd low_nibble timing
                                    GOTO loop

                            return wr

                let issue :: forall clk . REG (U1,U4,Bool) -> (U1,U8) -> STMT ()
                    issue theLCD (isCmd,cmd) = do
                            send theLCD (isCmd,fromIntegral $ (cmd `div` 16) `mod` 16)
                            waitFor 50
                            send theLCD (isCmd,fromIntegral $ cmd `mod` 16)
                            waitFor 2000

                    send :: forall clk . REG (U1,U4,Bool) -> (U1,U4) -> STMT ()
                    send theLCD (rs,sf) = do
                            theLCD := OP0 (pack (fromIntegral rs,fromIntegral sf,low))
                            waitFor 2
                            theLCD := OP0 (pack (fromIntegral rs,fromIntegral sf,high))
                            waitFor 12
                            theLCD := OP0 (pack (fromIntegral rs,fromIntegral sf,low))
                            waitFor 2

                SPARK $ \ loop -> do
--                        rs := OP0 (pack (0,0,False) :: Signal u (U1,U4,Bool))
                        ls ! 0 := OP0 high
                        
                        let bootCmd :: Signal u X4 -> Signal u (U1,U4,U18)
                            bootCmd = funMap (return . f)
                                                where f 0 = (0,3,205000)
                                                      f 1 = (0,3,5000)
                                                      f 2 = (0,3,2000)
                                                      f 3 = (0,2,2000)

                        waitFor 750000
                        for 0 3 $ \ (i :: EXPR X4) -> do
                                putAckBox send_nibble (OP1 bootCmd i)

                        ls ! 1 := OP0 high

                        let startCmd :: Signal u X4 -> Signal u U9
                            startCmd = funMap (return . f)
                                                where f 0 = 0x28
                                                      f 1 = 0x06
                                                      f 2 = 0x0C
                                                      f 3 = 0x01


                        for 0 3 $ \ (i :: EXPR X4) -> do
                                putAckBox send_cmd (OP1 startCmd i)

                        ls ! 2 := OP0 high
                        
                        waitFor 200000

                        (wr :: WriteAckBox ((X2,X16),U8),rd :: ReadAckBox ((X2,X16),U8)) <- newAckBox
                        
                        cmd_loop <- LABEL
                        
                        VAR row :: VAR X2  <- SIGNAL $ undefinedVar
                        VAR col :: VAR X16 <- SIGNAL $ undefinedVar
                        VAR ch  :: VAR U8  <- SIGNAL $ undefinedVar
                        takeAckBox rd $ \ e -> 
                                        row := OP1 (\e -> case unpack e of (x,_) -> fst (unpack x)) e 
                                    ||| col := OP1 (\e -> case unpack e of (x,_) -> snd (unpack x)) e 
                                    ||| ch  := OP1 (\e -> case unpack e of (_,x) -> x) e 


                        putAckBox send_cmd (OP2 (\ r c -> 0x80 + (unsigned r) * 0x40 + (unsigned) c) row col)
                        putAckBox send_cmd (OP1 (+ 0x100) $ OP1 (unsigned) ch)
                        ls ! 3 := OP0 high
                
                        GOTO cmd_loop

                        SPARK $ \ loop -> do
                                putAckBox wr (tuple2 (tuple2 0 1) 0x31)
                                putAckBox wr (tuple2 (tuple2 0 2) 0x32)
                                putAckBox wr (tuple2 (tuple2 0 3) 0x33)
                                putAckBox wr (tuple2 (tuple2 1 4) 0x34)
                                GOTO loop
                

        
example "lcd1" = do
        lcd_wt <- lcd 
        ls <- leds        
        Sim.core "main" $ do
                SPARK $ \ loop -> do
                        ls ! 0 := OP0 high
                        putAckBox lcd_wt (tuple2 (tuple2 0 0) 0x30)
                        ls ! 1 := OP0 high
                        putAckBox lcd_wt (tuple2 (tuple2 0 1) 0x31)
                        putAckBox lcd_wt (tuple2 (tuple2 0 2) 0x32)
                        putAckBox lcd_wt (tuple2 (tuple2 0 3) 0x33)
                        putAckBox lcd_wt (tuple2 (tuple2 0 4) 0x34)
                        ls ! 2 := OP0 high
                        ls ! 0 := OP0 low
                        GOTO loop


example "rs232in" = do
        rs232_in  <- rs232rx DCE (115200) --  * 100)
        lcd_wt <- lcd 
        rot <- dialRotation
        sw <- switches

        Sim.core "main" $ do
                (dialed, view_addr) <- dialedValue (0 :: X63, 62)
                dialed <== rot
                (wt_rs,rd_rs)   <- newAckBox

                lcdHexDump rd_rs view_addr (sw ! 0) lcd_wt 

                SPARK $ \ loop -> do
                        VAR ch :: VAR U8 <- SIGNAL $ var 0
                        takeEnabled rs232_in $ \ e -> ch := e
                        putAckBox wt_rs ch
                        GOTO loop


example "lambda-bridge" = do
        -- Read a packet, please
        return ()


        

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



   
---------------------------------------------------------------     
-- Utilites; to be moved elsewhere.

tuple2 :: (Rep a, Rep b) => EXPR a -> EXPR b -> EXPR (a,b)
tuple2 = OP2 (curry pack)

-- This takes a left/right command, and outputs a number, based on this 

dialedValue :: forall ix . (Rep ix, Num ix, Ord ix) => (ix,ix) -> STMT (REG Bool, EXPR ix)
dialedValue (lo,hi) = do
        VAR number :: VAR ix <- SIGNAL $ var lo
        (sig_in,sig_out) <- mkEnabled

        let pred_inc =
                OP2 and2 (OP1 (enabledVal) sig_out) 
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

lcdHexDump
  :: ReadAckBox U8                              -- incoming bytes
  -> EXPR X63                                   -- line to display
  -> EXPR Bool                                  -- the mode (hex vs ascii)
  -> WriteAckBox ((X2, X16), Unsigned X8)       -- LCD display
  -> STMT ()
lcdHexDump inp view_addr mode lcd_wt = do

                VAR reg :: VAR U8 <- SIGNAL $ var 0
                VAR addr :: VAR X256 <- SIGNAL $ var 0
                mem :: Memory X256 U8 <- memory

                SPARK $ \ loop -> do
                        takeAckBox inp $ \ e -> 
                                writeM mem := tuple2 addr e
                        addr := addr + 1
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
                                  IF (mode) (do
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

takeEnabled :: Rep a => EXPR (Maybe a) -> (EXPR a -> STMT ()) -> STMT ()
takeEnabled inp assign = do
        loop <- LABEL
        ((OP1 (bitNot . isEnabled) inp) :? GOTO loop)
                ||| assign (OP1 enabledVal inp)
