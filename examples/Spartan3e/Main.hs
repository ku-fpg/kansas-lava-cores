{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, DoRec, NoMonomorphismRestriction, DeriveDataTypeable, RankNTypes, ImpredicativeTypes #-}
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

options = Opts { demoFabric = "lambda-bridge"    &= help "demo fabric to be executed or built"
               , fastSim = True                 &= help "if running board at full speed"
               , beat = (50 * 1000 * 1000)       &= help "approx number of clicks a second"
               , vhdl = False                    &= help "generate VDHL"

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
--        debug <- monitor "M1"
        Sim.core "main" $ do
                VAR reg :: VAR U32 <- SIGNAL $ var 0
                SPARK $ \ loop -> do
                        sequence_ [ ls M.! i := OP2 (testABit) 
                                                    reg
                                                    (OP1 (+ pureS (fromIntegral i)) 0)
                                  | i <- [minBound..maxBound]
                                  ]
--                        debug := reg
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
        rs232_in  <- rs232rx DCE (115200 * 100)
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
                        readEnabled rs232_in $ \ e -> ch := e
                        putAckBox wt_rs ch
                        GOTO loop



example "lambda-bridge" = do
        rs232_in  <- rs232rx DCE (115200 * 10)
        lcd_wt <- lcd 
        rot <- dialRotation
        sw <- switches

        debug <- monitor

        Sim.core "main" $ do
                (dialed, view_addr) <- dialedValue (0 :: X63, 62)
                dialed <== rot

                -- The lambda-bridge FIFO 
                (wt_out_fifo, rd_out_fifo :: ReadAckBox U8) <- newAckBox
                lcdHexDump rd_out_fifo view_addr (sw ! 0) lcd_wt 

                VAR reg  :: VAR U8         <- SIGNAL $ var 0

                VAR addr :: VAR X256       <- SIGNAL $ var 0
                mem      :: Memory X256 U8 <- memory
                -- how much space the buffer has (1 always used)

                (inc_p1, dec_p1, rd_p1 :: EXPR X256) <- mkChannel2 $ \ e1 e2 ->
                        let val = register 0
                                 (val + defaultedEnabledVal 0 e1
                                      - defaultedEnabledVal 0 e2)
                        in val

                (inc_p2, dec_p2, rd_p2 :: EXPR X256) <- mkChannel2 $ \ e1 e2 ->
                        let val = register 255
                                 (val + defaultedEnabledVal 0 e1
                                      - defaultedEnabledVal 0 e2)
                        in val
                        
                VAR in_space :: VAR U8 <- SIGNAL $ var 0
                -- how much is read
                VAR out_space :: VAR U8 <- SIGNAL $ var 0


                -- Hack, fill memory with stuff so display does not crash

                SPARK $ \ loop -> do
                  for 0 255 $ \ (i :: EXPR X256) -> do
                        writeM mem := tuple2 i 0x71

                  putAckBox wt_out_fifo 99
                  putAckBox wt_out_fifo 100
                  inc_p1 := 255
                  -- TODO: make a macro
                  stop <- LABEL
                  GOTO stop

{-
                -- accept the RS232 input, and put into a buffer
                SPARK $ \ loop -> do
                        -- TODO: invent macro
                        -- Wait for p2 to not be zero 
                        (OP2 (.==.) rd_p2 0) :? 
                                GOTO loop

                        -- Yes, this can miss things if
                        -- the protocol slows down
                        takeEnabled rs232_in $ \ e -> 
                                writeM mem := tuple2 addr e
                                        ||| dec_p2 := 1
                        addr := addr + 1
                        inc_p1 := 1     -- allow a single char to be read
                        GOTO loop
-}

                -- Box to transmit a read packet size, which passed a CRC check.
                (wt_pkt,rd_pkt :: ReadAckBox X256) <- newAckBox

                SPARK $ \ loop -> do

                        VAR addr       :: VAR X256       <- SIGNAL $ var 0 -- The address of the current char
                        VAR start_addr :: VAR X256       <- SIGNAL $ var 0 -- The address of the start of the packet
                        VAR ch         :: VAR U8         <- SIGNAL $ var 0

                        VAR len        :: VAR U8         <- SIGNAL $ var 0


                        let readChar :: (EXPR U8 -> STMT ()) -> STMT ()
                            readChar = readEnabled rs232_in

                        -- wait for a 0xf0
                        header0 <- LABEL
                        readChar $ \ e -> ch := e
                        (OP2 (./=.) ch 0xf0) :? GOTO header0

                        -- The previous char was the header tag
                        header1 <- LABEL
                        readChar $ \ e -> len := e

                        header2 <- LABEL
                        (OP2 (.==.) len tag) :? GOTO header1
                        (OP2 (.>.) len maxFrameSize) :? GOTO header0

                        -- We ignore the CRC for now
                        headerCRC0 <- LABEL
                        readChar $ \ e -> ch := e
                        (OP2 (.==.) ch tag) :? GOTO header1

                        headerCRC1 <- LABEL
                        readChar $ \ e -> ch := e
                        (OP2 (.==.) ch tag) :? GOTO header1

                        payload <- LABEL
                        addr := start_addr
                        -- TODO: check for fit once

                        for 0 (len + 1) $ \ (i :: EXPR U8) -> do
                                readChar $ \ e -> ch := e
                                IF (OP2 (.==.) ch tag) (do
                                        readChar $ \ e -> ch := e
                                        (OP2 (.==.) ch tag) :? 
                                                (len := ch 
                                                  ||| GOTO header2)
                                   )(do
                                        return ()       -- nothing required
                                   )
                                -- note we write the CRC, but never read it
                                IF(OP2 (.<.) i len)  (do
                                        writeM mem := tuple2 addr ch
                                        addr := OP1 loopingIncS addr
                                    )(do
                                        return ()
                                    )

                        -- reset the starting addresss, and transmit the packet length
                        start_addr := addr
                        putAckBox wt_pkt (OP1 (unsigned) len)

                        GOTO header0
                
                -- handle the packets
                lambdaBridgeARQ debug rd_pkt mem wt_out_fifo
  where
                maxFrameSize = 0xef
                tag          = 0xf0



lambdaBridgeARQ (MONITOR mon) rd_pkt mem wt_out_fifo = do
                (wtDataPkt,rdDataPkt :: ReadAckBox (Bool,U16)) <- newAckBox
                local_in_mem :: Memory X256 U8 <- memory
                local_out_mem :: Memory X256 U8 <- memory

                d0 :: REG U8 <- mon "i1"
                d1 :: REG U9 <- mon "i1"

                --- Now we read the packet, figuring out the headers, etc.
                SPARK $  \ loop -> do
                        VAR addr       :: VAR X256      <- SIGNAL $ var 0 -- The address of the current char
                        VAR len        :: VAR X256      <- SIGNAL $ var 0 -- The length of payload
                        VAR ch         :: VAR U8        <- SIGNAL $ var 0       -- placeholder

--                        d0 := message0 "Hello"

                        -- This is how many bytes have been approved for reading
                        readAckBox rd_pkt $ \ e -> len := e
                                
                                
--                        d0 := message1 "(%d)" len
--                        d0 := 1
                        
                        -- we assume 1 packet per frame right now
                        readMemory mem addr $ \ e -> ch := e
                        addr := OP1 loopingIncS addr

                        -- We pull the rest into a ARQ-local buffer
                        -- TODO: make this 1-per channel, or GC them
                        for 0 (len - 1) $ \ (i :: EXPR X256) -> do
                                -- Stash our packet locally
                                readMemory mem addr $ \ e -> 
                                        writeM local_in_mem := tuple2 i e
                                addr := OP1 loopingIncS addr
                
                        -- we've got a local copy of the frame/packet now,
                        -- so we can ack the message.
                        takeAckBox rd_pkt $ \ _ -> return ()

                        -- Decode the tag byte
                        let ty :: EXPR X4
                            ty = OP1 (unsigned)
                               $ OP1 (\ byte -> byte .&. 0x03)
                               $ ch

                            ab :: EXPR Bool
                            ab = OP1 (\ b -> testABit b 2)
                               $ ch

                            chan :: EXPR U5
                            chan = OP1 (unsigned)
                                 $ OP1 (\ b -> shiftR b 3)
                                 $ ch


                        -- Incomming data packet
                        IF (OP2 (.==.) ty 0x0) (do
                                -- assume chan == 0
                                VAR a1 :: VAR U8 <- SIGNAL $ var 0
                                VAR a2 :: VAR U8 <- SIGNAL $ var 0
                                putAckBox wt_out_fifo 0x55
                                readMemory local_in_mem 0 $ \ e -> a1 := e
                                putAckBox wt_out_fifo a1
                                readMemory local_in_mem 1 $ \ e -> a2 := e
                                putAckBox wt_out_fifo a2
                                -- The rest of the packet is 
                                putAckBox wtDataPkt (tuple2 ab (OP1 bitwise (tuple2 a1 a2)))
                           ) $ return ()

                        -- and read the next frame, please
                        GOTO loop


                
                VAR datPkt :: VAR (Bool,U16) <- SIGNAL $ undefinedVar

                SPARK $ \ startRecv -> do
                    VAR m   :: VAR Bool <- SIGNAL $ var False
                    VAR len :: VAR U16  <- SIGNAL $ var 0
                    rec takeAckBox rdDataPkt $ \ e -> datPkt := e
                        -- TODO: check for out of order number
                        m   := OP1 (fst . unpack) datPkt
                        len := OP1 (snd . unpack) datPkt
                        GOTO recv'dRecv

                        -- variable m is set, which is the same as pid in the model
                        recv'dRecv <- LABEL
                        -- now we assume we need blocking, so send a "block"
--                        putAckBox wt_ack (OP1 (\ ab -> ab `shiftR` 2 .|. 0x2) m)

{-
                        for 2 (OP1 (unsigned) len + 1) $ \ (i :: EXPR X256) -> do
                          VAR ch :: VAR U8 <- SIGNAL $ var 0 
                          readMemory local_in_mem 0 $ \ e -> ch := e
                          putAckBox wt_out_fifo ch
-}
                        stop <- LABEL
                        GOTO stop

                        return ()
                    
                    
                    return ()



{-
                        -- push the packet forward
                        for 1 len $ \ (i :: EXPR X256) -> do
                                readMemory mem addr $ \ e -> ch := e
                                addr := OP1 loopingIncS addr
                                putAckBox wt_out_fifo ch

-}

        

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


{-
                        let readChar :: (EXPR U8 -> STMT ()) -> STMT ()
                            readChar cont = do
                                loop <- LABEL
                                -- wait for something to read
                                (OP2 (.==.) rd_p1 0) :? GOTO loop
                                readM mem := (OP1 (unsigned) addr)
                                        ||| addr := addr + 1
                                        ||| cont (valueM mem)
-}

   
---------------------------------------------------------------     
-- Utilites; to be moved elsewhere.

tuple2 :: (Rep a, Rep b) => EXPR a -> EXPR b -> EXPR (a,b)
tuple2 = OP2 (curry pack)

tuple3 :: (Rep a, Rep b, Rep c) => EXPR a -> EXPR b -> EXPR c -> EXPR (a,b,c)
tuple3 = OP3 (\ a b c -> pack (a,b,c)) 

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
  -> WriteAckBox ((X2, X16), U8)       -- LCD display
  -> STMT ()
lcdHexDump inp view_addr mode lcd_wt = do

                VAR reg :: VAR U8 <- SIGNAL $ var 0
                VAR addr :: VAR X256 <- SIGNAL $ var 0
                mem :: Memory X256 U8 <- memory

                SPARK $ \ loop -> do
                        takeAckBox inp $ \ e -> 
                                writeM mem := tuple2 addr e
                        addr := OP1 loopingIncS addr
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

lcdHexDump2
  :: Memory X256 U8                     -- memory to observe
  -> EXPR X63                           -- line to display
  -> EXPR Bool                          -- the mode (hex vs ascii)
  -> WriteAckBox ((X2, X16), U8)        -- LCD display
  -> STMT ()
lcdHexDump2 mem view_addr mode lcd_wt = do

                VAR reg :: VAR U8 <- SIGNAL $ var 0
                VAR addr :: VAR X256 <- SIGNAL $ var 0

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
                          IF (OP0 high {- OP2 (.>.) addr (OP1 (unsigned) view_addr')-}) (do
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


{-
message0 :: String -> EXPR (Message a)
message0 = undefined
message1 :: String -> EXPR a -> EXPR Message
message1 = undefined
-}