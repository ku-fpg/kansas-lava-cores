module Main where

import Language.KansasLava
import Hardware.KansasLava.RS232
import Hardware.KansasLava.FIFO
import Data.Sized.Ix
import Data.Sized.Unsigned

type X512 = ADD X256 X256
type X1024 = ADD X512 X512


fabric :: Fabric ()
fabric = do
        in_rs  <- inStdLogic "rs_in" 
        let in_val                     = rs232in baudRate clockRate in_rs
            (_,(out_val,counter))      = fifoZ (Witness :: Witness X255) low (in_val,out_ack')
	    (out_ack',out_val')	       = bridge (out_val,out_ready)
            (out_ready,out_rs)         = rs232out (baudRate {- + 2000 -}) clockRate out_val'

        outStdLogic "rs_out" out_rs 
----        outStdLogicVector "leds" (latch out_val)
        outStdLogicVector "leds" ((unsigned) counter :: Seq U8)
	return ()		

baudRate = 115200
--baudRate   = 117000
clockRate = 50 * 1000 * 1000

main = do
        kleg <- reifyFabric fabric
        writeVhdlCircuit "main" "main.vhd" kleg
        