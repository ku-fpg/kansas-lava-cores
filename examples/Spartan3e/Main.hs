module Main where

import Language.KansasLava
import Hardware.KansasLava.RS232
import Hardware.KansasLava.FIFO
import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Arith

type X512 = ADD X256 X256
type X1024 = ADD X512 X512
type X16K   = MUL X1024 X16

--rs232in >==> fifoZ >~~> bridge >==> rs232out

fabric :: Fabric ()
fabric = do
        in_rs  <- inStdLogic "rs_in" 
        let in_val                     = rs232in baudRate clockRate in_rs
	    rs232in_patch	       = enableToHandShake in_val
	    fifo_patch		       = fifo (Witness :: Witness X16) low 
	    rs232out_patch	       = rs232out (baudRate {- + 2000 -}) clockRate 
	    main_patch		       = rs232in_patch >~=> fifo_patch >~=> rs232out_patch

	    ((),_ :> counter :> out_rs,())  = main_patch ((),())

        outStdLogic "rs_out" (out_rs :: Seq Bool)
----        outStdLogicVector "leds" (latch out_val)
        outStdLogicVector "leds" $ 
--			((unsigned) counter :: Seq U8)
			(0xa6 :: Seq U8)
	return ()

baudRate = 115200
--baudRate   = 117000
clockRate = 50 * 1000 * 1000

main = do
        kleg <- reifyFabric fabric
        writeVhdlCircuit "main" "main.vhd" kleg
        