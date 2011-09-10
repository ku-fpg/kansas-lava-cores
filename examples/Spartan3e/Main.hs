{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction #-}
module Main where

import Language.KansasLava as KL
import Hardware.KansasLava.RS232
import Hardware.KansasLava.FIFO
import Hardware.KansasLava.LCD.ST7066U
import Hardware.KansasLava.Form
import Hardware.KansasLava.Rate

import Data.Sized.Ix
import Data.Sized.Unsigned
import Data.Sized.Arith
import Data.Sized.Matrix
import Data.Default
import System.CPUTime
import Data.Char as C
import Control.Concurrent

import Hardware.KansasLava.Simulators.Spartan3e as Board 
import Hardware.KansasLava.Simulators.Fabric    as Sim

type X512 = ADD X256 X256
type X1024 = ADD X512 X512
type X16K   = MUL X1024 X16

{-
circuit :: (sig ~ CSeq c, Clock c, c ~ ())
	=> Patch () 	(sig (U1,U4,Bool))
	         ()	()
circuit = pulse $$ appendPatch msg $$ lcdDriver
  where
	msg :: Matrix X38 U9
	msg = matrix
	    [ 0x80	-- set write address to start
	    , 0x06	-- cursor, no blinking

	    , 0x40	-- set CG addr to 0
	    , 0x10a	-- 
	    , 0x41	-- set CG addr to 0
	    , 0x115	-- 
	    , 0x42	-- set CG addr to 0
	    , 0x10a	-- 
	    , 0x43	-- set CG addr to 0
	    , 0x115	-- 
	    , 0x44	-- set CG addr to 0
	    , 0x10a	-- 
	    , 0x45	-- set CG addr to 0
	    , 0x115	-- 
	    , 0x46	-- set CG addr to 0
	    , 0x10a	-- 
	    , 0x47	-- set CG addr to 0
	    , 0x100	--

	    , 0x80	-- write into DD ram

	    , 0x100	-- write the special char

	    , 0x40	-- set CG addr to 0
	    , 0x10f	-- 
	    , 0x41	-- set CG addr to 0
	    , 0x115	-- 
	    , 0x42	-- set CG addr to 0
	    , 0x10f	-- 
	    , 0x43	-- set CG addr to 0
	    , 0x115	-- 
	    , 0x44	-- set CG addr to 0
	    , 0x10f	-- 
	    , 0x45	-- set CG addr to 0
	    , 0x115	-- 
	    , 0x46	-- set CG addr to 0
	    , 0x10a	-- 
	    , 0x47	-- set CG addr to 0
	    , 0x100	--

	    , 0x82	-- write into DD ram

	    , 0x100	-- write the special char


--	    , 0x07	-- move display
--	    , 0x18	-- shift to the *LEFT*
	    ]

pulse :: (sig ~ CSeq c, Clock c)
      => Patch () (sig (Enabled U9))
	       () (sig Ack)
pulse = openPatch $$
	(top `stack` bottom) $$ 
	zipPatch $$
	mapPatch (\ ab -> snd (unpack ab))
   where
	top = unitPatch (packEnabled (powerOfTwoRate (Witness :: Witness X25)) (pureS ())) $$
	      enabledToAckBox
	bottom = cyclePatch (matrix (map ((+ 0x100) . fromIntegral . C.ord) msg) :: Matrix X19 U9)

	msg = "Kansas Lava rocks! "


diff :: (Eq a) => [a] -> [(Integer,a)]
diff (x:xs) = f 0 x xs
  where
	f n x (x':xs)
	  | x /= x' = (n,x') : f (n+1) x' xs
	  | otherwise = f (n+1) x xs
	f _ _ [] = []

main = do
	let (_,res) = execPatch circuit ((),())

	let (rs,sf_d,e) = unpack res


	let fabric = do
		theClk "CLK_50MHZ"
		theRst "ROT_CENTER"

		outStdLogic "LCD_RS" rs
		outStdLogicVector "SF_D" (KL.append (0 :: Seq (U8)) sf_d  :: Seq U12)
		outStdLogic "LCD_E"  e

		outStdLogic "LCD_RW" low
		outStdLogic "SF_CE0" high
		return ()

	kleg <- reifyFabric fabric

	kleg' <- optimizeCircuit def kleg
	writeVhdlCircuit "main" "main.vhd" kleg
-}

-- New Example

-- spiny thingy
--example :: (Clock c, sig ~ CSeq c) 
---        => Patch () (sig (Enabled (X1, U8))) 
--	         () (sig Ack)

example1 = ratePatch (powerOfTwoRate (Witness :: Witness X2)) $$ aliveForm
example2 = ratePatch (powerOfTwoRate (Witness :: Witness X3)) $$ aliveForm
example3 = ratePatch (powerOfTwoRate (Witness :: Witness X4)) $$ aliveForm
example4 :: Patch () (Seq (Enabled (X4,U8))) () (Seq Ack)
example4 = unitPatch [ return n | n <- [0..]] $$ toAckBox $$ hexForm

message :: Matrix (X2,X16) U8
message = 
	matrix $ 
	map (fromIntegral . ord) $ 
	"Kansas Lava.... " ++
	"                "

f :: X5 -> (X2,X16)
f 0 = (0,15)
f 1 = (1,0)
f 2 = (1,1)
f 3 = (1,2)
f 4 = (1,3)

type P = Patch ()  (Seq (Enabled (X5, U8)))
               ()  (Seq  Ack)

main2 = do
	let p1 =
		matrixStack
		 ( matrix 
			[ example1 $$ pos 0
--			, example2 $$ pos 1 
--			, example3 $$ pos 2 
			, example4 $$ pos 1
			]
		   :: Matrix X2 P
		) $$
		matrixMergePatch PriorityMerge $$
		mm_driver message f $$ 
		unitClockPatch $$
		shallow_mm
		
	let os = map (frame (2,16)) $ runPatch (p1)

	sequence_ [ do
		putStrLn $ o 
		threadDelay (1000)
	   | o <- os ]
		
main = do
	let u8s :: Seq U8
	    u8s = toSeq (cycle ( [0..255]))

 	    m8 :: Matrix X8 (Seq Bool)
    	    m8 = forAll $ \ i -> testABit u8s (fromIntegral i)

            p1 =
		matrixStack
		 ( matrix 
			[ example1 $$ pos 0
--			, example2 $$ pos 1 
--			, example3 $$ pos 2 
			, example4 $$ pos 1
			]
		   :: Matrix X2 P
		) $$
		matrixMergePatch PriorityMerge $$
		mm_driver message f $$ 
                mm_lcdPatch


	let fab = do    board_init
                        (a,e_b) <- dial

	                bs <- switches

                        let ((),e_r) = enabledToAckBox (e_b,toAck low)


                        leds (matrix [a,isEnabled e_b, enabledVal e_b,low
                                     ,isEnabled e_r,enabledVal e_r,low,low
                                     ])

--                        runPatch (alwaysAckPatch ((0,0),33) $$ mm_lcdPatch)
                        runPatch p1

                        buttons
                        
                        runPatch (
                                unitPatch (map Just [33..126] ++ Prelude.repeat Nothing) $$
                                toAckBox $$
                                rs232_dce_tx 115200)

                        rs232_dce_rx (115200 * 100)

                        dial

	                showClock 1000
	Sim.runFabric fab

	