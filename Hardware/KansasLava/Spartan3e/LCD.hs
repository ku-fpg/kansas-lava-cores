{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module Hardware.KansasLava.LCD where

import Language.KansasLava as KL
import Data.Sized.Unsigned
import Data.Sized.Ix
import Data.Sized.Matrix

lcdDriver :: (Clock c, sig ~ CSeq c)
	  => Patch (sig (Enabled U9))	(sig (U1,U4,Bool))
	 	   (sig Ack)		()
lcdDriver = lcdBootPatch $$ phyLCDPatch

waitFor :: (Rep b, Num b) => Reg s c b -> CSeq c b -> RTL s c () -> RTL s c ()
waitFor counter count nextOp = do
	CASE [ IF (reg counter ./=. count) $ do
			counter := reg counter + 1
  	-- wait for 15ms (750,000 cycles)
             , OTHERWISE $ do
			counter := 0
			nextOp
	     ]

splitCmd :: Comb U9 -> Comb (Matrix X2 (U5,U18))
splitCmd cmd = pack $ matrix 
	[ pack ( high_op `KL.append` mode
	       , smallGap
	       )
	, pack ( low_op `KL.append` mode
	       , mux2 (cmd .<=. 0x03) (hugeGap,bigGap)
	       )
	]
    where
	(op :: Comb U8, mode :: Comb U1) = factor cmd
	(low_op :: Comb U4, high_op :: Comb U4) = factor op

	smallGap = 50		-- between nibbles
	bigGap   = 2000		-- between commands
	hugeGap	 = 100000	-- after clear display or return cursor home


lcdBootPatch :: forall c sig . (Clock c, sig ~ CSeq c)
	=> Patch (sig (Enabled U9))	(sig (Enabled (U5,U18)))
		 (sig Ack)		(sig Ack)
lcdBootPatch = appendPatch initCmds $$ toCmds $$ appendPatch bootCmds
   where
	toCmds = mapPatch splitCmd $$ matrixExpandPatch

	bootCmds :: Matrix X4 (U5,U18)
	bootCmds = matrix 
		[ (0x3, 205000)
		, (0x3, 5000)
		, (0x3, 2000)
		, (0x2, 2000)
		] 

	initCmds :: Matrix X4 U9
	initCmds = matrix [ 0x28, 0x06, 0x0C, 0x1
	 		  ]

-- The physical driver for the LCD patch
--  input: RS+nibble (5bits) and pause length in cycles
-- output: RS, SF_D[11:8], LCD_E
-- assuming LCD_RW is set always low

phyLCDPatch :: forall c sig . (Clock c, sig ~ CSeq c)
	=> Patch (sig (Enabled (U5,U18)))	(sig (U1,U4,Bool))
		 (sig Ack)			()
phyLCDPatch ~(inp,_) = (toAck inAck,out)
   where

	(inAck,out) = runRTL $ do
		state   <- newReg (5 :: X6)
		pause   <- newReg (0 :: U18)
		counter <- newReg (0 :: U20)
		ack     <- newReg False
		rs      <- newReg (0 :: U1)
		sf_d    <- newReg (0 :: U4)
		lcd_e   <- newReg False 

		let wait = waitFor counter
		
		let firstWait = 750000


		CASE [ IF (reg state .==. 0 .&&. isEnabled inp) $ do
			-- waiting for input
			ack := pureS True
			let (cmd' :: sig U5,pause' :: sig U18) = unpack (enabledVal inp)
			let (sf_d':: sig U4,rs' :: sig U1) = factor cmd'
			pause := pause'
			rs    := rs'
			sf_d  := sf_d'
			state := 1
		     , IF (reg state .==. 1) $ do
			wait 2 $ state := 2
		     , IF (reg state .==. 2) $ do
		 	lcd_e := comment "lcd_e := high" high
			wait 12 $ state := 3
		     , IF (reg state .==. 3) $ do
		 	lcd_e := comment "lcd_e := low" low
			state := 4
			wait 1 $ state := 4
		     , IF (reg state .==. 4) $ do
			wait ((unsigned) (reg pause)) $ state := 0
		     , IF (reg state .==. 5) $ do
			wait firstWait $ state := 0
		     ]

		-- Ack for one cycle only
		CASE [ IF (reg ack .==. high) $ do
			ack  := pureS False
		     ]

--		DEBUG "state" state
{-
			  wait 750000 $ state := 1
		     , IF (reg state .==. 1) $ do
			  output := pureS (Just 
		     ]
-}
		return (comment "ack" (var ack),pack (reg rs,reg sf_d,comment "lcd_e" $ reg lcd_e))

{-
circuit :: (Seq U1,Seq U4,Seq Bool)
circuit = unpack res
   where
	inp :: Seq (Enabled U9)
	inp = disabledS
	(_,res) = (lcdBootPatch $$ phyLCDPatch) (inp,())
		

main = do
	let (rs,sf_d,e) = circuit
	
	let fabric = do
		outStdLogic "LCD_RS" rs
		outStdLogicVector "LCD_SF_D" sf_d
		outStdLogic "LCD_E"  e
		return ()

	kleg <- reifyFabric fabric

	writeVhdlCircuit "main" "main.vhd" kleg
{-

	let xs :: Patch () (Seq (Enabled (U5,U18)))
		        () (Seq Ack)
	    xs = unitPatch (map (\ x -> Just (x,50)) [10..]) $$ toAckBox $$ unitClockPatch

   	print $ take 100 $ fromSeq $ runPatch (xs $$ phyLCDPatch)


	let zs = unitPatch (map Just [10..]) $$ toAckBox' [0..] $$ unitClockPatch

	print $ take 100 $ runPatch (zs $$ appendPatch (matrix [1..4] :: Matrix X4 U8) $$ fromAckBox' [0..])
	print ()

-}

	return ()
-}