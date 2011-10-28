{-# LANGUAGE TypeFamilies, ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types, TemplateHaskell #-}

module Hardware.KansasLava.RS232 (rs232out, rs232in) where -- , liftWithUART) where

import Data.Ratio

import Data.Sized.Unsigned
import Data.Sized.Signed
import Data.Sized.Ix
import Data.Sized.Unsigned as U
import Data.Sized.Matrix as M

import Hardware.KansasLava.Rate
import Hardware.KansasLava.FIFO(fifo)

import Language.KansasLava 
import qualified Language.KansasLava as KL
import Data.Maybe as Maybe
import Data.Char as Char
import Control.Monad	
import Data.Default
import Data.Word
import Debug.Trace


-- Lava implementation of RS232	

type SAMPLE_RATE = X16

data RS232_TX
	= TX_Idle
	| TX_Send X10
	deriving (Show,Eq,Ord)

isTX_Idle :: (sig ~ Signal c) => sig RS232_TX -> sig Bool
isTX_Idle = funMap $ \ tx -> return $ tx == TX_Idle

withTX_Send :: (sig ~ Signal c) => sig RS232_TX -> sig (Enabled X10)
withTX_Send = funMap $ \ tx -> return $ case tx of
		TX_Send i -> Just i
		_         -> Nothing


instance BitRep RS232_TX where
    bitRep =
	[ (TX_Idle, 	0) ] ++
	[ (TX_Send v, 	fromIntegral v + 1) | v <- [0..9] ]


$(repBitRep ''RS232_TX 4)

{-
-- Template Haskell would help here.
fromRS232_TX :: RS232_TX -> X11
fromRS232_TX TX_Idle = 0
fromRS232_TX (TX_Send n) = fromIntegral n + 1

toRS232_TX :: X11 -> RS232_TX
toRS232_TX 0 = TX_Idle
toRS232_TX n = TX_Send (fromIntegral (n - 1))

instance Rep RS232_TX where
    data X RS232_TX    		= X_RS232_TX (Maybe RS232_TX)
    type W RS232_TX             = X4
    unX (X_RS232_TX v) 		= v
    optX b           		= X_RS232_TX b
    repType Witness     	= repType (Witness :: Witness X11)
    toRep (X_RS232_TX v)	= toRep (optX (fmap fromRS232_TX v))
    fromRep v			= X_RS232_TX (fmap toRS232_TX (unX (fromRep v)))
    showRep (X_RS232_TX v)	= show v
-}

(.*&.) :: (sig ~ Signal c, Rep a) => sig (Enabled a) -> sig Bool -> sig (Enabled a)
(.*&.) en_a bool = packEnabled (en .&&. bool) a
  where
	(en,a) = unpackEnabled en_a

resize :: (sig ~ Signal c, Integral x, Rep x, Num y, Rep y) => sig x -> sig y
resize = funMap $ \ x -> return (fromIntegral x)

findBit :: forall sig c . (sig ~ Signal c) => (Num (sig X10)) => sig U8 -> sig X10 -> sig Bool
findBit byte x = (bitwise) byte .!. ((unsigned) (loopingDecS x) :: sig X8)

rs232out :: forall clk sig a . (Clock clk, sig a ~ Signal clk a)
	=> Integer			-- ^ Baud Rate.
	-> Integer			-- ^ Clock rate, in Hz.
        -> Patch (sig (Enabled U8)) 	(sig Bool)
		 (sig Ack)		()
rs232out baudRate clkRate ~(inp0,()) = (toAck (ready .&&. in_en),out)
  where
	-- at the baud rate for transmission
	fastTick :: Signal clk Bool 
    	fastTick = rate (Witness :: Witness X16) (fromIntegral baudRate / fromIntegral clkRate)

    	(in_en,in_val) 	= unpack inp0

    	(ready,out) = runRTL $ do
--		readVal <- newArr (Witness :: Witness X10)
		state  <- newReg (TX_Idle       :: RS232_TX)
		char   <- newReg (0     	:: U8)
		output <- newReg (True		:: Bool)	-- RS232, SPACE => high

--		DEBUG "state" state

		let ready = isTX_Idle (reg state)

		CASE [ IF (ready .&&. in_en) $ do
			state := pureS (TX_Send 0)	-- causes full to be set on next clock
			char  := in_val
		     ]

		WHEN fastTick $ CASE
		     [ match (withTX_Send (reg state)) $ \ ix -> do
			CASE [ IF (ix .==. maxBound) $ do
				state  := pureS TX_Idle
			     , OTHERWISE $ do
				state := funMap (\ x -> if x == maxBound 
							then return (TX_Send 0)
							else return (TX_Send (x + 1))) ix
			     ]
			CASE [ IF (ix .==. 0) $ do
				output := low	-- start bit
			     , IF (ix .==. 9) $ do
				output := high	-- stop bit
			     , OTHERWISE $ do
				output := findBit (reg char) ix
			     ]
		     ]

		-- We need to use 'var accept', because we need to accept the
		-- the on *this* cycle, not next cycle.
		return (ready,reg output)


-- | rs232in accepts data from UART line, and turns it into bytes.
--   There is no Ack or Ready, because there is no way to pause the 232.
--   For the same reason, this does not use a Patch.

rs232in :: forall clk sig a . (Clock clk, sig a ~ Signal clk a) 
	=> Integer			-- ^ Baud Rate.
	-> Integer			-- ^ Clock rate, in Hz.
	-> Patch (sig Bool)  (sig (Enabled U8))
		 ()	     ()
rs232in baudRate clkRate ~(in_val0,()) = ((),out)
  where
	-- 16 times the baud rate for transmission,
	-- so we can spot the start bit's edge.
	fastTick :: Signal clk Bool 
	fastTick = rate (Witness :: Witness X16) $
                        accurateTo (16 * fromIntegral baudRate / fromIntegral clkRate)
                                   0.99
	

        -- the filter, currently length 4
--        in_vals = in_val0 : map (register True) (take 4 in_vals)
        
	-- if 4 highs (lows) then go high (low), otherwise as you were.

        inp = in_val0
{-
        inp = register True 
                        (cASE [ (foldr1 (.&&.) in_vals, high)
                              , (foldr1 (.&&.) (map bitNot in_vals), low)
                              ]
                         inp)
-}
	findByte :: [sig Bool] -> sig U8
	findByte xs = bitwise (pack (matrix xs :: M.Matrix X8 (sig Bool)) :: sig (M.Matrix X8 Bool))

	out = runRTL $ do
		reading <- newReg False
		theByte <- newArr (Witness :: Witness X16)
		outVal  <- newReg (Nothing :: Enabled U8)
		ready	<- newReg (False :: Bool)
		counter <- newReg (0 :: U8)

		let lowCounter, highCounter :: sig U4
		    (lowCounter,highCounter) = unappendS (reg counter)

		WHEN fastTick $ do
	 		CASE [ IF ((reg reading .==. low) .&&. (inp .==. low)) $ do
				counter := 0
				reading := high
                                        -- check to see the edge *is* an edge
--                             , IF ((reg counter .>. 0) .&&. (reg counter .<. 8) .&&. (inp .==. high)) $ do
--				counter := 0
--				reading := low
			     , OTHERWISE $ do
				counter := reg counter + 1
			     ]
			
			-- We have a 3 sample average, so we wait an aditional 5
			-- to be in the middle of the 16-times super-sample.
			-- So, 5 is 16 / 2 - 3
			WHEN ((reg reading .==. high) .&&. (lowCounter .==. 8)) $ CASE 
			     [ IF (highCounter .<. 9) $ do
				theByte ((unsigned) highCounter) := inp
			     , IF ((highCounter .==. 9) .&&.
				   (reg (theByte 0) .==. low) .&&.
				   (inp .==. high)
				  ) $ do
				-- This should be the stop bit
				outVal := enabledS
					$ findByte [ reg (theByte (fromIntegral i))
						   | i <- [1..8]
						   ]
                                -- start looking for the start bit now
                                counter := 0
                                reading := low
			     , OTHERWISE $ do
				-- restart; should never happen with good signals
                                counter := 0
				reading := low
			     ]

		-- If you send something out, then do not do so on the next cycle.
		WHEN (isEnabled (reg outVal)) $ do
			outVal := pureS Nothing

		return $ (reg outVal)

