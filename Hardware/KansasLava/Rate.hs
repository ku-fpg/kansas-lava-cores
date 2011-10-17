{-# LANGUAGE RankNTypes, TypeFamilies, ScopedTypeVariables #-}
-- | The 'Clock' module provides a utility function for simulating clock rate
-- downsampling.
module Hardware.KansasLava.Rate(rate, powerOfTwoRate, rateP) where

import Data.Ratio

import Data.Sized.Unsigned
import Data.Sized.Signed
import Data.Sized.Ix

import Language.KansasLava

-- | 'rate' constructs a stream of enable bits used for clock-rate
-- downsampling. For example, with a rate of n=1/2, every other value in the
-- output stream will be True. If 1/n is not a integer, then the function uses
-- http://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm to approximate the
-- given rate.
rate :: forall x clk . (Clock clk, Size x) => Witness x -> Rational -> (Signal clk Bool)
rate Witness n
  | step > 2^sz = error $ "bit-size " ++ show sz ++ " too small for punctuate Witness " ++ show n
  | n <= 0 = error "can not have rate less than or equal zero"
  | n > 1 = error $ "can not have rate greater than 1, requesting " ++ show n

    -- for power of two, a simple counter works
  | num == 1 && step == 2^sz = runRTL $ do
	count <- newReg (0 :: (Unsigned x))
	CASE [ OTHERWISE $ do count := reg count + 1 ]  -- TODO: fix this
	return  (reg count .==. 0)

  | num == 1 = runRTL $ do
	count <- newReg (0 :: (Unsigned x))
	CASE [ IF (reg count .<. (fromIntegral step - 1)) $
		  count := reg count + 1
	     , OTHERWISE $ do
		  count := 0
	     ]
	return  (reg count .==. 0)

  -- inexact reciprocal, so use Bresenham's to approximate things.
  | otherwise = runRTL $ do
	count <- newReg (0 :: (Unsigned x))
	cut   <- newReg (0 :: (Unsigned x))
	err   <- newReg (0  :: (Signed x))
	CASE [ IF (reg count .<. (fromIntegral step + reg cut - 1)) $
		  count := reg count + 1
	     , OTHERWISE $ do
		  count := 0
		  CASE [ IF (reg err .>. 0) $ do
		            cut := 1
			    err   := reg err + fromIntegral nerr
		        , OTHERWISE $ do
		            cut := 0
			    err   := reg err + fromIntegral perr
			]

	     ]
	return  (reg count .==. 0)

   where sz :: Integer
         sz = fromIntegral (size (error "witness" :: x))
	 num = numerator n
	 dom = denominator n
	 step = floor (1 / n)
	 perr = dom - step       * num
	 nerr = dom - (step + 1) * num

-- | 'powerOfTwoRate' generates a pulse every 2^n cycles, which is often good enough for polling, timeouts, etc.
powerOfTwoRate :: forall x clk . (Clock clk, Size x) => Witness x -> Signal clk Bool
powerOfTwoRate Witness = rate (Witness :: Witness x) (1/(2^(fromIntegral (size (error "Witness" :: x)))))

-- | 'rateP' takes a result from rate, and generates token, one per pulse, with
-- unused tokens being discared.
rateP :: forall c sig . (Clock c, sig ~ Signal c)
	=> sig Bool 
	-> Patch ()	(sig (Enabled ()))
	         ()	(sig Ack)
rateP r = outputP (packEnabled r $ pureS ()) $$ enabledToAckBox
	