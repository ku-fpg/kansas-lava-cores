{-# LANGUAGE RankNTypes, TypeFamilies, ScopedTypeVariables #-}
-- | The 'Clock' module provides a utility function for simulating clock rate
-- downsampling.
module Hardware.KansasLava.Rate(rate, powerOfTwoRate, rateP, throttleP) where

import Data.Ratio

import Data.Sized.Unsigned
import Data.Sized.Signed
import Data.Sized.Ix

import Language.KansasLava as KL hiding ((:=), var, IF)
import Language.KansasLava.RTL

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
	count := reg count + 1
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

-- | 'throttleP' throttles input based on a given rate counter.
throttleP :: forall sig c a x . (sig ~ Signal c, Clock c, Rep a)
      => sig Bool
      -> Patch (sig (Enabled a)) (sig (Enabled a))
	       (sig Ack)         (sig Ack)
throttleP in_pred
      = openP $$
	(top `stackP` emptyP) $$ 
	zipP $$
	mapP (\ ab -> snd (unpack ab))
   where
	top = outputP (packEnabled in_pred (pureS ())) $$
	      enabledToAckBox

{-
-- Wrong, omit for this release.
--
-- | 'accurateTo' rounds up/down a number within a range, 
-- in an attempt to be a integral reciprical (and therefore cheaper to implement in hardware).
--accurateTo :: Rational -> Rational -> Rational
accurateTo n ac
        | diff > (1-ac) = error $ "can not find tolerance for "
                               ++ show n ++ " : need " ++ show (fromRational (1 - diff) :: Float)
        | otherwise  = nR
  where
        reci = 1 / n
        nR = 1 /  (fromInteger $ round reci)
        diff   = abs (n - nR)
-}

        