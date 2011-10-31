{-# LANGUAGE TypeFamilies, ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types #-}

module Hardware.KansasLava.Random (randomBytes) where 

import Language.KansasLava
import Data.Sized.Unsigned

-- Provides a pseudorandom stream of values.  The distinction between the 
--  pseudorandomsmall and the pseudorandom versions is the maximum output size.
--  The pseudorandom can output up to 32-bits, whereas the pseudorandomsmall 
--  can output a maximum of 8-bits.  This is a Lehmer Random Number Generator, 
--  which is defined by:
--      X(k+1) = [g * X(k)] mod n
--
--  The modulus n should be a prime or power of a prime, the multiplier g 
--  should be of high multiplicative order modulo n, and the seed X(0) should 
--  be coprime to modulus n.  The values we used for multiplier g and modulus 
--  n are given below.
--      g = 127
--      n = 257
--
--  For more info, see:
--      http://en.wikipedia.org/wiki/Lehmer_random_number_generator
--

-- | Provides a pseudorandom stream of values. 
--  On a test using the first 100K bytes, all 256 values occurred with
--  the same probability (390 or 391 times).

randomBytes :: forall c sig . (Clock c, Signal c ~ sig) => sig U8
randomBytes = (unsigned) rs
     where
	rs :: sig U16
	rs = iterateS (\ x -> (127 * x) `mod` 257) 127

