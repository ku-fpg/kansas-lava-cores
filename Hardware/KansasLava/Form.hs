{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp,
    RankNTypes, TypeOperators, NoMonomorphismRestriction  #-}

module Hardware.KansasLava.Form where

import Language.KansasLava as KL
import Data.Sized.Unsigned
import Data.Sized.Ix
import Data.Sized.Matrix as M
import Control.Applicative
import Data.Char
import qualified Data.Bits as B

-- This Form DSL is intended for building simple viewports into 
-- what circuits are doing, for example an LCD display.
-- It assumes an 7-bit ASCII-based memory mapped display,
-- so could also be used by VGA output.

-- The form of DSL is an ordered chain for commands,
-- seperated by $$.

-- Example
--
--	empty $$ string "Kansas Lava" $$ ...
--
{-
data Form ix = Form (forall sig clk . (sig ~ CSeq clk, Clock clk) => 
	   Patch (sig (Enabled (ix,U7)))	(sig (Enabled (ix,U7)))
                 (sig Ack)			(sig Ack))
-}

{-
empty :: forall c sig . (Clock c, sig ~ CSeq c)
      => Patch ()		(sig (Enabled (X32,U7)))
               ()		(sig Ack)
empty = emptyAckPatch

shift :: forall c sig . (Clock c, sig ~ CSeq c)
      =>	Patch (sig (Enabled (X32,U7)))	(sig (Enabled (X32,U7)))
              	      (sig Ack)			(sig Ack)
shift = mapPatch (\ ix_val -> 
			let (ix,val) = unpack ix_val
			in pack (ix-1,val))

char :: forall c sig . (Clock c, sig ~ CSeq c)
     => Char -> Patch (sig (Enabled (X32,U7)))	(sig (Enabled (X32,U7)))
              	      (sig Ack)			(sig Ack)
char ch = shift
	$$ appendPatch (matrix [(maxBound,fromIntegral (ord ch))] :: Matrix X1 (X32,U7))
	$$ fifo2

item :: forall c sig . (Clock c, sig ~ CSeq c)
     => Patch (sig (Enabled U7) :> sig (Enabled (X32,U7)))	(sig (Enabled (X32,U7)))
              (sig Ack :> sig Ack)			(sig Ack)
item =  fstPatch (mapPatch (\ u7 -> pack (maxBound,u7)))
     $$ sndPatch shift
--     $$ fstPatch fifo1
--     $$ sndPatch fifo1
     $$ swapPatch
     $$ mergePatch RoundRobinMerge	-- priority to secondary??
     $$ fifo1

-- single hex character
hex :: forall c sig . (Clock c, sig ~ CSeq c)
     => Patch (sig (Enabled X16) :> sig (Enabled (X32,U7)))	(sig (Enabled (X32,U7)))
              (sig Ack           :> sig Ack)			(sig Ack)
hex = fstPatch (mapPatch (funMap (\ x -> if x >= 0 && x <= 9 
					 then return (0x30 + fromIntegral x)
					 else return (0x41 + fromIntegral x - 10))))
    $$ item

-- print a hex number using n hex-characters
widehex :: forall c sig n x . (Clock c, sig ~ CSeq c, Size x, Integral x)
    	=> Patch (sig (Enabled (Unsigned x)) :> sig (Enabled (X32,U7)))	(sig (Enabled (X32,U7)))
              	 (sig Ack :> sig Ack)					(sig Ack)
widehex = patch w
  where
	-- w is the width in chars
	w :: Int
	w  = 1 + (size (undefined :: x) - 1) `div` 4 
	patch :: Int -> Patch (sig (Enabled (Unsigned x)) :> sig (Enabled (X32,U7)))	(sig (Enabled (X32,U7)))
              		      (sig Ack :> sig Ack)					(sig Ack)
	patch 0 = error "attempting to create a widehex with width 0"
	patch 1 = 
		fstPatch (mapPatch (\ b -> (unsigned) b))  $$	-- implicit modulo 0x10
		hex
	patch n =
		-- U x :> (X32,U7)
		fstPatch dupPatch      $$
		fstPatch (fifo2 `stack` fifo2) $$
		exp2Stack 	       $$
		-- U x :> U x :> (X32,U7)
		sndPatch (fstPatch (mapPatch (\ b -> b `B.shiftR` 4)) $$ patch (n-1)) $$
		-- U x :> (X32,U7)
		patch 1

string [] = nullPatch
string (c:cs) = char c $$ string cs

{-
	$$ string "Hello" at (1,1)
	$$ string "World" at (2,2)
-}
{-
data FromMsg ix 
	= FormWrite ix U7
	| FormDone

Patch (sig (Enabled FormMsg) :> res)	(sig (Enabled FormMsg) :> res)
      (sig Ack		     :> res)	(sig Ack               :> res)

-}
-}

-- | 'shallow_mm' simulates the memory mapped API, by drawing an ASCII picture after each write.
-- Note that this handles 2D devices.
shallow_mm :: forall c sig addr . (Clock c, sig ~ CSeq c, Size addr, Rep addr)
	=> Patch (sig (Enabled (addr,U8)))	[String]
		 (sig Ack)			()
shallow_mm = fromAckBox $$ 
	     forwardPatch (scanl driver screen) $$
	     forwardPatch (fmap (M.toList))
   where
	driver :: Matrix addr Char  -> Enabled (addr,U8) -> Matrix addr Char
	driver m Nothing = m
	driver m (Just (addr,val)) = m // [(addr,chr (fromIntegral val))]

	screen :: Matrix addr Char
	screen = pure ' '

rmDups x (y:ys) | x /= y = y : rmDups y ys
		| otherwise = rmDups x ys
rmDups x [] = error "rmDups"	

main = do
	let message :: [Maybe ((X2,X16),U8)]
	    message = [return ((0,i),fromIntegral (ord c)) | (i,c) <- zip [0..] "Hello, World"]
	let stimulus = unitPatch message $$ toAckBox $$ unitClockPatch
	let strs = runPatch (stimulus $$ shallow_mm)
	let strs' = Prelude.head strs : rmDups (Prelude.head strs) (Prelude.tail strs)

	sequence_
	  [ do putStrLn $"+" ++ take 16 (Prelude.repeat '-') ++ "+"
	       putStrLn $"|" ++ take 16 str ++ "|"
	       putStrLn $"|" ++ take 16 (drop 16 str) ++ "|"
	       putStrLn $"+" ++ take 16 (Prelude.repeat '-') ++ "+"
	  | str <- strs'
	  ]

	return ()


