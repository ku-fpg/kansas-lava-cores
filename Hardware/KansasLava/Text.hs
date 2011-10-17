{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp,
    RankNTypes, TypeOperators, NoMonomorphismRestriction  #-}

module Hardware.KansasLava.Text where

import Language.KansasLava as KL
import Data.Sized.Unsigned
import Data.Sized.Ix
import Data.Sized.Arith
import Data.Sized.Matrix as M
import Control.Applicative
import Data.Char
import qualified Data.Bits as B
import Data.Maybe as Maybe

-- | 'mm_text_driver' is a memory-mapped driver for a (small) display.
-- It gets passed the background "image", and the mapping from
-- active location number to row,col on the screen.
-- It outputs values sutable for input into the LCD mm drivers.
mm_text_driver :: forall c sig row col loc . 
	( Clock c, sig ~ Signal c
	, Rep loc, Rep row, Rep col
	, Size row, Size col
	, Rep (MUL row col)
	, Num (MUL row col)
	, Size (MUL row col)
	) 
	=> Matrix (row,col) U8		-- backscreen
	-> (loc -> (row,col))	-- active content mapping
	-> Patch (sig (Enabled (loc,U8)))	(sig (Enabled ((row,col),U8)))
		 (sig Ack)			(sig Ack)		
mm_text_driver m f = 
	mapP g $$
	prependP (matrix (M.toList m') :: Matrix (MUL row col) ((row,col),U8))
   where
	m' :: Matrix (row,col) ((row,col),U8)
	m' = forEach m $ \ addr ix -> (addr,ix)
	g :: forall comb . Signal comb (loc,U8) -> Signal comb ((row,col),U8)
	g arg = pack (funMap (return . f) addr,u8)
	   where (addr,u8) = unpack arg


-- | spining bar glyph; shows aliveness.
-- rotates by 45 degrees for each pulse sent in.
aliveGlyph :: forall c sig . (Clock c, sig ~ Signal c)
     => Patch (sig (Enabled ()))	(sig (Enabled (X1,U8)))
	      (sig Ack)			(sig Ack)
aliveGlyph 
      = openP $$
	fstP (cycleP (matrix $ map (fromIntegral . ord) "|/-\\" :: Matrix X4 U8) $$
		  mapP (\ x -> pack (0,x))
		 ) $$
	zipP $$
	mapP (\ ab -> let (a,b) = unpack ab in a)


-- what ever you write appears on the right,
-- pushing everything to the right.

window :: forall c sig x comb . (Clock c, sig ~ Signal c, c ~ (), Size x, Bounded x, Num x, Enum x)
        => Patch (sig (Enabled U8))	(sig (Enabled (Matrix x U8)))
	        (sig Ack)		(sig Ack)
window = loopP patch 
  where
     patch = 
        zipP $$ 
        mapP fn $$
        fifo1 $$
        dupP $$ 
        fstP (prependP (matrix [pure 32] :: Matrix X1 (Matrix x U8))) $$
        emptyP

     fn :: forall comb . Signal comb (Matrix x U8,U8) -> Signal comb (Matrix x U8)
     fn  ab = let (a:: Signal comb (Matrix x U8),b :: Signal comb U8) = unpack ab
                  a' = unpack a :: Matrix x (Signal comb U8)
              in pack $  matrix ([ a' ! x 
                                 | x <- [1..maxBound]
                                 ] ++ [b])

-- show a hex number
hexForm :: forall c sig w .
 	( Clock c, sig ~ Signal c, Size (MUL X4 w), Integral (MUL X4 w)
	, Integral w, Bounded w, Rep w, Size w
	) =>
	Patch (sig (Enabled (Unsigned (MUL X4 w))))	(sig (Enabled (w,U8)))
      	      (sig Ack)					(sig Ack)
hexForm
    = matrixDupP $$
    matrixStackP (forAll $ \ i -> 
		mapP (\ v -> witnessS (Witness :: Witness U4) $ (unsigned) (v `B.shiftR` (fromIntegral (maxBound - i) * 4))) $$
		mapP (funMap (\ x -> if x >= 0 && x <= 9 
                     then return (0x30 + fromIntegral x)
                     else return (0x41 + fromIntegral x - 10))) $$
		mapP (\ ch -> pack (pureS i,ch))) $$
    matrixMergeP PriorityMerge

