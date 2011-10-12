{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp,
    RankNTypes, TypeOperators, NoMonomorphismRestriction  #-}

module Hardware.KansasLava.Form where

import Language.KansasLava as KL
import Data.Sized.Unsigned
import Data.Sized.Ix
import Data.Sized.Arith
import Data.Sized.Matrix as M
import Control.Applicative
import Data.Char
import qualified Data.Bits as B
import Data.Maybe as Maybe

-- | 'mm_driver' is a memory-mapped driver for a (small) display.
-- It gets passed the background "image", and the mapping from
-- active location number to row,col on the screen.
-- It outputs values sutable for input into the shallow_mm (for
-- simulation), or the LCD mm drivers.
mm_driver :: forall c sig row col loc . 
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
mm_driver m f = 
	mapPatch g $$
	appendPatch (matrix (M.toList m') :: Matrix (MUL row col) ((row,col),U8))
   where
	m' :: Matrix (row,col) ((row,col),U8)
	m' = forEach m $ \ addr ix -> (addr,ix)
	g :: forall comb . Signal comb (loc,U8) -> Signal comb ((row,col),U8)
	g arg = pack (funMap (return . f) addr,u8)
	   where (addr,u8) = unpack arg


-- spining bar; shows aliveness.
aliveForm :: forall c sig . (Clock c, sig ~ Signal c)
     => Patch (sig (Enabled ()))	(sig (Enabled (X1,U8)))
	      (sig Ack)			(sig Ack)
aliveForm = openPatch $$
	fstPatch (cyclePatch (matrix $ map (fromIntegral . ord) "|/-\\" :: Matrix X4 U8) $$
		  mapPatch (\ x -> pack (0,x))
		 ) $$
	zipPatch $$
	mapPatch (\ ab -> let (a,b) = unpack ab in a)


-- what ever you write appears on the right,
-- pushing everything to the right.

window :: forall c sig x comb . (Clock c, sig ~ Signal c, c ~ (), Size x, Bounded x, Num x, Enum x)
        => Patch (sig (Enabled U8))	(sig (Enabled (Matrix x U8)))
	        (sig Ack)		(sig Ack)
window = loopPatch patch 
--        $$
--        cyclePatch (
--        matrixExpandPatch 
  where
     patch = 
        zipPatch $$ 
        mapPatch fn $$
        fifo1 $$
        dupPatch $$ 
        fstPatch (appendPatch (matrix [pure 32] :: Matrix X1 (Matrix x U8))) $$
        nullPatch

     fn :: forall comb . Signal comb (Matrix x U8,U8) -> Signal comb (Matrix x U8)
     fn  ab = let (a:: Signal comb (Matrix x U8),b :: Signal comb U8) = unpack ab
                  a' = unpack a :: Matrix x (Signal comb U8)
              in pack $  matrix ([ a' ! x 
                                 | x <- [1..maxBound]
                                 ] ++ [b])


pos :: forall c sig x y . (Clock c, sig ~ Signal c, Rep x, Rep y, Num x, Num y)
     => y
     -> Patch (sig (Enabled (x,U8)))	(sig (Enabled (y,U8)))
	      (sig Ack)			(sig Ack)
pos n = mapPatch $ 
	   \ x_u8 -> let (x,u8) = unpack x_u8
		     in pack ((unsigned) x + pureS n,u8)

-- show a hex number
hexForm :: forall c sig w .
 	( Clock c, sig ~ Signal c, Size (MUL X4 w), Integral (MUL X4 w)
	, Integral w, Bounded w, Rep w, Size w
	) =>
	Patch (sig (Enabled (Unsigned (MUL X4 w))))	(sig (Enabled (w,U8)))
      	     (sig Ack)					(sig Ack)
hexForm
    = matrixDupPatch $$
      matrixStack (forAll $ \ i -> 
		mapPatch (\ v -> witnessS (Witness :: Witness U4) $ (unsigned) (v `B.shiftR` (fromIntegral (maxBound - i) * 4))) $$
		mapPatch (funMap (\ x -> if x >= 0 && x <= 9 
                     then return (0x30 + fromIntegral x)
                     else return (0x41 + fromIntegral x - 10))) $$
		mapPatch (\ ch -> pack (pureS i,ch))) $$
    matrixMergePatch PriorityMerge


-- | 'shallow_mm' simulates the memory mapped API, by drawing an ASCII picture after each write.
-- Note that this handles 2D devices.
shallow_mm :: forall c sig addr . (Clock c, sig ~ Signal c, Size addr, Rep addr)
	=> Patch (sig (Enabled (addr,U8)))	[String]
		 (sig Ack)			()
shallow_mm = fromAckBox $$ 
	     forwardPatch (Maybe.catMaybes) $$
	     forwardPatch (scanl driver screen) $$
	     forwardPatch (fmap (M.toList))
   where
	driver :: Matrix addr Char  -> (addr,U8) -> Matrix addr Char
	driver m (addr,val) = m // [(addr,chr (fromIntegral val))]

	screen :: Matrix addr Char
	screen = pure ' '

-- | 'frame' puts a frame round the text, and puts it at the top of
-- the screen.
frame :: (Int,Int) -> String -> String
frame (row,col) str = 
	"\ESC[0H"  ++
	"+" ++ replicate col '-' ++ "+\n" ++
	concat	[ "|" ++ take col (drop (i * col) str) ++ "|\n"
		| i <- take row [0..]
		] ++
	"+" ++ replicate col '-' ++ "+\n"
