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

{-
joinWrites :: (Clock clk, sig ~ Signal clk)
           => Patch (Matrix x (sig (Enabled (loc,U8)))) (sig (Enabled (loc,U8)))
                    (Matrix x (sig Ack))                (sig Ack)
joinWrites = undefined
-}
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


-- | In a scrollbar, what ever you write appears on the right hand side, pushing everything to the left.

scrollBar :: forall c sig x comb . (Clock c, sig ~ Signal c, Size x, Bounded x, Num x, Enum x, Rep x)
        => Patch (sig (Enabled U8))	(sig (Enabled (x,U8)))
	         (sig Ack)		(sig Ack)
scrollBar = 
        prependP (matrix [32] :: Matrix X1 U8) $$
        loopP patch             $$
        mapP wt_cmds            $$
        matrixToElementsP       
  where
     patch = 
        zipP $$ 
        mapP fn $$
        fifo1 $$
        dupP $$ 
        fstP (prependP (matrix [pure 32] :: Matrix X1 (Matrix x U8)))

     fn :: forall comb . Signal comb (Matrix x U8,U8) -> Signal comb (Matrix x U8)
     fn  ab = let (a:: Signal comb (Matrix x U8),b :: Signal comb U8) = unpack ab
                  a' = unpack a :: Matrix x (Signal comb U8)
              in pack $  matrix ([ a' ! x 
                                 | x <- [1..maxBound]
                                 ] ++ [b])

     wt_cmds :: forall comb . Signal comb (Matrix x U8) -> Signal comb (Matrix x (x,U8))
     wt_cmds = pack . (\ m -> forAll $ \ i -> pack (pureS i,m M.! i)) . unpack
     


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


-- | ord for U8.
ordU8 :: Char -> U8
ordU8 = fromIntegral . ord

-- | chr for U8.
chrU8 :: U8 -> Char
chrU8 = chr . fromIntegral

-- | Turn a string into a 1D matrix
rowU8 :: (Size x) => String -> Matrix x U8
rowU8 = matrix . fmap ordU8

-- | Turn a string into a 2D matrix, ready for background.
boxU8 :: forall x row col . (Size x, Size row,Num row, Enum row, Size col, Num col, Enum col, x ~ MUL row col) 
      => [String] 
      -> Matrix x ((row,col),U8)
boxU8 inp = matrix
      [ ((row,col),ch)
      | (chs,row) <- zip (fmap (fmap ordU8) inp) [0..]
      , (ch,col)  <- zip chs [0..]
      ]

boxU8' :: forall row col . (Size row,Num row, Enum row, Size col, Num col, Enum col) 
      => [String] 
      -> Matrix (row,col) U8
boxU8' = matrix . concat . fmap (fmap ordU8)
