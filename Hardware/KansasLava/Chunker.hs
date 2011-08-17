{-# LANGUAGE TypeFamilies, ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types, TypeOperators #-}

module Hardware.KansasLava.Chunker where -- (chunker, dechunker) where 

import Data.Sized.Unsigned
import Data.Sized.Signed
import Data.Sized.Arith
import Data.Sized.Ix
import Data.Sized.Matrix as M

import Language.KansasLava 
import qualified Language.KansasLava as KL
import Data.Maybe as Maybe
import Data.Char as Char
import Control.Monad	
import Data.Default
import Data.Word
import Debug.Trace

import Hardware.KansasLava.FIFO


-- | We use network byte order
--    http://en.wikipedia.org/wiki/Endianness#Endianness_in_networking
{-
  +----+----+--------+
  | HI | LO |  DATA  |
  +----+----+--------+

  The idea is that a chunk can be transmitted *without* needing any extra inputs or stimuli. 
  Like an atomic unit of data.

-}
{-
waitForIt :: forall c sig a . (Clock c, sig ~ CSeq c, c ~ (), Rep a)
	    => Patch (sig (Enabled a))     (sig (Enabled U8))	
	   	     (sig Ready)           (sig Ready       )
waitForIt ~(inp,outReady) = (toReady ready,out)
  where
	maxCounter :: U8
	maxCounter = 15

	maxTime :: U16
	maxTime = 50

	-- triggers
	ready :: sig Bool
	ready = state .==. 0

	send :: sig Bool
	send = state .==. 1 .&&. fromReady outReady

	tick :: sig Bool
	tick = state .==. 0 .&&. isEnabled inp

	-- the state
	state :: sig X2
	state = register 0
	      $ cASE [ (tick .&&. counter0 .==. fromIntegral maxCounter, 1)
		     , (timer .==. 0,1)
		     , (send, 0)
		     ] state

	counter0, counter1 :: sig U8
	counter0 = cASE [ (tick, counter1 + 1)
			, (send, 0)
			] counter1
	counter1 = register 0 counter0

	out = packEnabled send counter1

	timer :: sig U16
	timer = register (fromIntegral maxTime)
	      $ cASE [ (state .==. 1, fromIntegral maxTime)
			-- only dec if someone is waiting
			-- and there is some data
		     , (fromReady outReady .&&. counter1 .>. 0, timer - 1)	
		     ] timer


-- | Count a (fixed-sized) header with 1's, and a payload with 0's.
-- The fixed sized header counting is done before reading the payload size.
chunkCounter :: forall c sig x y . (Clock c, sig ~ CSeq c, Size x, Num x, Rep x, Size y, Rep y, Num y, c ~ ())
	    => Witness x			-- number of 1's on the front
	    -> Patch (sig (Enabled (Unsigned y)))		(sig (Enabled Bool))
		     (sig Ready)	 		        (sig Ready)
chunkCounter Witness ~(inp,outReady) = (toReady ready,control)
  where
	-- triggers
	send_one  = state .==. 0 .&&. fromReady outReady
	recv_count = state .==. 1 .&&. isEnabled inp
	
	state :: sig X3
	state = register 0
	      $ cASE [ (send_one .&&. ones0 .==. 0, 		1)
		     , (recv_count,				2)
		     , (state .==. 2 .&&. counter0 .==. 0,	0)
		      ] state

	-- send out x 1's.
	ones0 :: sig x
	ones0 = cASE [ (send_one, ones1 - 1) ]
		     ones1
		
	ones1 = register (0 :: x) ones0
	
	ready :: sig Bool
	ready = state .==. 1

	counter0 :: sig (Unsigned y)
	counter0 = cASE [ (recv_count, 				 enabledVal inp)
		        , (state .==. 2 .&&. fromReady outReady, counter1 - 1)
		        ] counter1

	counter1 = register 0 counter0

	control :: sig (Enabled Bool)
	control = cASE [ (state .==. 0 .&&. fromReady outReady, enabledS high)
		       , (state .==. 2 .&&. fromReady outReady, enabledS low)
		       ] disabledS

{-
chunker :: forall c sig . (Clock c, sig ~ CSeq c, c ~ ())
	 => Patch (sig (Enabled U8))			(sig (Enabled U8))
		  (sig Ready)	 ()		        (sig Ready)

chunker = noStatus $ patch1 `bus` patch2 `bus` patch3 `bus` patch4	
  where
	patch1 = dupPatch `bus` fstPatch (
	patch2 = forwardPatch (\ ((a :> b) :> c) -> a :> b :> c) `bus`
		 backwardPatch (\ (a :> b :> c) -> (a :> b) :> c) 
	patch3 = fifo (Witness :: Witness X4)  low `stack`
	         fifo (Witness :: Witness X1)  low `stack`
		 fifo (Witness :: Witness X10) low 
	patch4 = muxPatch

dechunker = undefined
-}



chunkJoinHeader :: forall c sig x y a . 
   (Clock c, sig ~ CSeq c, Rep a, Rep x, Size x, Num x, Enum x, Rep y, Size y, Num y, c ~ ())
  => (Comb (Matrix x a) -> Comb (Unsigned y))
  -> Patch (sig (Enabled (Matrix x a))  :> sig (Enabled a))	(sig (Enabled a))
	   (sig Ready 		        :> sig Ready)	        (sig Ready)

chunkJoinHeader f = patch1 $$ patch2 $$ patch3
   where
	patch1 = stack (dupPatch $$ 
				stack (forwardPatch (mapEnabled f) $$ 
				       fifo1 $$ bridge $$
				       chunkCounter (Witness :: Witness x) $$
				       fifo1)
				      (fifo1 $$ matrixExpandPatch $$ fifo1)
		          )
			fifo1
	patch2 = forwardPatch (\ ((a :> b) :> c) -> a :> b :> c) `bus`
		 backwardPatch (\ (a :> b :> c) -> (a :> b) :> c) 
	patch3 = muxPatch


chunkSplitHeader :: forall c sig x y a . 
   (Clock c, sig ~ CSeq c, Rep a, Rep x, Size x, Num x, Enum x, Rep y, Size y, Num y, c ~ ())
  => (Comb (Matrix x a) -> Comb (Unsigned y))
  -> Patch (sig (Enabled a))	(sig (Enabled (Matrix x a))  :> sig (Enabled a))
	   (sig Ready)		(sig Ready 		     :> sig Ready)	        
chunkSplitHeader f = 
	loopPatch $
		(fifo1 `stack` fifo1) $$
		deMuxPatch $$
		(fstPatch (fifo1 $$ matrixContractPatch $$ dupPatch $$ fstPatch clicker)) $$
		reorg
  where
      clicker = forwardPatch (mapEnabled f) $$ 
 		fifo1 $$ bridge $$
		chunkCounter (Witness :: Witness x)
      reorg = forwardPatch (\ ((a :> b) :> c) -> a :> b :> c) `bus`
	      backwardPatch (\ (a :> b :> c) -> (a :> b) :> c) 


-}