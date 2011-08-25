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

waitForIt :: forall c sig a b t x y . 
	( Clock c, sig ~ CSeq c, c ~ ()
	, Rep a
	, b ~ Unsigned x, Size x
	, Size t
	)   => b		-- ^ The maximum size of chunk
	    -> Witness t	-- ^ 2^t is the timeout time between elements
	    -> Patch (sig (Enabled a))     (sig (Enabled b))	
	   	     (sig Ack)             (sig Ack)
waitForIt maxCounter Witness ~(inp,outAck) = (toAck tick,out)
  where
	-- triggers
	ready :: sig Bool
	ready = state .==. 0

	send :: sig Bool
	send = state .==. 1 .&&. fromAck outAck

	tick :: sig Bool
	tick = state .==. 0 .&&. isEnabled inp

	-- the state
	state :: sig X2
	state = register 0
	      $ cASE [ (tick .&&. counter0 .==. fromIntegral maxCounter, 1)
							    -- if reached max, then tick
		     , (timer .==. 0 .&&. counter0 .>. 0, 1) -- please send the size next time round
		     , (send .&&. fromAck outAck, 0)	     -- sent the size out
		     ] state

	counter0, counter1 :: sig b
	counter0 = cASE [ (tick, counter1 + 1)
			, (send, 0)
			] counter1
	counter1 = register 0 counter0

	out = packEnabled (state .==. 1) counter1

	-- in the background, we wait for a timeout event.
	timer :: sig (Unsigned t)
	timer = register 1
	      $ cASE [ (state .==. 1, 1)
			-- only dec if there *is* some data
		     , (counter1 .>. 0, timer + 1)
		     ] timer


-- | Count a (fixed-sized) header with 1's, and a payload with 0's.
-- The fixed sized header counting is done before reading the payload size.
chunkCounter :: forall c sig x y . (Clock c, sig ~ CSeq c, Size x, Num x, Rep x, Size y, Rep y, Num y, c ~ ())
	    => Witness x			-- number of 1's on the front
	    -> Patch (sig (Enabled (Unsigned y)))		(sig (Enabled Bool))
		     (sig Ack)	 			        (sig Ack)
chunkCounter w = ackToReadyBridge $$ chunkCounter' w $$ readyToAckBridge where
 chunkCounter' Witness ~(inp,outReady) = (toReady ready,control)
  where
	-- triggers
	send_one  = state .==. 0 .&&. fromReady outReady
	recv_count = state .==. 1 .&&. isEnabled inp
	
	state :: sig X3
	state = register 0
	      $ cASE [ (send_one .&&. ones0 .==. 0, 		1)
		     , (recv_count .&&. enabledVal inp .==. 0,	0)	-- do not issue *any* zeros for '0'.
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


chunkJoinHeader :: forall c sig x y a . 
   (Clock c, sig ~ CSeq c, Rep a, Rep x, Size x, Num x, Enum x, Rep y, Size y, Num y, c ~ ())
  => (Comb (Matrix x a) -> Comb (Unsigned y))
  -> Patch (sig (Enabled (Matrix x a))  :> sig (Enabled a))	(sig (Enabled a))
	   (sig Ack 		        :> sig Ack)	        (sig Ack)

chunkJoinHeader f = patch1 $$ patch2 $$ patch3
   where
	patch1 = stack (dupPatch $$ 
				stack (forwardPatch (mapEnabled f) $$ 
				       fifo1 $$
				       chunkCounter (Witness :: Witness x))
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
	   (sig Ack)		(sig Ack 		     :> sig Ack)	        
chunkSplitHeader f = 
	loopPatch $
		(fifo1 `stack` fifo1) $$
		deMuxPatch $$
		(fstPatch (fifo1 $$ matrixContractPatch $$ dupPatch $$ fstPatch clicker)) $$
		reorg
  where
      clicker = forwardPatch (mapEnabled f) $$ 
 		fifo1 $$ 
		chunkCounter (Witness :: Witness x)
      reorg = forwardPatch (\ ((a :> b) :> c) -> a :> b :> c) `bus`
	      backwardPatch (\ (a :> b :> c) -> (a :> b) :> c) 

-- TODO: generalize to Non-X1 headers, and use witness for max chunk size (so that the fifo size can be driven).
chunker :: forall c sig t w . (Size t, Clock c, sig ~ CSeq c, c ~ ())
        => Unsigned X8					-- max chunk size
	-> Witness t					-- 2^t is the timeout for a chunk
	-> (Comb (Matrix X1 U8) -> Comb U8)		-- interprete the header
	-> (Comb (Unsigned X8) -> Comb (Matrix X1 U8))	-- constructing the header
	-> Patch (sig (Enabled U8))                    (sig (Enabled U8))
                 (sig Ack)                             (sig Ack)
chunker mx wit f g = dupPatch $$ stack waiting stalling $$ chunkJoinHeader f
  where 
	waiting = waitForIt mx wit $$ 
		  mapPatch g

	stalling = fifo (Witness :: Witness X256) low

rdByteHeader :: Comb (Matrix X1 U8) -> Comb U8
rdByteHeader sz = unpack sz ! 0

mkByteHeader :: Comb U8 -> Comb (Matrix X1 U8)
mkByteHeader sz = pack (matrix [sz] :: Matrix X1 (Comb U8))

--twoByteHeader :: Comb U16 -> Comb (Matrix X2 U8)
--twoByteHeader sz = pack (matrix [sz] :: Matrix X2 U8)
