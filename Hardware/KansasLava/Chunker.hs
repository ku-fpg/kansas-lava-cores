{-# LANGUAGE TypeFamilies, ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types #-}

module Hardware.KansasLava.Chunker (chunker, dechunker) where 

import Data.Sized.Unsigned
import Data.Sized.Signed
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

waitForIt :: forall c sig a . (Clock c, sig ~ CSeq c, c ~ (), Rep a)
	    => Patch (sig (Enabled a))     (sig (Enabled U8))	
	   	     (sig Ready)        ()  (sig Ready       )
waitForIt ~(inp,outReady) = (toReady ready,(),out)
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


conditional :: forall c sig . (Clock c, sig ~ CSeq c, c ~ ())
	    => Patch (sig (Enabled U8))				(sig (Enabled Bool))
		     (sig Ready)	 ()		        (sig Ready)
conditional ~(inp,outReady) = (toReady ready,(),control)
  where
	ready :: sig Bool
	ready = state .==. 0

	state :: sig X3
	state = register 0
	      $ cASE [ (state .==. 0 .&&. isEnabled inp, 	1)
		     , (state .==. 1 .&&. fromReady outReady,	2)
		     , (state .==. 2 .&&. counter0 .==. 0,	0)
		      ] state

	counter0 :: sig U8
	counter0 = cASE [ (state .==. 0 .&&. isEnabled inp, 	 enabledVal inp)
		        , (state .==. 2 .&&. fromReady outReady, counter1 - 1)
		        ] counter1

	counter1 = register 0 counter0

	control :: sig (Enabled Bool)
	control = cASE [ (state .==. 1 .&&. fromReady outReady, enabledS high)
		       , (state .==. 2 .&&. fromReady outReady, enabledS low)
		       ] disabledS

chunker :: forall c sig . (Clock c, sig ~ CSeq c, c ~ ())
	 => Patch (sig (Enabled U8))			(sig (Enabled U8))
		  (sig Ready)	 ()		        (sig Ready)

chunker = noStatus $ patch1 `bus` patch2 `bus` patch3 `bus` patch4	
  where
	patch1 = dupPatch `bus` fstPatch (waitForIt `bus` dupPatch `bus` fstPatch conditional)
	patch2 = forwardPatch (\ ((a :> b) :> c) -> a :> b :> c) `bus`
		 backwardPatch (\ (a :> b :> c) -> (a :> b) :> c) 
	patch3 = fifo (Witness :: Witness X4)  low `stack`
	         fifo (Witness :: Witness X1)  low `stack`
		 fifo (Witness :: Witness X10) low 
	patch4 = muxPatch

dechunker = undefined
