{-# LANGUAGE TypeFamilies, ExistentialQuantification, FlexibleInstances, UndecidableInstances, FlexibleContexts,
    ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies,ParallelListComp,
    RankNTypes, TypeOperators  #-}

module Hardware.KansasLava.FIFO where

import Control.Concurrent
import Control.Monad
import Data.Maybe as Maybe
import Data.Sized.Arith as Arith
import Data.Sized.Ix as X
import Data.Word
import Data.Sized.Unsigned

import Language.KansasLava

import System.IO


------------------------------------------------------------------------------

toCount :: (Signal sig, Size x) => sig Bool -> sig (Unsigned x)
toCount = liftS1 (\ b -> mux2 b (1,0))

incGroup :: (Rep x, Num x, Bounded x) => Comb x -> Comb x
incGroup x = mux2 (x .==. maxBound) (0,x + 1)

-- | Make a sequence obey the given reset signal, returning given value on a reset.
resetable :: forall a c. (Clock c, Rep a) => CSeq c Bool -> Comb a -> CSeq c a -> CSeq c a
resetable rst val x = mux2 rst (liftS0 val,x)



fifoFE :: forall c a counter ix sig .
         (Size counter
        , Size ix
        , counter ~ ADD ix X1
        , Rep a
        , Rep counter
        , Rep ix
        , Num counter
        , Num ix
        , Clock c
	, sig ~ CSeq c
        )
      => Witness ix
         -- ^ depth of FIFO
      -> CSeq c Bool
         -- ^ hard reset option
      -> Patch (sig (Enabled a))			(sig (Enabled (ix,a)))
	       (sig Ready)	 (sig counter)		(sig counter)
         -- ^ input, and Seq trigger of how much to decrement the counter,
         -- ^ backedge for input, internal counter, and write request for memory.
fifoFE Witness rst (inp,dec_by{-,wt_ready-}) = (toReady inp_ready, in_counter1, wr)
  where
--      mem :: Seq ix -> Seq a
--      mem = pipeToMemory env env wr

        inp_done0 :: CSeq c Bool
        inp_done0 = inp_ready `and2` isEnabled inp -- `and2` wt_ready

        wr :: CSeq c (Enabled (ix,a))
        wr = packEnabled (inp_done0)
                         (pack (wr_addr,enabledVal inp))

        wr_addr :: CSeq c ix
        wr_addr = resetable rst 0
                $ register 0
                $ mux2 inp_done0 (liftS1 incGroup wr_addr,wr_addr)

        in_counter0 :: CSeq c counter
        in_counter0 = resetable rst 0
                    $ in_counter1
                        + (unsigned) inp_done0
                        - dec_by

        in_counter1 :: CSeq c counter
        in_counter1 = register 0 in_counter0

--      out :: Seq (Enabled a)
--      out = packEnabled (out_counter1 .>. 0) (mem rd_addr0)

        inp_ready :: CSeq c Bool
        inp_ready = (in_counter1 .<. fromIntegral (size (error "witness" :: ix)))
                        `and2`
                    (bitNot rst)

fifoBE :: forall a c counter ix sig .
         (Size counter
        , Size ix
        , counter ~ ADD ix X1
        , Rep a
        , Rep counter
        , Rep ix
        , Num counter
        , Num ix
        , Clock c
	, sig ~ CSeq c
        )
      => Witness ix
      -> CSeq c Bool    -- ^ reset
--      -> (Comb Bool -> Comb counter -> Comb counter)
--      -> Seq (counter -> counter)
      -> Patch (sig (Enabled a)  :> sig counter)		(sig (Enabled a))
	       (sig ix 		 :> sig Bool)	 (sig counter)	(sig Ack)

{-
      -> (CSeq c counter,CSeq c (Enabled a))
        -- inc from FE
        -- input from Memory read
      -> CSeq c Ack
      -> ((CSeq c ix, CSeq c Bool, CSeq c counter), CSeq c (Enabled a))
-}
        -- address for Memory read
        -- dec to FE
        -- internal counter, and
        -- output for HandShaken
fifoBE Witness rst (mem_rd :> inc_by, out_ready) = 
    let
        rd_addr0 :: CSeq c ix
        rd_addr0 = resetable rst 0
                 $ mux2 out_done0 (liftS1 incGroup rd_addr1,rd_addr1)

        rd_addr1 = register 0
                 $ rd_addr0

	-- technically, ack should never happen if isEnabled out is not set
        out_done0 :: CSeq c Bool
        out_done0 = fromAck out_ready `and2` (isEnabled out)

        out :: CSeq c (Enabled a)
        out = packEnabled ((out_counter1 .>. 0) `and2` bitNot rst `and2` isEnabled mem_rd) (enabledVal mem_rd)

        out_counter0 :: CSeq c counter
        out_counter0 = resetable rst 0
                     $ out_counter1
                        + inc_by
                        - (unsigned) out_done0 

        out_counter1 = register 0 out_counter0
    in
	(rd_addr0 :> out_done0, out_counter1, out)

fifoMem :: forall a c1 c2 counter ix sig1 sig2 .
         (Size counter
        , Size ix
        , counter ~ ADD ix X1
        , Rep a
        , Rep counter
        , Rep ix
        , Num counter
        , Num ix
        , Clock c1
        , Clock c2
	, sig1 ~ CSeq c1
	, sig2 ~ CSeq c2
	, c1 ~ c2
        )
      => Witness ix
      -> Patch (sig1 (Enabled (ix,a)))			(sig2 (Enabled a) :> sig2 counter)
	       (sig1 counter)	 	()		(sig2 ix 	  :> sig2 Bool)
fifoMem Witness ~(wr_in,~(rd_addr :> sent)) = (dec_fe,(),mem_val :> inc_be)
  where
	-- This is the memory.
	-- TODO: why the delay here?
	mem_val = enabledS $ syncRead (writeMemory (wr_in)) rd_addr

	-- Saying Here is some space to write to.
	dec_fe = (unsigned) sent

	-- This needs a two-cycle delay, to provide time for the memory read
	inc_be = (unsigned) $ register False $ register False $ isEnabled wr_in


fifoCounter :: forall counter . (Num counter, Rep counter) => Seq Bool -> Seq Bool -> Seq Bool -> Seq counter
fifoCounter rst inc dec = counter1
    where
        counter0 :: Seq counter
        counter0 = resetable rst 0
                 $ counter1
                        + (unsigned) inc
                        - (unsigned) dec

        counter1 = register 0 counter0

fifoCounter' :: forall counter . (Num counter, Rep counter) => Seq Bool -> Seq counter -> Seq counter -> Seq counter
fifoCounter' rst inc dec = counter1
    where
        counter0 :: Seq counter
        counter0 = resetable rst 0
                 $ counter1
                        + inc -- mux2 inc (1,0)
                        - dec -- mux2 dec (1,0)

        counter1 = register 0 counter0

fifo :: forall a c counter ix .
         (Size counter
        , Size ix
        , counter ~ ADD ix X1
        , Rep a
        , Rep counter
        , Rep ix
        , Num counter
        , Num ix
        , Clock c
        )
      => Witness ix
      -> CSeq c Bool
      -> Patch 	(CSeq c (Enabled a)) 				(CSeq c (Enabled a))
		(CSeq c Ready)		(CSeq c counter)	(CSeq c Ack)

fifo w_ix rst = mapStatus fnCounter fifo_patch
   where
	fifo_patch = fifoFE w_ix rst `bus` fifoMem w_ix `bus` fifoBE w_ix rst 

	fnCounter ~(counter_fe :> () :> _counter_be) = counter_fe

{-
fifo w_ix rst (inp,out_ready) =
    let
        wr :: CSeq c (Maybe (ix, a))
        inp_ready :: CSeq c Ready
        (inp_ready, counter_fe, wr) = fifoFE w_ix rst (inp,dec_by)

        inp_done2 :: CSeq c Bool
        inp_done2 = resetable rst low $ register False $ resetable rst low $ register False $ resetable rst low $ isEnabled wr

        mem :: CSeq c ix -> CSeq c (Enabled a)
        mem = enabledS . pipeToMemory wr

        (rd_addr0 :> out_done0,counter_be,out) = fifoBE w_ix rst (mem rd_addr0 :> inc_by, out_ready)

        dec_by = (unsigned) out_done0
        inc_by = (unsigned) inp_done2
    in
        (inp_ready, counter_fe, out)
-}
{-
fifoZ :: forall a c counter ix .
         (Size counter
        , Size ix
        , counter ~ ADD ix X1
        , Rep a
        , Rep counter
        , Rep ix
        , Num counter
        , Num ix
        , Clock c
        )
      => Witness ix
      -> CSeq c Bool
      -> I (CSeq c (Enabled a)) (CSeq c Ack)
      -> O (CSeq c Ready) (CSeq c (Enabled a),CSeq c counter)
fifoZ w_ix rst (inp,out_ready) =
    let
        wr :: CSeq c (Maybe (ix, a))
        inp_ready :: CSeq c Ready
        (inp_ready, counter, wr) = fifoFE w_ix rst (inp,dec_by)

        inp_done2 :: CSeq c Bool
        inp_done2 = resetable rst low $ register False $ resetable rst low $ register False $ resetable rst low $ isEnabled wr

        mem :: CSeq c ix -> CSeq c (Enabled a)
        mem = enabledS . pipeToMemory wr

        ((rd_addr0,out_done0,_),out) = fifoBE w_ix rst (inc_by,mem rd_addr0) out_ready

        dec_by = liftS1 (\ b -> mux2 b (1,0)) out_done0
        inc_by = liftS1 (\ b -> mux2 b (1,0)) inp_done2
    in
        (inp_ready, (out,counter))
-}

{-
fifoToMatrix :: forall a counter counter2 ix iy iz c .
         (Size counter
        , Size ix
        , Size counter2, Rep counter2, Num counter2
        , counter ~ ADD ix X1
        , counter2 ~ ADD iy X1
        , Rep a
        , Rep counter
        , Rep ix
        , Num counter
        , Num ix
        , Size iy
        , Rep iy, StdLogic ix, StdLogic iy, StdLogic a,
        WIDTH ix ~ ADD (WIDTH iz) (WIDTH iy),
        StdLogic counter, StdLogic counter2,
        StdLogic iz, Size iz, Rep iz, Num iy
        , WIDTH counter ~ ADD (WIDTH iz) (WIDTH counter2)
        , Num iz
        , Clock c
        )
      => Witness ix
      -> Witness iy
      -> CSeq c Bool
      -> HandShaken c (CSeq c (Enabled a))
      -> HandShaken c (CSeq c (Enabled (M.Matrix iz a)))
fifoToMatrix w_ix@Witness w_iy@Witness rst hs = HandShaken $ \ out_ready ->
    let
        wr :: CSeq c (Maybe (ix, a))
        wr = fifoFE w_ix rst (hs,dec_by)

        inp_done2 :: CSeq c Bool
        inp_done2 = resetable rst low
                  $ register False
                  $ resetable rst low
                  $ register False
                  $ resetable rst low
                  $ isEnabled wr

        mem :: CSeq c (Enabled (M.Matrix iz a))
        mem = enabledS
                $ pack
                $ fmap (\ f -> f rd_addr0)
                $ fmap pipeToMemory
                $ splitWrite
                $ mapEnabled (mapPacked $ \ (a,d) -> (factor a,d))
                $ wr

        ((rd_addr0,out_done0),out) = fifoBE w_iy rst (inc_by,mem) <~~ out_ready

        dec_by = mulBy (Witness :: Witness iz) out_done0
        inc_by = divBy (Witness :: Witness iz) rst inp_done2
    in
        out

-- Move into a Commute module?
-- classical find the implementation problem.
splitWrite :: forall a a1 a2 d c . (Rep a1, Rep a2, Rep d, Size a1) => CSeq c (Pipe (a1,a2) d) -> M.Matrix a1 (CSeq c (Pipe a2 d))
splitWrite inp = M.forAll $ \ i -> let (g,v)   = unpackEnabled inp
                                       (a,d)   = unpack v
                                       (a1,a2) = unpack a
                                    in packEnabled (g .&&. (a1 .==. pureS i))
                                                   (pack (a2,d))

-}
mulBy :: forall x sz c . (Clock c, Size sz, Num sz, Num x, Rep x) => Witness sz -> CSeq c Bool -> CSeq c x
mulBy Witness trig = mux2 trig (pureS $ fromIntegral $ size (error "witness" :: sz),pureS 0)

divBy :: forall x sz c . (Clock c, Size sz, Num sz, Rep sz, Num x, Rep x) => Witness sz -> CSeq c Bool -> CSeq c Bool -> CSeq c x
divBy Witness rst trig = mux2 issue (1,0)
        where
                issue = trig .&&. (counter1 .==. (pureS $ fromIntegral (size (error "witness" :: sz) - 1)))

                counter0 :: CSeq c sz
                counter0 = cASE [ (rst,0)
                                , (trig,counter1 + 1)
                                ] counter1
                counter1 :: CSeq c sz
                counter1 = register 0
                         $ mux2 issue (0,counter0)



{-
-- Turn a Hand Shaken signal into a packetized Hand Shaken signal.
mkPacketFIFO ::

-}




