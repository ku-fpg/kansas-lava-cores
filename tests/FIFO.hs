{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module FIFO (tests, FIFO(..), testFIFO) where

import Language.KansasLava
import Hardware.KansasLava.FIFO (fifo)

import Data.Sized.Unsigned
import Data.Sized.Arith
import Data.Sized.Ix
import System.Random
--import Data.Maybe 
import Debug.Trace

tests :: TestSeq -> IO ()
tests test = do
        -- testing FIFOs


        let fifoTest :: forall w sz . (Rep (ADD sz X1),
                      Rep sz,
                      Rep w,
                      Eq w,
                      Size sz,
                      Size (ADD sz X1),
                      Num sz,
                      Num (ADD sz X1)) => Witness sz -> FIFO w w
            fifoTest wit = FIFO
                        { theFIFO = fifo wit low :: (Seq (Enabled w), Seq Ack) -> (Seq Ack, Seq (Enabled w))
                        , correctnessCondition = \ ins outs -> trace (show ("cc",length ins,length outs)) $
                                case () of
                                  () | outs /= take (length outs) ins -> return "in/out differences"
                                  () | length outs < 20000            -> return "to few transfers"
                                  () | length ins - length outs > size (undefined :: sz) -> return "missing items?"
                                     | otherwise -> Nothing
                        , theFIFOName = "vanilla/" ++ show (size (error "witness" :: sz))
                        }

        let t :: forall w sz sz1 .
                 (Eq w, Rep w, Show w,
                               Size (W w),
                  sz1 ~ ADD sz X1,
                               Size (ADD (W w) X1),     --- Hmm
                  Size sz, Size sz1,
                  Rep sz, Rep sz1,
                  Num w, Num sz, Num sz1)
                 => String -> Gen (Maybe w) -> Witness sz -> IO ()
            t str arb w = testFIFO test str (fifoTest w) (dubGen arb)


        t "U5"  (arbitrary :: Gen (Maybe U5)) (Witness :: Witness X1)
        t "U5"  (arbitrary :: Gen (Maybe U5)) (Witness :: Witness X2)
        t "U5"  (arbitrary :: Gen (Maybe U5)) (Witness :: Witness X3)
        t "U5"  (arbitrary :: Gen (Maybe U5)) (Witness :: Witness X4)
        t "U5"  (arbitrary :: Gen (Maybe U5)) (Witness :: Witness X5)
        t "U5"  (arbitrary :: Gen (Maybe U5)) (Witness :: Witness X6)
        t "U5"  (arbitrary :: Gen (Maybe U5)) (Witness :: Witness X7)
        t "U5"  (arbitrary :: Gen (Maybe U5)) (Witness :: Witness X8)

-- Need to fix memories first
--        t "U1"  (arbitrary :: Gen (Bool,Maybe U1)) (Witness :: Witness X1)
--        t "Bool"  (arbitrary :: Gen (Bool,Maybe Bool)) (Witness :: Witness X1)

data FIFO w1 w2 = FIFO
            { theFIFO :: (Seq (Enabled w1), Seq Ack) -> (Seq Ack, Seq (Enabled w2))
            , correctnessCondition :: [w1] -> [w2] -> Maybe String
            , theFIFOName :: String
            }

testFIFO :: forall w sz sz1 . (Eq w, Rep w, Show w,
                               Size (W w),
                               Size (ADD (W w) X1),     --- Hmm
                               Num w)
        => TestSeq -> String -> FIFO w w -> Gen (Maybe w) -> IO ()
testFIFO (TestSeq test _) tyName fifoTest ws = do
        let vals    :: [Maybe w]
            vals = take 100000 $ genToRandom $ loop 10 $ ws

        -- good enough for this sort of testing
        let stdGen = mkStdGen 0

        let (lhs_r,rhs_r) = split stdGen
            cir = theFIFO fifoTest 

            driver :: ( Integer -> Float
                      , Integer -> Float
                      , Integer -> Float
                      , Integer -> Float
                      ) -> Fabric (Int -> Maybe String)
            driver (a,b,c,d) = do
                -- backedge output from DUT
                ack <- inStdLogic "ack"

                let vals' :: Seq (Enabled w)
                    vals' = toHandShake (vals ++ Prelude.repeat Nothing) ack'

                    (vals2,ack') = shallowHandShakeBridge lhs_r (a,b) (vals',toAck ack)

                -- sent to DUT
                outStdLogicVector "vals"        (enabledVal vals2)
                outStdLogic       "vals_en"     (isEnabled vals2)

                -- DUT does stuff

                -- reading from DUT
                res     <- inStdLogicVector "res" 
                res_en  <- inStdLogic       "res_en"

                let flag :: Seq Ack 
                    opt_as :: [Maybe w]

                    (res',flag) = shallowHandShakeBridge lhs_r (c,d) (packEnabled res_en res,flag')

                    (flag', opt_as) = fromHandShake res'



                outStdLogic "flag" flag

                return $ \ n -> correctnessCondition fifoTest 
                                   [ x | (Just (Just x),Just (Ack True)) <- take n $ zip (fromSeq vals') (fromSeq ack') ]
                                   [ x | (Just (Just x),Just (Ack True)) <- take n $ zip (fromSeq res')  (fromSeq flag') ]

{-
let ans = [ a | Just a <- take n opt_as ]
                                    inp = [ a | Just a <- take n vals ]
                                in if ans == take (length ans) inp
                                   && length inp > 1000
                                   then Nothing -- ("matched" ++ show (length ans))
                                   else Just (show (ans,inp))
-}

            dut :: Fabric ()
            dut = do
                flag    <- inStdLogic "flag"
                vls     <- inStdLogicVector "vals"
                vals_en <- inStdLogic "vals_en"
                let (ack,res') = cir (packEnabled vals_en vls, flag)
                outStdLogicVector "res"  (enabledVal res')
                outStdLogic "res_en"     (isEnabled res')
                outStdLogic "ack"        ack


            a = \ n -> [0.1,0.2 ..] !! fromIntegral (n `div` 10000) 
            b = \ n -> [0.1,0.2 ..] !! fromIntegral (n `div` 10000) 
            c = \ n -> [0.1,0.2 ..] !! fromIntegral (n `div` 10000) 
            d = \ n -> [0.1,0.2 ..] !! fromIntegral (n `div` 10000)  

        test ("fifo/" ++ theFIFOName fifoTest ++ "/" ++ tyName) (length vals) dut (driver (a,b,c,d))


{-
        --------------------------------------------------------------------------------------------

        -- A test for as fast as you can write

        let vals2 = cycle [ Just x | Just x <- vals ]

        let thu2 :: Thunk (CSeq () (Bool,Enabled w))
            thu2 = Thunk cir
                        (\ f -> let inp = toHandShaken vals2 back
                                    (back,res) = unpack $ f $ pack (inp,high)
                                 in pack (back,res)
                        )

        let res2 :: Seq (Bool,Enabled w)
            res2 = pack (undefinedS,toSeq $ fifoSpec vals2 (repeat True) [])


        test ("fifo/speed/sz_" ++ show fifoSize ++ "/" ++ tyName) (length vals) thu2 res2

        --------------------------------------------------------------------------------------------
        -- A test for phasing the fifo

        let vals3 = vals

        let accept = concat [ if (x `div` (fifoSize * 32)) `mod` 2 == 0
                              then if not v then [v,v,v] else [v]
                              else if     v then [v,v,v] else [v]
                            | (x,v) <- zip [0..] (cycle $ outBools)
                            ]

        let thu3 :: Thunk (CSeq () (Bool,Enabled w))
            thu3 = Thunk cir
                        (\ f -> let inp = toHandShaken vals3 back
                                    (back,res) = unpack $ f $ pack (inp,toSeq accept)
                                 in pack (back,res)
                        )

        let res3 :: Seq (Bool,Enabled w)
            res3 = pack (undefinedS,toSeq $ fifoSpec vals3 accept [])

        test ("fifo/phase/sz_" ++ show fifoSize ++ "/" ++ tyName) (length vals) thu3 res3

        --------------------------------------------------------------------------------------------
-}
        return ()
