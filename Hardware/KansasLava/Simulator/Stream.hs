{-# LANGUAGE ScopedTypeVariables, GADTs, DeriveDataTypeable, DoRec, KindSignatures, TypeFamilies, BangPatterns #-}

module Hardware.KansasLava.Simulator.Stream
        ( -- * Our streams
         Stream -- abstact
         -- * Our utilities
        , changed
        , freeze
        , flipper
        , tick
        , once
        , count
        , doubleRate
        ) where

import Language.KansasLava.Stream (Stream)
import qualified Language.KansasLava.Stream as S

import Control.Applicative

changed :: (Eq a) => Stream a -> Stream (Maybe a)
changed xs = Just (S.head xs) `S.cons` f (S.head xs) (S.tail xs)
    where
        f x xs | x == S.head xs = Nothing `S.cons` f x (S.tail xs)
               | otherwise      = Just (S.head xs) `S.cons` f (S.head xs) (S.tail xs)

freeze :: (Eq a) => a -> Stream (Maybe a) -> Stream a
freeze a = loop a
  where
        loop n ss = case S.uncons ss of
                      (Nothing,xs) -> n `S.cons` loop n xs
                      (Just v,xs)  -> v `S.cons` loop v xs

flipper :: Stream (Maybe ()) -> Stream Bool
flipper =  loop False
  where
        loop n ss = case S.uncons ss of
                      (Nothing,xs) -> n       `S.cons` loop n xs
                      (Just _,xs)  -> (not n) `S.cons` loop (not n) xs

tick :: Bool -> Maybe ()
tick True = Just ()
tick False = Nothing

-- This can be generalized, etc.
count :: Stream (Maybe ()) -> Stream (Maybe Integer)
count = loop 1
  where
          loop !n ss = case S.uncons ss of
                         (Just (),ts) -> Just n `S.cons` loop (succ n) ts
                         (Nothing,ts) -> Nothing `S.cons` loop n ts

once :: a -> Stream (Maybe a)
once a = Just a `S.cons` pure Nothing


doubleRate :: Stream (Maybe a) -> Stream (Maybe a)
doubleRate (s `S.Cons` (Just ss)) = s `S.cons` (Nothing `S.cons` doubleRate ss)
doubleRate (s `S.Cons` Nothing)   = s `S.Cons` Nothing

