module Hardware.KansasLava.Simulator.Stream
        ( -- * Our streams
         Stream -- abstact
         -- * Our utilities
        , changed
        , freeze
        , flipper
        , tick
        ) where

import Language.KansasLava.Stream (Stream)
import qualified Language.KansasLava.Stream as S

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
