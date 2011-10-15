{-# LANGUAGE FlexibleContexts, TypeFamilies, ScopedTypeVariables, TypeOperators, OverloadedStrings, TemplateHaskell #-}

module Hardware.KansasLava.VGA where

import Language.KansasLava
import Data.Sized.Ix
import Data.Char
import Data.Sized.Unsigned
import qualified System.Console.ANSI as ANSI

data Color
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Eq, Ord, Show, Enum, Bounded)

repBitRep ''Color 3

-- TODO: move to Kansas Lava
enumBitRep :: (Enum a, Bounded a, Size (W a)) => [(a, BitPat (W a))]
enumBitRep = [ (a,fromIntegral (fromEnum a))
             | a <- [minBound .. maxBound]
             ]

-- move to Kansas Lava
bitPatOf :: (BitRep a, Size (W a)) => a -> BitPat (W a)
bitPatOf a = case lookup a bitRep of
           Nothing -> error "bitPatOf"
           Just b -> b

instance BitRep Color where
    bitRep = enumBitRep

data Attr = Attr { bg :: Color, fg :: Color }
        deriving (Eq, Ord, Show)
        
repBitRep ''Attr 6

instance BitRep Attr where
    bitRep = [ (Attr c1 c2, bitPatOf c1 & bitPatOf c2)
             | c1 <- [Black .. White]
             , c2 <- [Black .. White]
             ]


init_VCG_ANSI :: String
init_VCG_ANSI = ANSI.clearScreenCode ++ ANSI.hideCursorCode 
        
show_VCG_ANSI :: (Integral x, Integral y) => ((x,y),(Attr,U7)) -> String
show_VCG_ANSI ((x,y),(Attr bgColor fgColor,u7)) =
        ANSI.setSGRCode 
                [ ANSI.Reset
                , ANSI.SetColor ANSI.Foreground ANSI.Vivid $ toANSIColor fgColor
                , ANSI.SetColor ANSI.Background ANSI.Vivid $ toANSIColor bgColor
                ] ++
        ANSI.setCursorPositionCode (fromIntegral x) (fromIntegral y) ++
	[chr $ fromIntegral $ u7]
                
toANSIColor :: Color -> ANSI.Color
toANSIColor Black   = ANSI.Black
toANSIColor Red     = ANSI.Red
toANSIColor Green   = ANSI.Green
toANSIColor Yellow  = ANSI.Yellow
toANSIColor Blue    = ANSI.Blue
toANSIColor Magenta = ANSI.Magenta
toANSIColor Cyan    = ANSI.Cyan
toANSIColor White   = ANSI.White
        
