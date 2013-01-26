{-# LANGUAGE ScopedTypeVariables, GADTs, DeriveDataTypeable, DoRec, KindSignatures, TypeFamilies #-}

module Hardware.KansasLava.Simulator.ANSI
        ( ANSI(..)
        , showANSI
        , Color(..)     -- from System.Console.ANSI
        ) where

import System.Console.ANSI
import System.IO

-----------------------------------------------------------------------
-- Helpers for printing to the screen
-----------------------------------------------------------------------

-- | Datastructure helper fort printing on screen
data ANSI a where
        REVERSE :: ANSI ()                 -> ANSI ()
        COLOR   :: Color -> ANSI ()        -> ANSI ()
        PRINT   :: String                  -> ANSI ()
        AT      :: ANSI () -> (Int,Int)    -> ANSI ()
        BIND'    :: ANSI b -> (b -> ANSI a) -> ANSI a
        RETURN'  :: a                       -> ANSI a
        CLEAR    ::                           ANSI ()

instance Monad ANSI where
        return a = RETURN' a
        m >>= k  = BIND' m k

-- | render the ANSI to the screen
showANSI :: ANSI a -> IO a
showANSI (REVERSE ascii) = do
        setSGR [SetSwapForegroundBackground True]
        showANSI ascii
        setSGR []
        hFlush stdout
showANSI (COLOR col ascii) = do
        setSGR [SetColor Foreground Vivid col]
        showANSI ascii
        setSGR []
        hFlush stdout
showANSI (PRINT str) = do
        putStr str
showANSI (AT ascii (row,col)) = do
        setCursorPosition row col
        showANSI ascii
        setCursorPosition 24 0
        hFlush stdout
showANSI (RETURN' a) = return a
showANSI (BIND' m k) = do
        a <- showANSI m
        showANSI (k a)
showANSI (CLEAR) = do
        clearScreen
        hFlush stdout
