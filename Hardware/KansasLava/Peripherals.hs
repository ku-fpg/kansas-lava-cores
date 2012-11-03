{-# LANGUAGE ScopedTypeVariables,TypeFamilies, FlexibleContexts, RankNTypes #-}

module Hardware.KansasLava.Peripherals where

import Data.Sized.Ix hiding (all)
import Data.Sized.Matrix as M
import Data.Sized.Unsigned (U8)
import Hardware.KansasLava.Core

import Control.Monad.Fix

import Language.KansasLava


------------------------------------------------------------
-- LEDs
------------------------------------------------------------

class (MonadFix fab, Size (LEDCount fab)) => LEDs fab where
        type LEDCount fab
        ledNames :: (LEDCount fab ~ x) => SuperFabric fab (Matrix x String)

leds :: (LEDs fab, x ~ LEDCount fab) => Matrix x (Seq Bool) -> SuperFabric fab ()
leds m = do
        led_names <- ledNames
        sequence_ [ outStdLogic nm (m ! i)
                  | (i,nm) <- M.assocs led_names
                  ]


------------------------------------------------------------
-- Switches and buttons
------------------------------------------------------------

class (MonadFix fab, Size (SwitchCount fab)) => Switches fab where
        type SwitchCount fab
        switchNames :: (SwitchCount fab ~ x) => SuperFabric fab (Matrix x String)

switches :: (Switches fab, x ~ SwitchCount fab) => SuperFabric fab (Matrix x (Seq Bool))
switches = do
        switch_names <- switchNames
        sws <- sequence [ inStdLogic nm
                        | nm <- M.toList switch_names
                        ]
        return $ matrix sws

class (MonadFix fab, Size (ButtonCount fab)) => Buttons fab where
        type ButtonCount fab
        buttonNames :: (ButtonCount fab ~ x) => SuperFabric fab (Matrix x String)

buttons :: (Buttons fab, x ~ ButtonCount fab) => SuperFabric fab (Matrix x (Seq Bool))
buttons = do
        button_names <- buttonNames
        buts <- sequence [ inStdLogic nm
                         | nm <- M.toList button_names
                         ]
        return $ matrix buts

------------------------------------------------------------
-- Serial Connections
------------------------------------------------------------

class (CoreMonad fab) => RS232 fab where
        type RS232Count fab
        -- Requests the specific RS232 port for reading,
        -- and the baud rate, gets back an enabled value.
        rs232rx :: (RS232Count fab ~ x) => x -> Int -> SuperFabric fab (Seq (Enabled U8))
        -- Request the specific RS232 port for writing,
        -- and the baud rate, gets back a Write AckBox.
        rs232tx :: (RS232Count fab ~ x) => x -> Int -> Seq Ack -> SuperFabric fab (Enabled U8)


------------------------------------------------------------
-- LCD display
------------------------------------------------------------

class (CoreMonad fab) => LCD fab where
        type LCDSize fab
        lcd :: (LCDSize fab ~ pos) => Seq Ack -> SuperFabric fab (Enabled (pos,U8))


------------------------------------------------------------
-- Rank2 debugging port genertor.
------------------------------------------------------------
{-

newtype MONITOR = MONITOR (forall a. (Rep a, Size (W (Enabled a))) => String -> STMT (REG a))

class (CoreMonad fab) => Monitor fab where
        monitor :: fab MONITOR
-}

