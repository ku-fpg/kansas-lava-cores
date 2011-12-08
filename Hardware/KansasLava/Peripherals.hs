{-# LANGUAGE ScopedTypeVariables,TypeFamilies, FlexibleContexts #-}

module Hardware.KansasLava.Peripherals where

import Data.Sized.Ix hiding (all)
import Data.Sized.Matrix as M
import Data.Sized.Unsigned (U8)
import Hardware.KansasLava.Core 

import Language.KansasLava


------------------------------------------------------------
-- LEDs
------------------------------------------------------------

class (CoreMonad fab, Size (LEDCount fab)) => LEDs fab where
        type LEDCount fab
        ledNames :: (LEDCount fab ~ x) => fab (Matrix x String)

leds :: (LEDs fab, x ~ LEDCount fab) => fab (Matrix x (REG Bool))
leds = do
        led_names <- ledNames
        core "leds" $ do
                ls <- sequence
                        [ OUTPUT (outStdLogic nm . delayEnabled) :: STMT (REG Bool)
                        | nm <- M.toList led_names
                        ]
                return $ matrix ls 

------------------------------------------------------------
-- Switches and buttons
------------------------------------------------------------

class (CoreMonad fab, Size (SwitchCount fab)) => Switches fab where
        type SwitchCount fab
        switchNames :: (SwitchCount fab ~ x) => fab (Matrix x String)

switches :: (Switches fab, x ~ SwitchCount fab) => fab (Matrix x (EXPR Bool))
switches = do
        switch_names <- switchNames
        core "switches" $ do
                ss <- sequence
                        [ INPUT (inStdLogic nm)
                        | nm <- M.toList switch_names
                        ]
                return $ matrix ss

class (CoreMonad fab, Size (ButtonCount fab)) => Buttons fab where
        type ButtonCount fab
        buttonNames :: (ButtonCount fab ~ x) => fab (Matrix x String)

buttons :: (Buttons fab, x ~ ButtonCount fab) => fab (Matrix x (EXPR Bool))
buttons = do
        button_names <- buttonNames
        core "buttons" $ do
                ss <- sequence
                        [ INPUT (inStdLogic nm)
                        | nm <- M.toList button_names
                        ]
                return $ matrix ss

------------------------------------------------------------
-- Serial Connections
------------------------------------------------------------

class (CoreMonad fab) => RS232 fab where
        type RS232Count fab
        -- Requests the specific RS232 port for reading,
        -- and the baud rate, gets back an enabled value.
        rs232rx :: (RS232Count fab ~ x) => x -> Int -> fab (EXPR (Enabled U8))
        -- Request the specific RS232 port for writing,
        -- and the baud rate, gets back a Write AckBox.
        rs232tx :: (RS232Count fab ~ x) => x -> Int -> fab (WriteAckBox U8)

------------------------------------------------------------
-- LCD display
------------------------------------------------------------

class (CoreMonad fab) => LCD fab where
        type LCDSize fab
        lcd :: (LCDSize fab ~ pos) => fab (WriteAckBox (pos,U8))
