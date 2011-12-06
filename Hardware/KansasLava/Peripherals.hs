{-# LANGUAGE ScopedTypeVariables,TypeFamilies, FlexibleContexts #-}

module Hardware.KansasLava.Peripherals where

import Data.Sized.Ix hiding (all)
import Data.Sized.Matrix as M
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

