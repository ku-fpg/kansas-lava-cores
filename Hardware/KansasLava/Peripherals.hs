{-# LANGUAGE ScopedTypeVariables,TypeFamilies, FlexibleContexts, RankNTypes #-}

module Hardware.KansasLava.Peripherals where


import Data.Sized.Matrix as M
import Data.Sized.Unsigned (U8)
import Hardware.KansasLava.Core

import Control.Monad.Fix

import Language.KansasLava

-- TODO: Remove this module
