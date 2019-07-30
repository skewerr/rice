module Config.HandleEventHook (handleEventHook) where

import XMonad hiding (handleEventHook)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks (docksEventHook)

import Data.Semigroup (All(..))

handleEventHook :: Event -> X All
handleEventHook
    = fullscreenEventHook
  <+> docksEventHook
  <+> defaultHEHook

defaultHEHook :: Event -> X All
defaultHEHook = const $ return (All True)
