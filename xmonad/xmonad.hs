import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Util.EZConfig

import qualified Config.Bindings as BD
import qualified Config.Dimensions as D
import qualified Config.HandleEventHook as HEH
import qualified Config.Layouts as LY
import qualified Config.LogHook as LH
import qualified Config.ManageHook as MH
import qualified Config.StartupHook as SH

main :: IO ()
main = xmonad . ewmh $ def
  { terminal           = "st -e tmux"
  , focusFollowsMouse  = True
  , focusedBorderColor = "#a54242"
  , normalBorderColor  = "#656565"
  , modMask            = mod4Mask
  , workspaces         = ["main"]
  , borderWidth        = D.borderWidth
  , startupHook        = SH.startupHook
  , logHook            = LH.logHook
  , manageHook         = MH.manageHook
  , handleEventHook    = HEH.handleEventHook
  , layoutHook         = LY.layoutHook
  }
  `additionalKeysP`         BD.keyBindings
  `additionalMouseBindings` BD.mouseBindings
  `removeKeysP`             BD.removedBindings

-- vim: set ts=2 sw=2 et :
