import XMonad -- {{{ and a bunch of other things

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.FloatKeys

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.XUtils (fi)
import XMonad.Util.SpawnNamedPipe

import XMonad.Layout.NoBorders
import XMonad.Layout.BoringWindows
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps

import Text.Printf
import Control.Monad (join)
import Data.Ratio
import Data.Monoid (All)
import System.IO (hPutStrLn)

import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map as M

import XMonad.Actions.Hidden
import XMonad.Actions.DmenuWorkspaces
import XMonad.Hooks.ShittyManageHooks
import XMonad.Layout.Hidden
import XMonad.Layout.HiddenQueueLayout
import XMonad.Util.CenterRationalRect
import XMonad.Util.Hidden
-- }}}

-- {{{ some constants
-- TODO: get rid of these
scrRes@(scrWid, scrHei) = (1366, 768)
chaRes@(chaWid, chaHei) = (6, 12)
(horPad, verPad) = (2, 2)

panHei = 20 -- height of the top panel
borWid =  2 -- border width for the windows
terPad =  3 -- terminal padding (internal border)
winGap =  1 -- window margin, will appear twofold

-- }}}

main :: IO () -- {{{
main = xmonad . ewmh $ def
  { terminal           = "st -e tmux"
  , focusFollowsMouse  = True
  , focusedBorderColor = "#b44c21"
  , normalBorderColor  = "#656565"
  , borderWidth        = fi borWid
  , modMask            = mod4Mask
  , workspaces         = ["main"]
  , startupHook        = myStartupHook
  , logHook            = myLogHook
  , manageHook         = myManageHook
  , handleEventHook    = myHandleEventHook
  , layoutHook         = myLayoutHook
  }

  `additionalKeysP`         myKeyBindings
  `additionalMouseBindings` myMouseBindings
  `removeKeysP`              myRemovedBindings
-- }}}

barPP :: PP -- {{{
barPP = def
  { ppHidden = const "", ppHiddenNoWindows = const "", ppVisible = const ""
  , ppUrgent = const ""
  , ppOrder = \(ws:_:_:rs) -> ws:rs
  , ppSep = " "
  , ppCurrent = id
  , ppExtras = [ hiddenNum ]
  }
  where
    hiddenNum = withHidden $ return . catchZero . length
    -- catchZero = \l -> if l == 0 then Nothing else Just . inParens $ show l
    catchZero = \l -> if l == 0 then Nothing else Just $ replicate l '-'
    inParens = wrap "(" ")"
    toIcon = wrap "<icon=.local/share/icons/xpm/" ".xpm/>"
-- }}}
scratchpads :: [NamedScratchpad] -- {{{
scratchpads =
  [ NS "term" "st -n scratch -t scratch -e tmux new -A -s scratch"
    (title =? "scratch")
    (customFloating $ centerIRectOffsetY panHei tw th sw sh)
  , NS "volume" "pavucontrol"
    (className =? "Pavucontrol")
    (customFloating $ centerIRectOffsetY panHei vw vh sw sh)
  ]
  where tw = 100 * chaWid + 2 * (borWid + terPad)
        th =  27 * chaHei + 2 * (borWid + terPad)
        (vw,vh) = (768,648)
        (sw,sh) = scrRes
-- }}}

myStartupHook :: X () -- {{{
myStartupHook
    = startupHook def
  <+> docksStartupHook
  <+> setSupportedWithFullscreen
  <+> spawnNamedPipe "xmobar" "xmopipe"
  <+> join (asks $ logHook . config)
-- }}}
myLogHook :: X () -- {{{
myLogHook = do
  Just xmobarH <- getNamedPipe "xmopipe"
  dynamicLogWithPP $ barPP { ppOutput = ppOutputF xmobarH }
  updatePointer (0.5, 0.5) (0, 0.25)
  unhideOnFocus
  where
    -- toSpaces = (flip replicate) ' ' . length
    centerField [wkspc] = wkspc
    -- centerField [wkspc,mnum] = printf "%s %s %s" (toSpaces mnum) wkspc mnum
    centerField [wkspc,mnum] = printf "%s %s %s" mnum wkspc mnum
    ppOutputF handle = hPutStrLn handle . centerField . words
-- }}}
myHandleEventHook :: Event -> X All -- {{{
myHandleEventHook
    = fullscreenEventHook
  <+> handleEventHook def
  <+> docksEventHook
-- }}}
myManageHook :: ManageHook -- {{{
myManageHook = composeAll
  [ namedScratchpadManageHook scratchpads
  , manageDocks
  , manageHook def
  , composeOne' maybeHooks
  ]
  where
    -- whenever I decide that workspace names should mean something
    -- viewShift = doF . liftM2 (.) W.greedyView W.shift
    centerHook = doCenterFloatOffset scrRes borWid panHei
    maybeHooks = manageByClassName centerHook floatClasses
      ++ manageByProp32Exists centerHook floatProp32s
      ++ manageByPropValue centerHook floatPropValues
      ++ [ isTiled -?> insertPosition Master Newer
         , isDialog -?> centerHook
         ]
    floatClasses =
      [ "mpv"
      , "feh"
      , "Sxiv"
      , "sun-awt-X11-XFramePeer"
      , "Bluetooth-sendto" ]
    floatPropValues =
      [ ("WM_WINDOW_ROLE", "GtkFileChooserDialog")
      , ("WM_WINDOW_ROLE", "gimp-message-dialog")
      , ("WM_WINDOW_ROLE", "gimp-toolbox-color-dialog")
      , ("WM_WINDOW_ROLE", "gimp-query-box")
      , ("WM_WINDOW_ROLE", "file-png")
      , ("WM_NAME", "Friends")
      , ("WM_NAME", "Steam - News")
      , ("WM_NAME", "Discord Updater") ]
    floatProp32s =
      [ "STEAM_GAME" ]
-- }}}
myLayoutHook -- {{{ :: (LayoutClass l) => l Window
  = lessBorders OnlyFloat $ hqLayout ||| fullLayout

hqLayout
  = boringWindows
  . hidden
  . gaps
    [ (R, fi horPad), (L, fi horPad)
    , (U, fi $ verPad + panHei)
    , (D, fi verPad)
    ]
  . spacing (fi winGap)
  $ HQLayout 2 mratio tratio rsratio
  where
    mratio  = 1 - rcratio
    rcratio = (82 * chaWid + 2 * allgaps) % (scrWid - 2 * horPad)
    tratio  = (22 * chaHei + 2 * allgaps) % (scrHei - 2 * verPad - panHei)
    rsratio = chaWid % (scrWid - 2 * horPad)
    allgaps = winGap + borWid + terPad

fullLayout
  = noBorders
  . boringWindows
  . gaps [(U, fi panHei)]
  $ Full
-- }}}

setSupportedWithFullscreen :: X () -- {{{ [ firefox fix ]
setSupportedWithFullscreen = withDisplay $ \dpy -> do
  r <- asks theRoot
  a <- getAtom "_NET_SUPPORTED"
  c <- getAtom "ATOM"
  supp <- mapM getAtom
    ["_NET_WM_STATE_HIDDEN"
    ,"_NET_WM_STATE_FULLSCREEN"
    ,"_NET_NUMBER_OF_DESKTOPS"
    ,"_NET_CLIENT_LIST"
    ,"_NET_CLIENT_LIST_STACKING"
    ,"_NET_CURRENT_DESKTOP"
    ,"_NET_DESKTOP_NAMES"
    ,"_NET_ACTIVE_WINDOW"
    ,"_NET_WM_DESKTOP"
    ,"_NET_WM_STRUT"
    ]
  io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)
-- }}}

myKeyBindings :: [(String, X ())] -- {{{
myKeyBindings =
  -- window navigation
  [ ("M-j", focusDown)
  , ("M-k", focusUp)
  ] ++

  -- workspace navigation
  [ ("M-<R>", moveTo Next (WSIs . return $ (/= "NSP") . W.tag))
  , ("M-<L>", moveTo Prev (WSIs . return $ (/= "NSP") . W.tag))
  , ("M-y", selectWorkspace)
  , ("M-u", renameWorkspace)
  , ("M-i", removeWorkspace)
  ] ++ [
    ("M-" ++ show n, DO.withNthWorkspace W.greedyView (n - 1)) | n <- [1..9]
  ] ++

  -- hiding, restoring and swapping windows
  [ ("M-M1-h", withFocused hideWindow)
  , ("M-M1-j", withFocused swapWithNextHidden)
  , ("M-M1-k", withFocused swapWithLastHidden)
  , ("M-M1-l", withLastHidden unhideWindow)
  ] ++

  -- scratchpads
  [ ("M-M1-<Return>", namedScratchpadAction scratchpads "term")
  , ("M-s v", namedScratchpadAction scratchpads "volume")
  ] ++

  -- copying windows across workspaces
  [ ("M-C-y", chooseWorkspace >>= windows . copy)
  , ("M-S-c", kill1)
  ] ++ [
    ("M-C-" ++ show n, DO.withNthWorkspace copy (n - 1)) | n <- [1..9]
  ] ++

  -- moving windows across workspaces
  [ ("M-S-y", withFocused moveToWorkspace)
  ] ++ [
    ("M-S-" ++ show n, DO.withNthWorkspace W.shift (n - 1)) | n <- [1..9]
  ] ++

  -- centering float windows
  [ ("M-M1-c", withFocused $ keysMoveWindowTo (958,549) (1/2,1/2))
  ]
-- }}}
myMouseBindings -- {{{
  = -- allows floating window resizing with mod+right click
  [ ((mod4Mask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
  ]
-- }}}
myRemovedBindings -- {{{
  =
  [ "M-p"
  , "M-S-r"
  ]
-- }}}

-- vim: set ts=2 sw=2 et :
