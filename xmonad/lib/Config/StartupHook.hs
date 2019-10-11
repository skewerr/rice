module Config.StartupHook (startupHook) where

import XMonad hiding (startupHook)
import XMonad.Hooks.ManageDocks (docksStartupHook)
import XMonad.Util.SpawnNamedPipe (spawnNamedPipe)

import Control.Monad (join)

-- Come up with a better definition since XMonad's default startupHook might
-- change from `return ()` at some point.
startupHook :: X ()
startupHook
    = docksStartupHook
   <> setSupportedWithFullscreen
   <> spawnNamedPipe "xmobar" "xmopipe"
   <> join (asks $ logHook . config)

-- This is a way to make Firefox' fullscreen mode work again with XMonad.  Check
-- from time to time whether it's still necessary, as EwmhDesktops is
-- updated.
setSupportedWithFullscreen :: X ()
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
