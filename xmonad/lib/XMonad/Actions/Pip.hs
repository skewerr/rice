module XMonad.Actions.Pip
  ( markPipWindow
  , unmarkPipWindow
  , togglePipWindow
  , isPipWindow
  ) where

import XMonad
import XMonad.Hooks.Place
import XMonad.Util.WindowProperties (getProp32s)
import Data.Maybe

markPipWindow :: Window -> X ()
markPipWindow win = withDisplay $ \dpy -> do
  pipAtom <- getAtom "_PIP_WINDOW"
  io $ changeProperty32 dpy win pipAtom aTOM propModeReplace [1]
  -- borderChange <- io $ setWindowBorderWidth dpy win 0
  -- if borderChange == ()
  placeFocused $ fixed (0,1)
    -- then placeFocused $ fixed (0,1)
    -- else return ()

unmarkPipWindow :: Window -> X ()
unmarkPipWindow win = withDisplay $ \dpy -> do
  pipAtom <- getAtom "_PIP_WINDOW"
  io $ deleteProperty dpy win pipAtom
--  io $ setWindowBorderWidth dpy win 2
  placeFocused $ fixed (0,1)

isPipWindow :: Query Bool
isPipWindow = fmap isJust $ ask >>= liftX . getProp32s "_PIP_WINDOW"

togglePipWindow :: Window -> X ()
togglePipWindow win = do
  isPip <- runQuery isPipWindow win
  if isPip
    then unmarkPipWindow win
    else markPipWindow win
