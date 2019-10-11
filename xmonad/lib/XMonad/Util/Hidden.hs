{-# LANGUAGE DeriveDataTypeable #-}

--------------------------------------------------------------------------------
-- | Creates the record that will be stored through X.U.ExtensibleState, as well
-- as some functions to modify it. It aims to simplify and replace X.U.Minimize
-- for my particular setup.
--
-- TODO: support floating windows (need to keep the desired rect)
--------------------------------------------------------------------------------

module XMonad.Util.Hidden
  ( Hidden(..)
  , InsertFunc
  , modified
  , setHidden
  , setNotHidden
  , getHidden
  , withHidden
  ) where

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.WindowProperties (getProp32)

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.List as L
import qualified Data.Sequence as S
import Foreign.C.Types (CLong)

-- these are really just for brevity in type annotations
type ModifyFunc = S.Seq Window -> S.Seq Window
type InsertFunc = Window -> ModifyFunc

newtype Hidden = Hidden
  { hiddenWindows :: S.Seq Window
  } deriving (Eq, Typeable, Read, Show)

instance ExtensionClass Hidden where
  initialValue = Hidden { hiddenWindows = S.empty }
  extensionType = PersistentExtension

modified :: ModifyFunc -> X Bool
modified mf = XS.modified $ Hidden . mf . hiddenWindows

setHidden :: Window -> X ()
setHidden window = setHiddenState window iconicState (:)

setNotHidden :: Window -> X ()
setNotHidden window = setHiddenState window normalState L.delete

setHiddenState :: Window -> Int -> (CLong -> [CLong] -> [CLong]) -> X ()
setHiddenState window state cons = do
  setWMState window state
  withDisplay $ \display -> do
    wm_state <- getAtom "_NET_WM_STATE"
    hidden <- fromIntegral <$> getAtom "_NET_WM_STATE_HIDDEN"
    wstate <- fromMaybe [] <$> getProp32 wm_state window
    io $ changeProperty32 display window wm_state aTOM propModeReplace
      (cons hidden wstate)

getHidden :: X (S.Seq Window)
getHidden = withHidden return

withHidden :: (S.Seq Window -> X a) -> X a
withHidden action = do
  hidden <- XS.gets hiddenWindows
  currentStack <- withWindowSet $ return . W.index
  action $ S.filter (`elem` currentStack) hidden
