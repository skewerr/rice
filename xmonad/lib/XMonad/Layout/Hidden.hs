{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}

module XMonad.Layout.Hidden
  ( hidden
  ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Hidden (Hidden(..))
import XMonad.Util.ExtensibleState as XS
import XMonad.Layout.LayoutModifier
import qualified XMonad.Layout.BoringWindows as BW

import Data.Foldable (toList)

data HiddenL a = HiddenL deriving (Read, Show)

hidden :: l Window -> ModifiedLayout HiddenL l Window
hidden = ModifiedLayout HiddenL

instance LayoutModifier HiddenL Window where
  modifierDescription = const "Hidden"

  modifyLayout HiddenL wksp rect = do
    allHidden <- XS.gets hiddenWindows
    let filtered = W.stack wksp >>= W.filter (`notElem` allHidden)
    runLayout (wksp {W.stack = filtered}) rect

  handleMess HiddenL m
    | Just BW.UpdateBoring <- fromMessage m = do
        hidden <- XS.gets hiddenWindows
        ws <- XMonad.gets (W.workspace . W.current . windowset)
        flip sendMessageWithNoRefresh ws $ BW.Replace "Hidden" (toList hidden)
        return Nothing
    | otherwise = return Nothing
