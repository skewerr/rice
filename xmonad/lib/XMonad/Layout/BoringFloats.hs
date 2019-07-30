{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}

module XMonad.Layout.BoringFloats
  ( boringFloats
  ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import qualified XMonad.Layout.BoringWindows as BW

import qualified Data.Map as M

data BoringFloatsL a = BoringFloatsL deriving (Read, Show)

boringFloats :: l Window -> ModifiedLayout BoringFloatsL l Window
boringFloats = ModifiedLayout BoringFloatsL

instance LayoutModifier BoringFloatsL Window where
  modifierDescription = const "BoringFloats"

  handleMess BoringFloatsL m
    | Just BW.UpdateBoring <- fromMessage m = do
        ws <- XMonad.gets (W.workspace . W.current . windowset)
        floats <- XMonad.gets (M.keys . W.floating . windowset)
        flip sendMessageWithNoRefresh ws $ BW.Replace "Floating" floats
        return Nothing
    | otherwise = return Nothing
