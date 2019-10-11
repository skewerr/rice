{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module XMonad.Layout.ZeroBorders (zeroBorders) where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.StackSet (integrate)

data ZeroBorders a = ZeroBorders deriving (Read, Show)

zeroBorders :: l Window -> ModifiedLayout ZeroBorders l Window
zeroBorders = ModifiedLayout ZeroBorders

instance LayoutModifier ZeroBorders Window where
  modifierDescription = const "ZeroBorders"

  redoLayout ZeroBorders _ Nothing wrs = return (wrs, Nothing)
  redoLayout ZeroBorders _ (Just s) wrs = do
    mapM_ setZeroBorder $ integrate s
    return (wrs, Nothing)

setZeroBorder :: Window -> X ()
setZeroBorder w = withDisplay $ \d -> io $ setWindowBorderWidth d w 0
