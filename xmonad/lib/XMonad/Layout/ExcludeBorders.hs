{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module XMonad.Layout.ExcludeBorders where

import Control.Monad
import Data.Monoid
import qualified Data.Map as Map
import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.StackSet hiding (filter)
import XMonad.Actions.Pip

setBorders :: [Window] -> Dimension -> X ()
setBorders ws bw =
  withDisplay $ \dp -> mapM_ (\w -> io $ setWindowBorderWidth dp w bw) ws

data ExcludeBorders p a =
  ExcludeBorders p [a]
  deriving (Eq, Read, Show)

excludeBorders :: p -> l a -> ModifiedLayout (ExcludeBorders p) l a
excludeBorders qs = ModifiedLayout (ExcludeBorders qs [])

instance (Show p, Read (ExcludeBorders p Window), Excludes p) =>
  LayoutModifier (ExcludeBorders p) Window where
  unhook (ExcludeBorders _p s) = asks (borderWidth . config) >>= setBorders s
  redoLayout (ExcludeBorders p _s) _ mst wrs = do
    ws <- withWindowSet (\wset -> excludes p wset mst wrs)
    setBorders ws 0
    return (wrs, Just $ ExcludeBorders p ws)

class Excludes p where
  excludes :: p -> WindowSet -> Maybe (Stack Window) -> [(Window, Rectangle)]
    -> X [Window]

data ExcludeProp = ExcludeClassName String | ExcludeAppName String | ExcludePip
  deriving (Eq, Read, Show)

instance Excludes [ExcludeProp] where
  excludes qs wset mst _wrs =
    let ws = integrate' mst ++ [w | (w, _) <- Map.toList . floating $ wset]
    in flip filterM ws $ \w ->
      fmap (getAny . mconcat) .
      sequenceA . map (fmap Any . flip runQuery w . toQuery) $
      qs

toQuery :: ExcludeProp -> Query Bool
toQuery (ExcludeClassName s) = className =? s
toQuery (ExcludeAppName s) = appName =? s
toQuery ExcludePip = isPipWindow
