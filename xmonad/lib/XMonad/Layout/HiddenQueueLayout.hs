{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XMonad.Layout.HiddenQueueLayout
  ( HiddenQueueLayout(..)
  ) where

import XMonad hiding (tile)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Util.Hidden
import XMonad.Actions.Hidden

import Control.Monad (ap, msum, when)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Sequence as S

data HiddenQueueLayout a = HQLayout
  { sideWindowsNum :: Int      -- number of windows on the side area
  , masterRatio    :: Rational -- width ratio for the master area
  , sideTopRatio   :: Rational -- height ratio for the top side window (swn=2)
  , resizeRatio    :: Rational -- ratio inc/decrement when resizing
  } deriving (Show, Read)

instance LayoutClass HiddenQueueLayout Window where
  doLayout (HQLayout swnum mratio stratio _) rect stack = do
    tiled <- tilesList <$> getHidden
    when (length tiled > swnum + 1) $
      hideWindowAndAct (S.<|) (tiled !! 1) (pure ())
    let tiled' | length tiled > swnum + 1 = deleteInd 1 tiled
               | otherwise = tiled
    return . (\x -> (x, Nothing))
      . ap zip (tile swnum mratio stratio rect . length) $ tiled'
    where
      tilesList st | Just nst <- W.filter (`notElem` st) stack = W.integrate nst
                   | otherwise = []
      deleteInd _ [] = []
      deleteInd 0 (x:xs) = xs
      deleteInd i (x:xs) = x : deleteInd (i-1) xs

  handleMessage lyt@(HQLayout swnum mratio stratio rratio) msg =
    return $ msum
      [ fmap resize (fromMessage msg)
      , fmap incsid (fromMessage msg)
      ]
    where
      resize Shrink = lyt { masterRatio = max 0 (mratio - rratio) }
      resize Expand = lyt { masterRatio = min 1 (mratio + rratio) }
      incsid (IncMasterN d) = lyt { sideWindowsNum = max 0 (swnum + d) }

  description _ = "HQLayout"

tile :: Int -> Rational -> Rational -> Rectangle -> Int -> [Rectangle]
tile 0 _ _ r _ = [r]
tile 1 m _ r n = drop (2 - n) [r1,r2] where (r1,r2) = splitHorizontallyBy m r
tile 2 m t r n = drop (3 - n) [r1,r2,r3]
  where (r1,rs) = splitHorizontallyBy m r
        (r2,r3) = splitVerticallyBy t rs
tile n m _ r w = drop (length (r1:rss) - w) (r1:rss)
  where (r1,rs) = splitHorizontallyBy m r
        rss = splitVertically (w - 1) rs

-- vim: set et ts=2 sw=2 :
