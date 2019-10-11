{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module XMonad.Layout.HiddenQueueLayout
  ( HiddenQueueLayout(..)
  ) where

import XMonad hiding (tile, focus)
import XMonad.Actions.Hidden
import XMonad.StackSet hiding (visible, delete)
import XMonad.Util.Hidden

import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import Control.Monad (ap, msum, when)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S

data HiddenQueueLayout a = HQLayout
  { sideWindowsNum :: Int      -- ^ Number of windows on the side area.
  , masterRatio    :: Rational -- ^ Width ratio for the master area.
  , sideTopRatio   :: Rational -- ^ Height ratio for the top side window.
  , resizeRatio    :: Rational -- ^ Ratio inc/decrement when resizing.
  } deriving (Show, Read)

instance LayoutClass HiddenQueueLayout Window where
  doLayout l@(HQLayout swn mr sr _) rect stack = do
    hidden <- getHidden
    fcswin <- gets (W.peek . windowset)
    let viswin = visible stack hidden
        shouldHide = length viswin > swn + 1
        shouldUnhide = hidden /= S.empty && length viswin < swn + 1
        toUnhide = rightmost hidden
        tiled' | shouldHide = delete 1 viswin
               | shouldUnhide = toUnhide : viswin
               | otherwise = viswin
    when shouldHide $
      hideWindowAndAct (<|) (viswin !! 1) (pure ())
    when shouldUnhide $
      unhideWindowAndAct toUnhide (pure ())
    return . (, Nothing) . ap zip (tile swn mr sr rect . length) $ tiled'

  handleMessage lyt@(HQLayout swnum mratio stratio rratio) msg =
    return $ msum
      [ fmap resize (fromMessage msg)
      , fmap incsid (fromMessage msg)
      ]
    where
      resize Shrink = lyt { masterRatio = max 0 (mratio - rratio) }
      resize Expand = lyt { masterRatio = min 1 (mratio + rratio) }
      incsid (IncMasterN d) = lyt { sideWindowsNum = max 0 (swnum + d) }

  description (HQLayout n _ _ _) = "HQLayout " ++ show n

-- | TODO: add haddoc for this function
tile :: Int -> Rational -> Rational -> Rectangle -> Int -> [Rectangle]
tile 0 _ _ r _ = [r]
tile 1 m _ r n = drop (2 - n) [r1,r2] where (r1,r2) = splitHorizontallyBy m r
tile 2 m t r n = drop (3 - n) [r1,r2,r3]
  where (r1,rs) = splitHorizontallyBy m r
        (r2,r3) = splitVerticallyBy t rs
tile n m _ r w = drop (length (r1:rss) - w) (r1:rss)
  where (r1,rs) = splitHorizontallyBy m r
        rss = splitVertically (w - 1) rs

-- | Filters hidden windows from a Stack into a list.
visible :: Stack Window -> Seq Window -> [Window]
visible s h = integrate' $ W.filter (`notElem` h) s

-- | Deletes an element of a list given its index.
delete :: Int -> [a] -> [a]
delete _ [] = []
delete 0 (x:xs) = xs
delete i (x:xs) = x : delete (i - 1) xs
