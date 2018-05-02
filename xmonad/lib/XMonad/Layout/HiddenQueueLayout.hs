{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XMonad.Layout.HiddenQueueLayout
    (HiddenQueueLayout(..)) where

import XMonad hiding (tile)
import qualified XMonad.StackSet as W

import XMonad.Util.Minimize
import XMonad.Actions.Minimize
import qualified XMonad.Util.ExtensibleState as XS

import Control.Monad (ap, msum, when)
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.List as L
import qualified Data.Map as M

-- Layout section.

data HiddenQueueLayout a = HQLayout
    { sideWindowsNum :: Int      -- number of windows on the side area
    , masterRatio    :: Rational -- width ratio for the master area
    , sideTopRatio   :: Rational -- height ratio for the top side window*
    , resizeRatio    :: Rational -- ratio inc/decrement when resizing
    } deriving (Show, Read)

instance LayoutClass HiddenQueueLayout Window where

    -- Looking at traces of the values for stack, apparently the stack that this
    -- function receives is only the stack of tiling windows, so it makes no
    -- sense for us to do any floating filtering.
    doLayout (HQLayout swnum mratio stratio _) rect stack = do
        minimized <- currentlyMinimized
        when (length (tilesList minimized) > swnum + 1) $
            minimizeWindow' (++) (tilesList minimized !! 1) (return ())
        let tilesList' | length (tilesList minimized) > swnum + 1 = deleteInd 1 (tilesList minimized)
                       | otherwise = tilesList minimized
        return . (\x -> (x, Nothing)) . ap zip (tile swnum mratio stratio rect . length) $ tilesList'
        where
            tilesList hidden = case W.filter (`notElem` hidden) stack of
                                   Just newStack -> W.integrate newStack
                                   Nothing -> []
            deleteInd _ [] = []
            deleteInd i (x:xs) | i == 0 = xs
                               | otherwise = x : deleteInd (i - 1) xs

    handleMessage lyt@(HQLayout swnum mratio stratio rratio) msg =
        return $ msum [ fmap resize (fromMessage msg)
                      , fmap incsid (fromMessage msg)
                      ]
        where resize Shrink = lyt { masterRatio = max 0 (mratio - rratio) }
              resize Expand = lyt { masterRatio = min 1 (mratio + rratio) }
              incsid (IncMasterN d) = lyt { sideWindowsNum = max 0 (swnum + d) }

    description _ = "HQLayout"

tile :: Int -> Rational -> Rational -> Rectangle -> Int -> [Rectangle]
tile 0 _ _ r _ = [r]
tile 1 m _ r n = let (r1,r2) = splitHorizontallyBy m r in drop (2 - n) [r1,r2]
tile 2 m t r n = let (r1,rs) = splitHorizontallyBy m r
                     (r2,r3) = splitVerticallyBy t rs
                  in drop (3 - n) [r1, r2, r3]
tile n m _ r w = let (r1,rs) = splitHorizontallyBy m r
                     rss = splitVertically (w - 1) rs
                  in drop (length (r1 : rss) - w) (r1:rss)

-- vim: set et ts=4 sw=4 :
