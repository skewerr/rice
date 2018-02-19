{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- TODO: rewrite this entire thing cause it sucks
-- TODO: make Shrink/Expand messages work again
-- TODO: think of a better name (will probably become ShittyLayout)
-- TODO: use ExtensibleState instead of relying on the minimize modifier

module XMonad.Layout.SResizableTile (
    SResizableTall(..),
    SMirrorResize(..)
) where

import Control.Monad
import Data.List ((\\))
import XMonad.Actions.Minimize
import XMonad hiding (tile, splitVertically, splitHorizontallyBy)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

data SMirrorResize = SMShrink | SMExpand deriving Typeable
instance Message SMirrorResize

data SResizableTall a = SRTall
    { nwin    :: Int        -- number of windows we want to show onscreen
    , delta   :: Rational   -- change when resizing
    , mratio  :: Rational   -- width ratio for master area
    , sratios :: [Rational] -- ratio for non-master windows
    } deriving (Show, Read)

instance LayoutClass SResizableTall Window where
    doLayout (SRTall w _ mr srs) r s = do
        when (length windowList > w) $ minimizeWindow' First (return ()) (windowList !! 1 :: Window)
        when (W.focus s `notElem` windowList') $ windowsNoRefresh (W.focusWindow (windowList' !! 0))
        -- when (length windowList < w) $ withFirstMinimized maximizeWindowAndFocus
        return . (\x -> (x, Nothing)) . ap zip
            (tile mr (srs ++ repeat 1) w r . length) $ windowList'
        where
            windowList = W.integrate s
            windowList' = if length windowList > w
                              then deleteN 1 windowList
                              else windowList
    handleMessage (SRTall nmaster delta frac mfrac) m =
        do ms <- (W.stack . W.workspace . W.current) `fmap` gets windowset
           fs <- (M.keys . W.floating) `fmap` gets windowset
           return $ ms >>= unfloat fs >>= handleMesg
        where handleMesg s = msum [fmap resize (fromMessage m)
                                  ,fmap (\x -> mresize x s) (fromMessage m)
                                  ,fmap incmastern (fromMessage m)]
              unfloat fs s = if W.focus s `elem` fs
                               then Nothing
                               else Just (s { W.up = (W.up s) \\ fs
                                            , W.down = (W.down s) \\ fs })
              resize Shrink = SRTall nmaster delta (max 0 $ frac-delta) mfrac
              resize Expand = SRTall nmaster delta (min 1 $ frac+delta) mfrac
              mresize SMShrink s = mresize' s delta
              mresize SMExpand s = mresize' s (0-delta)
              mresize' s d = let n = length $ W.up s
                                 total = n + (length $ W.down s) + 1
                                 pos = if n == (nmaster-1) || n == (total-1) then n-1 else n
                                 mfrac' = modifymfrac (mfrac ++ repeat 1) d pos
                             in SRTall nmaster delta frac $ take total mfrac'
              modifymfrac [] _ _ = []
              modifymfrac (f:fx) d n | n == 0    = f+d : fx
                                     | otherwise = f : modifymfrac fx d (n-1)
              incmastern (IncMasterN d) = SRTall (max 1 (nmaster+d)) delta frac mfrac
    description _ = "SRTall"

windowsNoRefresh f = modify (\s -> s { windowset = f $ windowset s })

deleteN :: Int -> [a] -> [a]
deleteN _ [] = []
deleteN n (x:xs) | n == 0 = xs
                 | otherwise = x : deleteN (n - 1) xs

tile :: Rational -> [Rational] -> Int -> Rectangle -> Int -> [Rectangle]
tile mr srs nw wr n = if n < nw
    then splitVertically (tail srs) n r2
    else r1 : splitVertically (tail srs) (nw - 1) r2
    where
        (r1, r2) = splitHorizontallyBy mr wr

splitVertically :: RealFrac r => [r] -> Int -> Rectangle -> [Rectangle]
splitVertically [] _ r = [r]
splitVertically _ n r | n < 2 = [r]
splitVertically (f:fx) n (Rectangle sx sy sw sh) = Rectangle sx sy sw smallh :
    splitVertically fx (n-1) (Rectangle sx (sy+fromIntegral smallh) sw (sh-smallh))
  where smallh = min sh (floor $ fromIntegral (sh `div` fromIntegral n) * f) --hmm, this is a fold or map.

splitHorizontallyBy :: RealFrac r => r -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
    ( Rectangle sx sy leftw sh
    , Rectangle (sx + fromIntegral leftw) sy (sw-fromIntegral leftw) sh)
  where leftw = floor $ fromIntegral sw * f

-- vim: set expandtab tabstop=4 shiftwidth=4 :
