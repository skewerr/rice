module XMonad.Util.CenterRationalRect
  ( centerRRect
  , centerRRectOffsetX
  , centerRRectOffsetY
  , centerRRectOffsetXY
  , centerIRect
  , centerIRectOffsetX
  , centerIRectOffsetY
  , centerIRectOffsetXY
  ) where

import Data.Ratio
import XMonad.StackSet (RationalRect(..))

centerIRect :: (Integral a) => a -> a -> a -> a -> RationalRect
centerIRect w h sw sh = centerIRectOffsetXY 0 0 w h sw sh

centerIRectOffsetX :: (Integral a) => a -> a -> a -> a -> a -> RationalRect
centerIRectOffsetX ox w h sw sh = centerIRectOffsetXY ox 0 w h sw sh

centerIRectOffsetY :: (Integral a) => a -> a -> a -> a -> a -> RationalRect
centerIRectOffsetY oy w h sw sh = centerIRectOffsetXY 0 oy w h sw sh

centerIRectOffsetXY :: (Integral a) => a -> a -> a -> a -> a -> a -> RationalRect
centerIRectOffsetXY ox oy w h sw sh = centerRRectOffsetXY orx ory rw rh
  where orx = (toInteger ox % toInteger sw)
        ory = (toInteger oy % toInteger sh)
        rw = (toInteger w % toInteger sw)
        rh = (toInteger h % toInteger sh)

centerRRect :: Rational -> Rational -> RationalRect
centerRRect rw rh = centerRRectOffsetXY 0 0 rw rh

centerRRectOffsetX :: Rational -> Rational -> Rational -> RationalRect
centerRRectOffsetX ox rw rh = centerRRectOffsetXY ox 0 rw rh

centerRRectOffsetY :: Rational -> Rational -> Rational -> RationalRect
centerRRectOffsetY oy rw rh = centerRRectOffsetXY 0 oy rw rh

centerRRectOffsetXY :: Rational -> Rational -> Rational -> Rational -> RationalRect
centerRRectOffsetXY ox oy rw rh = RationalRect rx ry rw rh
  where rx = (1 - rw + ox) / 2
        ry = (1 - rh + oy) / 2
