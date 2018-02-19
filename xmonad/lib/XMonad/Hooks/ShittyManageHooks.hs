module XMonad.Hooks.ShittyManageHooks
    ( manageByPropValue
    , manageByProp32Exists
    , manageByClassName
    , doCenterFloatOffset
    ) where

import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Util.CenterRationalRect
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.StackSet (RationalRect(..))
import Data.Maybe (isJust)
import Data.Monoid
import Data.Ratio

manageByPropValue :: ManageHook -> [(String,String)] -> Query (Endo WindowSet)
manageByPropValue m = composeOne . map (\(p,v) -> stringProperty p =? v -?> m)

manageByProp32Exists :: ManageHook -> [String] -> Query (Endo WindowSet)
manageByProp32Exists m = composeOne . map ((-?> m) . prop32Exists)

manageByClassName :: ManageHook -> [String] -> Query (Endo WindowSet)
manageByClassName m = composeOne . map ((-?> m) . (className =?))

prop32Exists :: String -> Query Bool
prop32Exists p = fmap isJust $ ask >>= liftX . getProp32s p

doCenterFloatOffset :: Integral a => (a,a) -> a -> a -> ManageHook
doCenterFloatOffset (sw,sh) bw ph = doFloatDep move where
    move (RationalRect _ _ rw rh) = centerRRectOffsetY oy nw nh
        where aw = rw + 2 * (toInteger bw % toInteger sw)
              ah = rh + 2 * (toInteger bw % toInteger sh)
              pr = toInteger ph % toInteger sh
              nw | aw <= 1 = aw
                 | otherwise = rw
              nh | ah <= 1 = ah
                 | otherwise = rh
              oy | nh <= (1 - pr) = pr
                 | otherwise = 0
