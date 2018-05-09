module XMonad.Hooks.ShittyManageHooks
    ( manageByPropValue
    , manageByProp32Exists
    , manageByClassName
    , doCenterFloatOffset
    , isFloating
    , isTiled
    , isMaster
    , isMasterTile
    , isFixedSize
    , composeOne'
    ) where

import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Util.CenterRationalRect
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.StackSet (RationalRect(..), floating, peek, index, focusMaster)
import Data.Map (member, notMember)
import Data.Maybe (isJust, fromMaybe)
import Data.Ratio

isMasterTile :: Query Bool
isMasterTile = ask >>= \win -> liftX . withWindowSet $ \ws ->
  let floats = floating ws
      tiled = filter (`notMember` floats) $ index ws
      master = head tiled
  in return (win == master)

isMaster :: Query Bool
isMaster = ask >>= \win -> liftX . withWindowSet $ \ws -> return $
  case peek (focusMaster ws) of
    Just mw -> win == mw
    Nothing -> False

isFloating :: Query Bool
isFloating = ask >>= \w -> liftX . withWindowSet $ return . member w . floating

isTiled :: Query Bool
isTiled = fmap not isFloating

isFixedSize :: Query Bool
isFixedSize = ask >>= (\w -> liftX $ withDisplay $ \d -> do
    sh <- io $ getWMNormalHints d w
    return $ sh_min_size sh /= Nothing && sh_min_size sh == sh_max_size sh )

composeOne' :: [MaybeManageHook] -> ManageHook
composeOne' [] = idHook
composeOne' (x:xs) = do
    r <- x
    case r of
        Just h -> return h
        Nothing -> composeOne' xs

prop32Exists :: String -> Query Bool
prop32Exists p = fmap isJust $ ask >>= liftX . getProp32s p

manageByPropValue :: ManageHook -> [(String,String)] -> [MaybeManageHook]
manageByPropValue m = map (\(p,v) -> stringProperty p =? v -?> m)

manageByProp32Exists :: ManageHook -> [String] -> [MaybeManageHook]
manageByProp32Exists m = map ((-?> m) . prop32Exists)

manageByClassName :: ManageHook -> [String] -> [MaybeManageHook]
manageByClassName m = map ((-?> m) . (className =?))

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
