module Config.Layouts (layoutHook) where

import XMonad (Full(..), (|||))
import XMonad.Layout.BoringWindows
import XMonad.Layout.Hidden
import XMonad.Layout.HiddenQueueLayout
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing hiding (screenBorder, windowBorder)
import XMonad.Layout.ZeroBorders

import qualified Config.Dimensions as D

layoutHook
  = lessBorders OnlyScreenFloat
  . boringWindows
  . onWorkspaces ["anime"] animeLayout
  . onWorkspaces ["web", "gimp"] (fullLayout ||| hiddenQueueLayout)
  . onWorkspaces ["osu"] fullLayout
  $ hiddenQueueLayout ||| fullLayout

hiddenQueueLayout
  = spacingRaw False screenBorder True windowBorder True
  . hidden
  $ HQLayout 2 D.masterRatio D.topWindowRatio D.resizeRatio

fullLayout
  = zeroBorders
  . spacingRaw False (Border D.panelHeight 0 0 0) True (borderAll 0) False
  $ Full

animeLayout
  = spacingRaw False animeBorder True (borderAll 0) False
  $ Full

windowBorder :: Border
windowBorder = borderAll D.windowGap

screenBorder :: Border
screenBorder = incBTop D.panelHeight $
  Border D.verticalPadding   D.verticalPadding
         D.horizontalPadding D.horizontalPadding

animeBorder :: Border
animeBorder = incBAll D.windowGap
            . incBTnB (12 * D.characterHeight)
            . incBLnR (45 * D.characterWidth)
            $ screenBorder

-- these functions should seriously be exported by X.L.Spacing

borderAll :: Integer -> Border
borderAll b = Border b b b b

incBTop :: Integer -> Border -> Border
incBTop n (Border t x y z) = Border (t + n) x y z
incBBot :: Integer -> Border -> Border
incBBot n (Border x b y z) = Border x (b + n) y z
incBRig :: Integer -> Border -> Border
incBRig n (Border x y r z) = Border x y (r + n) z
incBLef :: Integer -> Border -> Border
incBLef n (Border x y z l) = Border x y z (l + n)
incBLnR :: Integer -> Border -> Border
incBLnR n = incBRig n . incBLef n
incBTnB :: Integer -> Border -> Border
incBTnB n = incBTop n . incBBot n
incBAll :: Integer -> Border -> Border
incBAll n = incBTnB n . incBLnR n
