module Config.Layouts where

import XMonad ((|||))
import XMonad.Layout.BoringWindows
import XMonad.Layout.Gaps
import XMonad.Layout.Hidden
import XMonad.Layout.HiddenQueueLayout
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.StateFull

import qualified Config.Dimensions as D

layoutHook
  = lessBorders OnlyScreenFloat
  . boringWindows
  . onWorkspaces ["web", "gimp"] (fullLayout ||| hiddenQueueLayout)
  $ hiddenQueueLayout ||| fullLayout

hiddenQueueLayout
  = hidden
  . gaps [ (R, D.horizontalPadding), (L, D.horizontalPadding)
         , (U, D.verticalPadding + D.panelHeight)
         , (D, D.verticalPadding)
         ]
  . spacing D.windowGap
  $ HQLayout 1 D.masterRatio D.topWindowRatio D.resizeRatio

fullLayout = noBorders . gaps [(U, D.panelHeight)] $ StateFull
