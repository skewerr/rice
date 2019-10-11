-- TODO: fix hideWindow(AndAct) behavior, maybe add hideWindowWith

module XMonad.Actions.Hidden
  ( hideWindow
  , unhideWindow
  , hideWindowAndAct
  , unhideWindowAndAct
  , unhideWindowAndWindows
  , unhideOnFocus
  , swapWithLastHidden
  , swapWithNextHidden
  , withLastHidden
  , withNextHidden
  , rightmost
  , leftmost
  ) where

import XMonad
import XMonad.Util.Hidden

import qualified XMonad.Layout.BoringWindows as BW
import qualified XMonad.StackSet as W

import Data.Map (notMember)
import qualified Data.Sequence as S

hideWindowAndAct :: InsertFunc -> Window -> X () -> X ()
hideWindowAndAct ins win act =
  whenX (modified $ ins win) $ do
    setHidden win
    act

unhideWindowAndAct :: Window -> X () -> X ()
unhideWindowAndAct win act =
  whenX (modified $ delete win) $ do
    setNotHidden win
    broadcastMessage BW.UpdateBoring
    act

unhideWindowAndWindows :: Window -> (WindowSet -> WindowSet) -> X ()
unhideWindowAndWindows win mws = unhideWindowAndAct win (windows mws)

hideWindow :: Window -> X ()
hideWindow win = hideWindowAndAct (flip (S.|>)) win $ do
  BW.focusDown
  windows $ W.sink win

unhideWindow :: Window -> X ()
unhideWindow win = unhideWindowAndWindows win (W.swapMaster . W.focusWindow win)

swapWithHidden :: Window -> Window -> InsertFunc -> X ()
swapWithHidden uwin hwin insf =
  whenX (runQuery isMasterTile uwin) $
    hideWindowAndAct insf uwin $ unhideWindowAndWindows hwin $
      W.swapMaster . W.sink uwin . W.focusWindow hwin

swapWithLastHidden :: Window -> X ()
swapWithLastHidden w = withLastHidden $ flip (swapWithHidden w) (S.<|)

swapWithNextHidden :: Window -> X ()
swapWithNextHidden w = withNextHidden $ flip (swapWithHidden w) (flip (S.|>))

withLastHidden :: (Window -> X a) -> X a
withLastHidden = withHidden . (\f -> f . rightmost)

withNextHidden :: (Window -> X a) -> X a
withNextHidden = withHidden . (\f -> f . leftmost)

unhideOnFocus :: X ()
unhideOnFocus = withFocused $ \fwin ->
  whenX (elem fwin <$> getHidden) $
    unhideWindowAndWindows fwin W.swapMaster

-- these shall fail if the sequence is empty, but we expect that never to happen
leftmost  :: (Eq a) => S.Seq a -> a
rightmost :: (Eq a) => S.Seq a -> a
leftmost seq  | (l S.:< _) <- S.viewl seq = l
              | otherwise = error "empty sequence"
rightmost seq | (_ S.:> r) <- S.viewr seq = r
              | otherwise = error "empty sequence"

-- this won't fail at all, but yeah, I just needed a delete
delete :: (Eq a) => a -> S.Seq a -> S.Seq a
delete x = S.filter (/= x)

-- a query to determine whether a window is tile AND the master window
isMasterTile :: Query Bool
isMasterTile = ask >>= \win -> liftX . withWindowSet $ \ws ->
  let floats = W.floating ws
      tiled = filter (`notMember` floats) $ W.index ws
      master = head tiled
  in return (win == master)
