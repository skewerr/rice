--------------------------------------------------------------------------------
-- | Does dmenu things with dynamic workspaces.
--------------------------------------------------------------------------------

module XMonad.Actions.DmenuWorkspaces
  ( selectWorkspace
  , moveToWorkspace
  , renameWorkspace
  , removeWorkspace
  , withNthWorkspace
  , chooseWorkspace
  ) where

import XMonad hiding (workspaces)
import XMonad.StackSet hiding (filter)
import qualified XMonad.Actions.DynamicWorkspaces as DW
--import XMonad.Actions.DynamicWorkspaces
import XMonad.Util.DmenuPrompts
-- import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.Actions.DynamicWorkspaceOrder (getSortByOrder, updateName, removeName)

args =
  [ "-nb", "#ffffff"
  , "-nf", "#000000"
  , "-sb", "#c0c0c0"
  , "-sf", "#000000"
  , "-h", "20"
  , "-i", "-p", "»"
  , "-fn", "DejaVu Sans Mono:size=9"
  , "-fn", "IPAGothic:size=10"
  ]

goTo :: String -> X ()
goTo "" = return ()
goTo ws = do
  s <- gets windowset
  if tagMember ws s
    then windows $ greedyView ws
    else DW.addWorkspace ws

sendTo :: Window -> String -> X ()
sendTo _  "" = return ()
sendTo wn ws = do
  whenX (not . tagMember ws <$> gets windowset) $
    DW.addWorkspace ws
  windows $ greedyView ws . shiftWin ws wn

selectWorkspace :: X ()
selectWorkspace = chooseWorkspace >>= goTo

chooseWorkspace :: X String
chooseWorkspace = workspaceDmenuArgs args

moveToWorkspace :: Window -> X ()
moveToWorkspace w = do
  chosenWorkspace <- chooseWorkspace
  sendTo w chosenWorkspace

renameWorkspace :: X ()
renameWorkspace = dmenuArgs args [] >>= renameWorkspaceByName

renameWorkspaceByName :: String -> X ()
renameWorkspaceByName "" = return ()
renameWorkspaceByName ws = do
  old <- gets (currentTag . windowset)
  updateName old ws
  windows $ \s ->
    let sett wk = wk { tag = ws }
        setscr scr = scr { workspace = sett $ workspace scr }
        sets q = q { current = setscr $ current q }
     in sets $ removeWorkspace' ws s

removeWorkspace' :: (Eq i) => i -> StackSet i l a sid sd -> StackSet i l a sid sd
removeWorkspace' torem s@(StackSet { current = scr@(Screen { workspace = wc })
                                  , hidden = hs })
  = let (xs, ys) = break ((== torem) . tag) hs
     in removeWorkspace'' xs ys
  where meld Nothing Nothing = Nothing
        meld x Nothing = x
        meld Nothing x = x
        meld (Just x) (Just y) = differentiate (integrate x ++ integrate y)
        removeWorkspace'' xs (y:ys) = s { current = scr { workspace = wc { stack = meld (stack y) (stack wc) } }
                                        , hidden = xs ++ ys }
        removeWorkspace'' _  _      = s

removeWorkspace :: X ()
removeWorkspace = do
  tag <- gets (currentTag . windowset)
  DW.removeWorkspace
  removeName tag

withNthWorkspace :: (String -> WindowSet -> WindowSet) -> Int -> X ()
withNthWorkspace job wnum = do
  sort <- getSortByOrder
  ws <- gets (filter (/= "NSP") . map tag . sort . workspaces . windowset)
  case drop wnum ws of
    (w:_) -> windows $ job w
    [] -> return ()
