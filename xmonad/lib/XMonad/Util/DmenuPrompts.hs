--------------------------------------------------------------------------------
-- | Some preset functions for dmenu prompts, since I don't like XMonad.Prompt
-- and refuse to use it instead of the good ol' dmenu.
--------------------------------------------------------------------------------

module XMonad.Util.DmenuPrompts where

import XMonad hiding (workspaces)
import XMonad.StackSet (workspaces, tag)
import XMonad.Actions.DynamicWorkspaceOrder (getSortByOrder)

import System.IO
import System.Process (runInteractiveProcess)
import Control.Monad (when)

runProcessWithInput :: MonadIO m => FilePath -> [String] -> String -> m String
runProcessWithInput cmd args input = io $ do
  (pin, pout, perr, _) <- runInteractiveProcess cmd args Nothing Nothing
  hPutStr pin input
  hClose pin
  output <- hGetContents pout
  when (output == output) $ return ()
  hClose pout
  hClose perr
  return output

menuArgs' :: String -> [String] -> [String] -> X String
menuArgs' mcmd args opts = fmap (filter (/= '\n')) $
  runProcessWithInput mcmd args (unlines opts)

dmenuArgs :: [String] -> [String] -> X String
dmenuArgs = menuArgs' "dmenu"

workspaceMenuArgs :: String -> [String] -> X String
workspaceMenuArgs m a = do
  ws <- gets (workspaces . windowset)
  sort <- getSortByOrder
  menuArgs' m a . filter (/= "NSP") . map tag $ sort ws

workspaceMenu :: String -> X String
workspaceMenu = flip workspaceMenuArgs []

workspaceDmenuArgs :: [String] -> X String
workspaceDmenuArgs = workspaceMenuArgs "dmenu"

workspaceDmenu :: X String
workspaceDmenu = workspaceDmenuArgs []
