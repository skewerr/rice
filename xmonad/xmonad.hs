import XMonad
import XMonad.Config
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.FloatKeys
import XMonad.Actions.Minimize
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.XUtils (fi)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run
import XMonad.Layout.ResizableTile
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.BoringWindows
import XMonad.Layout.Minimize
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps

import Control.Monad
import Text.Printf   (printf)
import System.IO (Handle)
import Data.Monoid
import Data.Ratio
import Data.Maybe

import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map as M

wrr = W.RationalRect

------------------------------------------------------------------ main function

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad . ewmh $ def
        { terminal           = "urxvt -e tmux"
        , focusFollowsMouse  = True

        , focusedBorderColor = "#656565"
        , normalBorderColor  = "#656565"
        , borderWidth        = 2

        , modMask            = modm
        , workspaces         = myWorkspaces

        , startupHook        = myStartupHook
        , logHook            = myLogHook xmproc
        , manageHook         = myManageHook
        , handleEventHook    = myHandleEventHook
        , layoutHook         = myLayoutHook
        }

        `additionalKeys`             myKeyBindings
        `additionalMouseBindings`    myMouseBindings
        `removeKeys`                 myRemovedBindings

----------------------------------------------------------------- some variables

(fullwidth, fullheight) = (1920 :: Integer, 1080 :: Integer) -- self explanatory
(cellwidth, cellheight) = (   7 :: Integer,   14 :: Integer) -- terminal cells
(hpadding, vpadding)    = (  36 :: Integer,   39 :: Integer) -- gaps

panelheight = 22 :: Integer -- height of the top panel
borderwidth =  2 :: Integer -- window border
termpadding =  5 :: Integer -- terminal padding (aka internal border)
windowgap   =  7 :: Integer -- window margin, will appear twofold

--myWorkspaces = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]
myWorkspaces = ["月", "火", "水", "木", "金", "土", "日"]
modm = mod4Mask

-------------------------------------------------------------------- prettyprint

customPP :: PP
customPP = def
    { ppHidden = emptyStr
    , ppVisible = emptyStr
    , ppHiddenNoWindows = emptyStr
    , ppUrgent = emptyStr
    , ppTitle = emptyStr
    , ppLayout = emptyStr
    , ppSep = ""
    , ppWsSep = ""
    , ppCurrent = id
    , ppExtras = [ fmap surround minimizedNum ]
    }

    where
        emptyStr = \_ -> ""
        surround = fmap $ \s -> if (read s /= 0) then " (" ++ s ++ ")"
                                                 else ""

-------------------------------------------------------------------- scratchpads

scratchpads :: [NamedScratchpad]
scratchpads = [ NS "music" "urxvt -title music -e ncmpcpp"
                  (title =? "music")
                  (customFloating $ wrr mrx mry mrw mrh)

              , NS "term" "urxvt -title scratch -e tmux"
                  (title =? "scratch")
                  (customFloating $ wrr trx try trw trh)

              , NS "volume" "pavucontrol"
                  (className =? "Pavucontrol")
                  (customFloating $ wrr vrx vry vrw vrh)
              ]

              where
                  trx = (1 - trw)/2
                  try = (1 - trh + prh)/2
                  trw = tw % fullwidth
                  trh = th % fullheight

                  prh = panelheight % fullheight
                  tw  = 100 * cellwidth + 2 * (borderwidth + termpadding)
                  th  = 27 * cellheight + 2 * (borderwidth + termpadding)

                  vrx = (1 - vrw)/2
                  vry = (1 - vrh + prh)/2
                  vrw = 0.4
                  vrh = 0.6

                  mrx = - borderwidth % fullwidth
                  mry = 1 - mrh
                  mrw = 1 + 2*borderwidth % fullwidth
                  mrh = trh

-------------------------------------------------------------------------- hooks

myStartupHook = startupHook def  <+>
                docksStartupHook

myLogHook h = (dynamicLogWithPP $ customPP { ppOutput = shittyFilter h })
    >> updatePointer (0.5, 0.5) (0, 0.25)

    where
        wrapIcon :: String -> String
        wrapIcon s = "<icon=.local/share/icons/xpm/" ++ s ++ ".xpm/>"

        shittyFilter :: Handle -> String -> IO ()
        shittyFilter h s =
            if ' ' `elem` s then
                let [wkspc,right] = words s
                    spaceNum  = length right + 1
                    spaceStr  = take spaceNum $ repeat ' '
                in  hPutStrLn h (spaceStr ++ wrapIcon wkspc ++ " " ++ right)
            else
                hPutStrLn h $ wrapIcon s

myHandleEventHook =
    fullscreenEventHook <+>
    handleEventHook def <+>
    docksEventHook

myManageHook = composeAll $
    namedScratchpadManageHook scratchpads :
    manageDocks :
    manageHook def :
    map (--> doCenterFloat') (isDialog : byClass ++ byProp)

    where
        nSMHook = namedScratchpadManageHook
        byClass = map (className =?)
            [ "feh"
            , "mpv"
            , "sun-awt-X11-XFramePeer"
            , "Pinentry"
            ]
        byProp  = map (\(prop, val) -> stringProperty prop =? val)
            [ ("WM_WINDOW_ROLE", "GtkFileChooserDialog")
            , ("WM_WINDOW_ROLE", "gimp-message-dialog")
            , ("WM_WINDOW_ROLE", "gimp-toolbox-color-dialog")
            , ("WM_WINDOW_ROLE", "file-png") ]

-- Custom manage hook to center a window in the AVAILABLE window area.

doCenterFloat' :: ManageHook
doCenterFloat' = doFloatDep move where
    move (W.RationalRect _ _ w h) =
        let phr = panelheight % fullheight
            mhr = 1 - phr

            aw = w + (2*borderwidth) % fullwidth
            ah = h + (2*borderwidth) % fullheight

            cx = (1 - nw)/2
            cy | nh <= mhr = (1 - nh + phr)/2
               | otherwise = (1 - nh)/2
            nw |   aw <= 1 = aw
               | otherwise = w
            nh |   ah <= 1 = ah
               | otherwise = h

        in wrr cx cy nw nh

------------------------------------------------------------------------ layouts

tallLayout = lessBorders OnlyFloat
    . minimize
    . boringWindows
    . limitWindows 3
    . spacing (fi windowgap)
    $ ResizableTall nmaster delta ratio [botwrat, topwrat]

    where
        nmaster = 1
        delta   = 2 % (fullheight - 2*(vpadding) - panelheight)
        ratio   = 1 - rightcolratio

        topwrat = 322 / 490 -- TODO: ratio should depend on padding and line #s
        botwrat = 658 / 490 -- TODO: same as above

        rightcolratio = (84*cellwidth + 2*allgaps) % (fullwidth - 2*hpadding)
        allgaps = windowgap + borderwidth + termpadding

fullLayout = noBorders . boringWindows . gaps [(U, fi panelheight)] $ Full

addGaps = gaps [ (U, fi $ vpadding + panelheight)
               , (D, fi vpadding)
               , (R, fi hpadding)
               , (L, fi hpadding)
               ]

myLayoutHook = (addGaps tallLayout ||| fullLayout)

---------------------------------------------------- keyboard and mouse bindings

myKeyBindings :: [((KeyMask, KeySym), X ())]
myKeyBindings =
    -- resizableTall messages
    [ ((modm, xK_a), sendMessage MirrorShrink)
    , ((modm, xK_z), sendMessage MirrorExpand)
    , ((modm, xK_s), replicateM_ (fi cellheight) $ sendMessage MirrorShrink)
    , ((modm, xK_x), replicateM_ (fi cellheight) $ sendMessage MirrorExpand)

    -- minimizing/restoring/swapping windows
    , ((modm, xK_m),                  withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_m),    withLastMinimized maximizeWindowAndFocus)
    , ((modm .|. mod1Mask, xK_m),     swapWindow)
    , ((modm .|. mod1Mask, xK_Left),  swapWindowLeft)
    , ((modm .|. mod1Mask, xK_Right), swapWindowRight)

    -- cycling through windows
    , ((modm, xK_j), focusDown)
    , ((modm, xK_k), focusUp)

    -- centering float windows
    , ((modm, xK_c), withFocused centerWindow)

    -- workspaces
    , ((modm, xK_Right), moveTo Next (WSIs notSP))
    , ((modm, xK_Left) , moveTo Prev (WSIs notSP))

    -- resizing floating windows
    , ((modm .|. controlMask, xK_Up),    withFocused $
        keysResizeWindow ( 0,  1) (1%2, 1%2))
    , ((modm .|. controlMask, xK_Down),  withFocused $
        keysResizeWindow ( 0, -1) (1%2, 1%2))
    , ((modm .|. controlMask, xK_Right), withFocused $
        keysResizeWindow ( 1,  0) (1%2, 1%2))
    , ((modm .|. controlMask, xK_Left),  withFocused $
        keysResizeWindow (-1,  0) (1%2, 1%2))

    -- killing window copy
    , ((modm .|. shiftMask, xK_c), kill1)

    -- scratchpads
    , ((modm .|. controlMask, xK_Return), namedSA scratchpads "term")
    , ((modm .|. controlMask, xK_m),      namedSA scratchpads "music")
    , ((modm .|. controlMask, xK_v),      namedSA scratchpads "volume")
    ]
    ++
    -- handles moving/copying clients and accessing workspaces
    [ ((modm .|. modKey, wsNum), f wsName)
        | (wsName, wsNum) <- zip myWorkspaces [xK_1 ..]
        , (f, modKey)     <-
            [ (windows . W.view, 0)
            , (windows . W.shift, shiftMask)
            , (copy', shiftMask .|. controlMask) ]
    ]

    where
        notSP        = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
        centerWindow = keysMoveWindowTo (958, 549) (1/2, 1/2)
        namedSA      = namedScratchpadAction

        notify :: MonadIO m => String -> String -> m ()
        notify t s = spawn (printf "notify-send \"%s\" \"%s\"" t s :: String)

        copy' :: String -> X ()
        copy' ws = do
            windows (copy ws);
            windowTitle <- withWindowSet $ traverse (fmap show . getName) .
                W.peek
            notify "copied window" $ "title: " ++ unpack windowTitle
                                     ++ "\nworkspace: " ++ ws
            where
                unpack :: Maybe String -> String
                unpack Nothing = ""
                unpack (Just str) = str

        changeVGaps :: (Int -> Direction2D -> GapMessage) -> Int -> X ()
        changeVGaps m i = do
            cws <- gets (W.workspace . W.current . windowset)
            sendMessageWithNoRefresh (m i U) cws
            sendMessage $ m i D

myMouseBindings =
    -- allows floating window resizing with mod+right click
    [ ((modm, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
    ]

myRemovedBindings =
    [ (modm, xK_p)               -- default: dmenu_run
    , (modm .|. shiftMask, xK_r) -- default: does this do anything?
    ]

-- vim: set ts=4 sw=4 expandtab :
