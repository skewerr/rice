import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.FloatKeys
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.XUtils (fi)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run
import XMonad.Layout.NoBorders
import XMonad.Layout.BoringWindows
import XMonad.Layout.Minimize
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps

import Text.Printf
import Data.Ratio

import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map as M

import XMonad.Actions.Minimize
import XMonad.Hooks.ShittyManageHooks
import XMonad.Layout.HiddenQueueLayout
import XMonad.Util.CenterRationalRect

main = do
    xmobarH <- spawnPipe "xmobar"
    xmonad . ewmh $ def
        { terminal           = "st -e tmux"
        , focusFollowsMouse  = True
        , focusedBorderColor = "#b44c21"
        , normalBorderColor  = "#656565"
        , borderWidth        = fi borWid
        , modMask            = modm
        , workspaces         = myWorkspaces
        , startupHook        = myStartupHook
        , logHook            = myLogHook xmobarH
        , manageHook         = myManageHook
        , handleEventHook    = myHandleEventHook
        , layoutHook         = myLayoutHook
        }

        `additionalKeys`          myKeyBindings
        `additionalMouseBindings` myMouseBindings
        `removeKeys`              myRemovedBindings

----------------------------------------------------------------- some variables

scrRes@(scrWid, scrHei) = (1920, 1080)
chaRes@(chaWid, chaHei) = (   7,   14)
(horPad, verPad) = (8, 11)

panHei = 22 -- height of the top panel
borWid =  2 -- border width for the windows
terPad =  5 -- terminal padding (internal border)
winGap =  7 -- window margin, will appear twofold

myWorkspaces = ["月", "火", "水", "木", "金", "土", "日"]
modm = mod4Mask

------------------------------------------------------------- xmobar prettyprint

customPP :: PP
customPP = def
    { ppHidden = const "", ppHiddenNoWindows = const "", ppVisible = const ""
    , ppUrgent = const ""
    , ppOrder = \(ws:_:_:rs) -> ws:rs
    , ppSep = " "
    , ppCurrent = toIcon
    , ppExtras = [ minimizedNum ]
    }
    where
        minimizedNum = withMinimized $ return . catchZero . length
        catchZero = \l -> if l == 0 then Nothing else Just . inParens $ show l
        inParens = \s -> "(" ++ s ++ ")"
        toIcon = \s -> "<icon=.local/share/icons/xpm/" ++ s ++ ".xpm/>"

-------------------------------------------------------------------- scratchpads

scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "term" "st -n scratch -t scratch -e tmux new -A -s scratch"
        (title =? "scratch")
        (customFloating $ centerIRectOffsetY panHei tw th sw sh)
    , NS "volume" "pavucontrol"
        (className =? "Pavucontrol")
        (customFloating $ centerIRectOffsetY panHei vw vh sw sh)
    ]
    where tw = 100 * chaWid + 2 * (borWid + terPad)
          th =  27 * chaHei + 2 * (borWid + terPad)
          (vw,vh) = (768,648)
          (sw,sh) = scrRes

-------------------------------------------------------------------------- hooks

myStartupHook =
    startupHook def  <+>
    docksStartupHook <+>
    setSupportedWithFullscreen

myLogHook xmobarH = do
    dynamicLogWithPP $ customPP { ppOutput = ppOutputF xmobarH }
    updatePointer (0.5, 0.5) (0, 0.25)
    where
        toSpaces = (flip replicate) ' ' . length
        centerField [wkspc] = wkspc
        centerField [wkspc,mnum] = printf "%s %s %s" (toSpaces mnum) wkspc mnum
        ppOutputF handle = hPutStrLn handle . centerField . words

myHandleEventHook =
    fullscreenEventHook <+>
    handleEventHook def <+>
    docksEventHook

myManageHook = composeAll
    [ namedScratchpadManageHook scratchpads
    , manageDocks
    , manageHook def
    , composeOne' maybeHooks
    ] where
        centerHook = doCenterFloatOffset scrRes borWid panHei
        maybeHooks = manageByClassName centerHook floatClasses
            ++ manageByProp32Exists centerHook floatProp32s
            ++ manageByPropValue centerHook floatPropValues
            ++ [ isTiled <&&> isNotFixedSize -?> insertPosition Master Newer ]
        floatClasses =
            [ "mpv"
            , "feh"
            , "sun-awt-X11-XFramePeer"
            , "Bluetooth-sendto"
            , "DDLC" ]
        floatPropValues =
            [ ("WM_WINDOW_ROLE", "GtkFileChooserDialog")
            , ("WM_WINDOW_ROLE", "gimp-message-dialog")
            , ("WM_WINDOW_ROLE", "gimp-toolbox-color-dialog")
            , ("WM_WINDOW_ROLE", "file-png")
            , ("WM_NAME", "Friends")
            , ("WM_NAME", "Steam - News")
            , ("WM_NAME", "Discord Updater") ]
        floatProp32s =
            [ "STEAM_GAME" ]

--------------------------------------------------------- firefox fullscreen fix

setSupportedWithFullscreen :: X ()
setSupportedWithFullscreen = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom
        ["_NET_WM_STATE_HIDDEN"
        ,"_NET_WM_STATE_FULLSCREEN"
        ,"_NET_NUMBER_OF_DESKTOPS"
        ,"_NET_CLIENT_LIST"
        ,"_NET_CLIENT_LIST_STACKING"
        ,"_NET_CURRENT_DESKTOP"
        ,"_NET_DESKTOP_NAMES"
        ,"_NET_ACTIVE_WINDOW"
        ,"_NET_WM_DESKTOP"
        ,"_NET_WM_STRUT"
        ]
    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)

------------------------------------------------------------------------ layouts

hqLayout = boringWindows
    . minimize
    . addGaps
    . spacing (fi winGap)
    $ HQLayout 2 mratio tratio (7 % scrWid)
    where
        mratio = 1 - rcratio
        rcratio = (84 * chaWid + 2 * allgaps) % (scrWid - 2 * horPad)
        allgaps = winGap + borWid + terPad
        tratio = (23 * chaHei + 2 * allgaps) % (scrHei - 2 * verPad - panHei)

fullLayout = noBorders
    . boringWindows
    . gaps [(U, fi panHei)]
    $ Full

addGaps = gaps [ (U, fi $ verPad + panHei)
               , (D, fi verPad)
               , (R, fi horPad)
               , (L, fi horPad)
               ]

myLayoutHook = lessBorders OnlyFloat $ hqLayout ||| fullLayout

---------------------------------------------------- keyboard and mouse bindings

myKeyBindings :: [((KeyMask, KeySym), X ())]
myKeyBindings =
    -- minimizing/restoring/swapping windows
    [ ((modm, xK_m),                  withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_m),    withLastMinimized maximizeWindowAndFocus)
    , ((modm .|. mod1Mask, xK_m),     withFocused swapWithFirstMinimized)
    , ((modm .|. mod1Mask, xK_Left),  withFocused swapWithLastMinimized)
    , ((modm .|. mod1Mask, xK_Right), withFocused swapWithFirstMinimized)

    -- cycling through windows
    , ((modm, xK_j), focusDown)
    , ((modm, xK_k), focusUp)

    -- workspaces
    , ((modm, xK_Right), moveTo Next (WSIs notSP))
    , ((modm, xK_Left) , moveTo Prev (WSIs notSP))

    -- killing window copy
    , ((modm .|. shiftMask, xK_c), kill1)

    -- scratchpads
    , ((modm .|. controlMask, xK_Return), namedSA scratchpads "term")
    , ((modm .|. controlMask, xK_v),      namedSA scratchpads "volume")

    -- centering float windows
    , ((modm, xK_c), withFocused centerWindow)
    , ((modm .|. controlMask, xK_c), withFocused centerWindowM)

    -- resizing floating windows
    , ((modm .|. controlMask, xK_Up),    withFocused $
        keysResizeWindow ( 0,  1) (1%2, 1%2))
    , ((modm .|. controlMask, xK_Down),  withFocused $
        keysResizeWindow ( 0, -1) (1%2, 1%2))
    , ((modm .|. controlMask, xK_Right), withFocused $
        keysResizeWindow ( 1,  0) (1%2, 1%2))
    , ((modm .|. controlMask, xK_Left),  withFocused $
        keysResizeWindow (-1,  0) (1%2, 1%2))
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
        notSP         = (return $ ("NSP" /=) . W.tag)
        centerWindow  = keysMoveWindowTo (958, 549) (1/2, 1/2)
        centerWindowM = keysMoveWindowTo (650, 549) (1/2, 1/2)
        namedSA       = namedScratchpadAction

        notify :: MonadIO m => String -> String -> m ()
        notify t s = spawn (printf "notify-send \"%s\" \"%s\"" t s)

        copy' :: String -> X ()
        copy' ws = do
            windows (copy ws);
            (Just windowTitle) <- withWindowSet $ traverse (fmap show . getName) .
                W.peek
            notify "copied window" $ "title: " ++ windowTitle
                                     ++ "\nworkspace: " ++ ws

myMouseBindings =
    -- allows floating window resizing with mod+right click
    [ ((modm, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
    ]

myRemovedBindings =
    [ (modm, xK_p)               -- default: dmenu_run
    , (modm .|. shiftMask, xK_r) -- default: does this do anything?
    ]

-- vim: set ts=4 sw=4 expandtab :
