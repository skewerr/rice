import XMonad
import XMonad.Config
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.FloatKeys
import XMonad.Actions.Minimize
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.XUtils (fi)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run
import XMonad.Util.CenterRationalRect
import XMonad.Layout.SimplestFloat
import XMonad.Layout.NoBorders
import XMonad.Layout.BoringWindows
import XMonad.Layout.Minimize
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps

-- local stuff
import XMonad.Hooks.ShittyManageHooks
import XMonad.Layout.SResizableTile
import XMonad.Util.CenterRationalRect

import Control.Monad
import Text.Printf (printf)
import System.IO (Handle, openFile, IOMode(..))
import Data.Monoid
import Data.Ratio
import Data.Maybe
import Data.List
import Data.List.Split

import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map as M

------------------------------------------------------------------ main function

main = do
    xmproc <- spawnPipe "xmobar"
    wnpipe <- openFile "/dev/null" WriteMode --spawnPipe "wtfifo"

    xmonad . ewmh $ def
        { terminal           = "urxvt -e tmux"
        , focusFollowsMouse  = True

        , focusedBorderColor = "#b44c21"
        , normalBorderColor  = "#656565"
        , borderWidth        = fi borWid

        , modMask            = modm
        , workspaces         = myWorkspaces

        , startupHook        = myStartupHook
        , logHook            = myLogHook xmproc wnpipe
        , manageHook         = myManageHook
        , handleEventHook    = myHandleEventHook
        , layoutHook         = myLayoutHook
        }

        `additionalKeys`             myKeyBindings
        `additionalMouseBindings`    myMouseBindings
        `removeKeys`                 myRemovedBindings

----------------------------------------------------------------- some variables

scrRes@(scrWid, scrHei) = (1920 :: Integer, 1080 :: Integer) -- self explanatory
chaRes@(chaWid, chaHei) = (   7 :: Integer,   14 :: Integer) -- terminal cells
(horPad, verPad)        = (  36 :: Integer,   39 :: Integer) -- gaps

panHei = 22 -- :: Integer -- height of the top panel
borWid =  2 -- :: Integer -- border width for the windows
terPad =  5 -- :: Integer -- terminal padding (internal border)
winGap =  7 -- :: Integer -- window margin, will appear twofold

myWorkspaces = ["月", "火", "水", "木", "金", "土", "日"]
modm = mod4Mask

-------------------------------------------------------------------- prettyprint

emptyPP :: PP
emptyPP = def
    { ppHidden = const ""
    , ppVisible = const ""
    , ppHiddenNoWindows = const ""
    , ppUrgent = const ""
    , ppTitle = const ""
    , ppLayout = const ""
    , ppSep = ""
    , ppWsSep = ""
    , ppCurrent = const ""
    }

customPP :: PP
customPP = emptyPP
    { ppCurrent = id
    , ppTitle = id
    , ppSep = "~"
    , ppOrder = \(ws:_:t:rs) -> t:ws:rs
    , ppExtras = [ fmap surround minimizedNum ]
    }
    where
        surround = fmap $ \s -> if (read s /= 0) then " (" ++ s ++ ")"
                                                 else ""

-------------------------------------------------------------------- scratchpads

scratchpads :: [NamedScratchpad]
scratchpads = [ NS "term" "urxvt -title scratch -e tmux"
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

myStartupHook = startupHook def  <+>
                docksStartupHook <+>
                setSupportedWithFullscreen

myLogHook xmobarH _ = (dynamicLogWithPP $ customPP { ppOutput = ppOutputF })
    >> updatePointer (0.5, 0.5) (0, 0.25)
    where
        ppOutputF rawLogStr = do
            hPutStrLn xmobarH centerField
            --hPutStrLn pipeH windowTitle
            where
                tok = ppSep customPP
                mWsExist = ')' == last rawLogStr
                minimizedTax | mWsExist = init
                             | otherwise = id
                centerField  | mWsExist = paddedWSName
                             | otherwise = wrappedWSName
                splitLogStr = splitOn tok rawLogStr
                workspaceName = last $ minimizedTax splitLogStr
                --windowTitle = intercalate tok . init $ minimizedTax splitLogStr
                wrappedWSName = wrapIcon workspaceName
                paddedWSName | mWsExist = leftPad ++ wrappedWSName ++ rightPad
                             | otherwise = wrappedWSName
                    where
                        rightPad = last splitLogStr
                        leftPad = replicate(length rightPad) ' '
                wrapIcon s = "<icon=.local/share/icons/xpm/" ++ s ++ ".xpm/>"

myHandleEventHook =
    fullscreenEventHook <+>
    handleEventHook def <+>
    docksEventHook

myManageHook = composeAll
    [ namedScratchpadManageHook scratchpads
    , manageDocks
    , insertPosition Master Newer
    , manageHook def
    , manageByClassName centerHook floatClasses
    , manageByProp32Exists centerHook floatProp32s
    , manageByPropValue centerHook floatPropValues
    ] where
        centerHook = doCenterFloatOffset scrRes borWid panHei
        floatClasses =
            [ "mpv"
            , "feh"
            , "sun-awt-X11-XFramePeer"
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
    supp <- mapM getAtom ["_NET_WM_STATE_HIDDEN"
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

tallLayout = minimize
    . spacing (fi winGap)
    $ SRTall 3 delta ratio [botwrat, topwrat]
    where
        nmaster = 1
        delta   = 2 % (scrHei - 2*(verPad) - panHei)
        ratio   = 1 - rightcolratio

        topwrat = 322 / 490 -- TODO: ratio should depend on padding and line #s
        botwrat = 658 / 490 -- TODO: same as above

        rightcolratio = (84*chaWid + 2*allgaps) % (scrWid - 2*horPad)
        allgaps = winGap + borWid + terPad

fullLayout = noBorders . boringWindows . gaps [(U, fi panHei)] $ Full

addGaps = gaps [ (U, fi $ verPad + panHei)
               , (D, fi verPad)
               , (R, fi horPad)
               , (L, fi horPad)
               ]

myLayoutHook = lessBorders OnlyFloat . boringWindows $ addGaps tallLayout ||| fullLayout
-- toggleLayouts (addGaps tallLayout ||| addGaps accLayout)
--     fullLayout

---------------------------------------------------- keyboard and mouse bindings

myKeyBindings :: [((KeyMask, KeySym), X ())]
myKeyBindings =
    -- resizableTall messages
    [ ((modm, xK_a), sendMessage SMShrink)
    , ((modm, xK_z), sendMessage SMExpand)
    , ((modm, xK_s), replicateM_ (fi chaHei) $ sendMessage SMShrink)
    , ((modm, xK_x), replicateM_ (fi chaHei) $ sendMessage SMExpand)

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
    , ((modm .|. controlMask, xK_c), withFocused centerWindowM)

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
    --, ((modm .|. controlMask, xK_m),      namedSA scratchpads "music")
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
        notSP         = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
        centerWindow  = keysMoveWindowTo (958, 549) (1/2, 1/2)
        centerWindowM = keysMoveWindowTo (650, 549) (1/2, 1/2)
        namedSA       = namedScratchpadAction

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
