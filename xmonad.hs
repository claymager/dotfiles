{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Exit
import Data.List
import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Actions.NoBorders
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Prompt.Pass
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedScratchpad
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "kitty"


------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
{-myWorkspaces = map show [1..9]-}


------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
-- 
-- title =? WM_NAME
-- className =? WM_CLASS
-- resource =? WM_CLASS
--
myManageHook = composeOne
  [ isFullscreen -?> (doF W.focusDown <+> doFullFloat)
  , className =? "Gimp"           -?> doFloat
  , title =? "Plover"             -?> doFloat
  , isDialog                      -?> doFloat
  , return True -?> doF W.swapDown
  ] <+> namedScratchpadManageHook myScratchpads

myHandleEventHook = composeAll
    [ dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> floatSpotify)
    , dynamicPropertyChange "WM_NAME" (findGmail --> floatRight)
    , dynamicPropertyChange "WM_NAME" (findGcal --> floatRight)
    ]
  where
    hidden_border = -0.002
    floatSpotify = customFloating $ W.RationalRect x y w h
      where
        h = 0.3
        w = 0.2
        y = (1-h)/2
        x = (1-h)/2
    floatRight = customFloating $ W.RationalRect x y w h
      where
        x = (1-w) - hidden_border
        y = hidden_border
        w = 0.3
        h = 1.01
    findGcal = resource =? "google-chrome" <&&> (take 2 <$> words <$> title) =? ["Google","Calendar"]
    findGmail = resource =? "google-chrome" <&&> (elem "Gmail" <$> words <$> title)

myScratchpads = [ NS "spotify"  spawnSpotify  findSpotify doFloat
                , NS "terminal" spawnTerm findTerm manageTerm
                , NS "gcal" spawnGcal findGcal doFloat
                , NS "gmail" spawnGmail findGmail doFloat
                , NS "notes" spawnNotes findNotes manageNotes
                ]
  where
    hidden_border = -0.002
    spawnSpotify = "spotify"
    findSpotify = className =? "Spotify"

    spawnNotes = myTerminal ++ " --name notes -e nvim ~/.messages"
    findNotes  = resource =? "notes"
    manageNotes = customFloating $ W.RationalRect x y w h
      where
        h = 1.01
        w = 0.2
        y = hidden_border
        x = hidden_border
    spawnTerm = myTerminal ++ " --name scratchpad"
    findTerm  = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect x y w h
      where
        h = 0.1               -- terminal height, 10%
        w = 0.3               -- terminal width
        y = hidden_border
        x = (1-w)/2           -- distance from left
    spawnGcal = "google-chrome-stable --new-window --kiosk calendar.google.com"
    findGcal = resource =? "google-chrome" <&&> (take 2 <$> words <$> title) =? ["Google","Calendar"]
    manageGcal = customFloating rightThird

    spawnGmail = "google-chrome-stable --new-window --kiosk mail.google.com"
    findGmail = resource =? "google-chrome" <&&> (elem "Gmail" <$> words <$> title)
    manageGmail = customFloating rightThird

    rightThird = W.RationalRect x y w h
      where
        h = 1.01
        w = 0.4
        y = hidden_border
        x = (1-w) - hidden_border


------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-esc' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout =
  spacingRaw False (Border 10 10 10 440) True (Border 10 10 10 10) True $
  ThreeColMid  1 (3/100) (5/12)  |||
  Mirror (Tall 4 (3/100) (2/3)) |||
  spiral (6/7) |||
  Full


------------------------------------------------------------------------
-- Colors and borders
--
--
{- color scheme theme -}
foreground = "#f8f8f2"
background = "#282a36"
color0 = "#000000"
color1 = "#ff5555"
color2 = "#50fa7b"
color4 = "#bd93f9"
color5 = "#ff79c6"
color6 = "#8be9fd"
color7 = "#bfbfbf"
color8 = "#4d4d4d"
color9 = "#ff6e67"
color10 = "#5af78e"
color11 = "#f4f99d"
color12 = "#caa9fa"
color13 = "#ff92d0"
color14 = "#9aedfe"
color15 = "#e6e6e6"

myNormalBorderColor  = color8
myFocusedBorderColor = color5
myBorderWidth = 3

-- for conky
cFocus = tail color13
cActive = tail color4


-----------------------------------------------------------------------
-- XPConfig
--
-- This configures the prompt used with `pass` and other extensions.
--
xpconfig = def {
  bgColor = background,
  font = "xft:Hasklig-16",
  position = CenteredAt 0.4 0.5,
  height = 40
}


------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask
altGrMask = mod3Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask, xK_t),                 spawn $ XMonad.terminal conf)
  , ((modMask, xK_c),                 namedScratchpadAction myScratchpads "gcal")
  , ((modMask .|. shiftMask, xK_c),   namedScratchpadAction myScratchpads "gmail")
  , ((modMask, xK_f),                 spawn "qutebrowser")
  , ((modMask, xK_s),                 namedScratchpadAction myScratchpads "terminal")
  {-, ((modMask, xK_d),                 namedScratchpadAction myScratchpads "discord")-}
  , ((modMask, xK_z),                 namedScratchpadAction myScratchpads "spotify")
  , ((modMask, xK_a),                 namedScratchpadAction myScratchpads "notes")

  -- Password-store interface
  , ((modMask, xK_p),                 passPrompt xpconfig)
  , ((modMask .|. controlMask, xK_p), passGeneratePrompt xpconfig)

  -- Screenshots
  , ((modMask,               xK_x),   spawn "scrot -ue 'mv $f ~/pictures/screenshots/'")
  , ((modMask .|. shiftMask, xK_x),   spawn "scrot -e 'mv $f ~/pictures/screenshots/'")

  -- Audio
  , ((modMask, xF86XK_AudioMute),     spawn "pavucontrol")
  , ((0, xF86XK_AudioMute),           spawn "amixer -q set Master toggle")
  , ((0, xF86XK_AudioLowerVolume),    spawn "amixer -q set Master 10%-")
  , ((0, xF86XK_AudioRaiseVolume),    spawn "amixer -q set Master 10%+")
  , ((0, 0x1008FF16),                 spawn "playerctl previous")
  , ((0, 0x1008FF14),                 spawn "playerctl play-pause")
  , ((0, 0x1008FF17),                 spawn "playerctl next")

  -- Backlight
  , ((0, xF86XK_MonBrightnessUp),     spawn "light -A 12")
  , ((0, xF86XK_MonBrightnessDown),   spawn "light -U 12")
  , ((modMask, xF86XK_Calculator),    spawn "light -A 2")
  , ((modMask, xF86XK_Search),        spawn "light -U 2")
  , ((0, xF86XK_Calculator),          spawn "light -A 12")
  , ((0, xF86XK_Search),              spawn "light -U 12")

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close focused window.
  , ((modMask .|. controlMask, xK_c), kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_l),                 sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask, xK_Escape),            setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_r), refresh)

  -- Move focus
  , ((modMask, xK_h),                 windows W.focusMaster  )
  , ((modMask, xK_n),                 windows W.focusDown)
  , ((modMask, xK_e),                 windows W.focusUp  )

  -- Swap the focused window
  , ((modMask .|. shiftMask, xK_h),   windows W.swapMaster)
  , ((modMask .|. shiftMask, xK_n),   windows W.swapDown)
  , ((modMask .|. shiftMask, xK_e),   windows W.swapUp)

  -- Resize the master area.
  , ((modMask, xK_o),                 sendMessage Shrink)
  , ((modMask, xK_i),                 sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_m),                 withFocused $ windows . W.sink)

  -- Adjust the number of windows in the master area.
  , ((modMask, xK_comma),             sendMessage (IncMasterN 1))
  , ((modMask, xK_period),            sendMessage (IncMasterN (-1)))

  -- Adjust spacing between windows.
  , ((modMask, xK_k),                 incScreenSpacing 5 >> incWindowSpacing 5)
  , ((modMask, xK_j),                 decScreenSpacing 5 >> decWindowSpacing 5)
  , ((modMask .|. shiftMask, xK_g),   withFocused toggleBorder)
  , ((modMask, xK_g),                 toggleWindowSpacingEnabled >>
                                      toggleScreenSpacingEnabled)

  -- Quit xmonad.
  , ((modMask, xK_q),                 restart "xmonad" True)
  , ((modMask .|. shiftMask, xK_q),   io (exitWith ExitSuccess))
  , ((modMask .|. shiftMask .|. controlMask, xK_c),   spawn "systemctl hibernate")
  ] ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++

  -- mod-{;,y,u}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{;,y,u}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_y, xK_semicolon, xK_u] [0..]
    , (f, m)    <- [(W.view, 0), (W.shift, shiftMask)]
  ]



------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
-- pointerFollowsFocus is in LogHook
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
  -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

  -- mod-button2, Raise the window tt the top of the stack
  , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

  -- mod-button3, Set the window to floating mode and resize by dragging
  , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

  -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
--

layoutSymbols = [sThreeCol, sTall, sSpiral, sFull]
  where
    sThreeCol = cUnicode "f58d"
    sTall = cUnicode "f0db"
    sSpiral = cUnicode "f2f9"
    sFull = cUnicode "f24d"

layoutPosition x = case x of
  "ThreeCol" -> 0
  "Tall"     -> 1
  "Spiral"   -> 2
  "Full"     -> 3
  x          -> -1

wsFont   = cFont "Ubuntu Mono" 60
wsPrefix = "${voffset -25}${goto 98}"
loFont   = cFont "Font Awesome 5 Free Solid" 25
loPrefix = "${voffset -186}"
loSymbolPrefix = "${goto 327}${voffset 5}"
titlePrefix    = "${alignc}${voffset -48}${font}"

myLogHook = dynamicLogWithPP conkyPP >> updatePointer (0.25,0.25) (0.25,0.25)

conkyPP :: PP
conkyPP = def {
  ppOutput = writeFile "/home/john/tmp/xmonadStatus" . wsFont . (("${color}")++)
  , ppCurrent = processWorkspace (-8) cFocus  (cUnicode "25c9")
  , ppVisible = processWorkspace (-8) cActive (cUnicode "25c9")
  , ppHidden  = processWorkspace (-8) cActive (cUnicode "25cb")
  , ppHiddenNoWindows = processWorkspace 0 "" (cUnicode "2022")
  , ppOrder = myLogOrder
  , ppSep = "\n"
  , ppWsSep = "\n"
  , ppTitle = (titlePrefix++) . const "" --shorten 40
  , ppTitleSanitize = id
  , ppLayout = (loPrefix++) . loFont . layoutMap
  }


processWorkspace :: Int -> String -> String -> String -> String
processWorkspace vOffset color symbol name =
  if name == "NSP" then "" -- filters scratchpad workspace
  else cVOffset vOffset $ colorWrap color symbol

myLogOrder :: [String] -> [String]
myLogOrder (ws:rest) = (splitToRows ws) ++ rest
  where
    splitFunctions = [
      take 3,
      take 3 . drop 3,
      drop 6 ]
    splitToRows ws = (wsPrefix++)<$> (intercalate " ") <$> ($lines ws) <$> splitFunctions

layoutMap :: String -> String
layoutMap = unlines . map (loSymbolPrefix ++) . highlightSymbol . last . words
  where
  highlightSymbol :: String -> [String]
  highlightSymbol x = zipWith ($) (highlightPosition $ layoutPosition x) layoutSymbols

  highlightPosition i = foo i (colorWrap cFocus)
  foo :: Int -> (a -> a) -> [a -> a]
  foo n f
    | n <  0 = repeat id
    | n == 0 = f : repeat id
    | otherwise = id : foo (n-1) f

colorWrap :: String -> String -> String
colorWrap color base = "${color " ++ color ++ "}" ++ base ++ "${color}"

cFont :: String -> Int -> String -> String
cFont f s base = "${font " ++ f ++ ":pixelsize=" ++ (show s) ++ "}" ++ base

cUnicode :: String -> String
cUnicode code = "${exec printf \\\\u" ++ code ++ "}"

cVOffset :: Int -> String -> String
cVOffset n base = "${voffset "++ show n ++ "}" ++ base ++ "${voffset "++ show (-n) ++ "}"

debugLogHook = dynamicLogWithPP $ def {
  ppOutput = writeFile "/home/john/tmp/xmonadStatus"
  }

------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
myInitCommands =
  [ "feh --bg-fill .wallpaper"
  , "pkill conky; conky"
  ]

myStartupHook = do
  mapM_ spawn myInitCommands


------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main = do
  xmonad mySettings


------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
mySettings = def {
  -- simple stuff
  terminal           = myTerminal,
  focusFollowsMouse  = myFocusFollowsMouse,
  borderWidth        = myBorderWidth,
  modMask            = myModMask,
  {-workspaces         = myWorkspaces,-}
  normalBorderColor  = myNormalBorderColor,
  focusedBorderColor = myFocusedBorderColor,

  -- key bindings
  keys               = myKeys,
  mouseBindings      = myMouseBindings,

  -- hooks, layouts
  layoutHook         = myLayout,
  manageHook         = myManageHook,
  handleEventHook    = myHandleEventHook,
  startupHook        = myStartupHook,
  logHook            = myLogHook
}

