{-# LANGUAGE OverloadedStrings #-}
module MyWindowHooks
(myManageHook, myHandleEventHook, runScratchpad) where

import XMonad
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.ManageHelpers
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import MySettings (myTerminal)

-- Exports
myManageHook = composeOne
  [ isFullscreen            -?> (doF W.focusDown <+> doFullFloat)
  , className =? "Gimp"     -?> doFloat
  , className =? "Xmessage" -?> floatCenter
  , isDialog                -?> doFloat
  , return True             -?> doF W.swapDown
  ] <+> namedScratchpadManageHook myScratchpads

runScratchpad = namedScratchpadAction myScratchpads

myHandleEventHook = composeAll
    [ dynamicPropertyChange "WM_CLASS" (findDiscord --> floatDiscord)
    , dynamicPropertyChange "WM_NAME"  (findGcal    --> floatGcal)
    , dynamicPropertyChange "WM_NAME"  (findGmail   --> floatGmail)
    , dynamicPropertyChange "WM_CLASS" (findSpotify --> floatSpotify)
    ]

myScratchpads = 
  [ NS "discord"  spawnDiscord findDiscord floatDiscord
  , NS "gcal"     spawnGcal    findGcal    doFloat
  , NS "gmail"    spawnGmail   findGmail   doFloat
  , NS "notes"    spawnNotes   findNotes   manageNotes
  , NS "spotify"  spawnSpotify findSpotify doFloat
  , NS "terminal" spawnTerm    findTerm    manageTerm
  , NS "plover"   spawnPlover  findPlover  managePlover
  , NS "python"   spawnPython  findPython  managePython
  , NS "ghci"     spawnGhci    findGhci    manageGhci
  ]


-- Settings
browserKiosk = ("google-chrome-stable --new-window --kiosk " ++)
border = 0.0025
full = 1 + (2 * border)
floatRight = customFloating $ W.RationalRect x y w h
  where
    x = (1-w) + border -- distance from left
    y = (1-h)/2        -- distance from top
    w = 0.3            -- width of window
    h = 0.66           -- height of window

floatCenter =  customFloating $ W.RationalRect x y w h
  where
    h = 0.5
    w = 0.3
    y = (1-h)/2
    x = (1-w)/2

-- Instance definitions
spawnSpotify = "spotify"
findSpotify  = className =? "Spotify"
floatSpotify = floatCenter
  where
    h = 0.5
    w = 0.3
    y = (1-h)/2
    x = (1-w)/2

spawnDiscord = "Discord"
findDiscord  = className =? "discord"
floatDiscord = customFloating $ W.RationalRect x y w h
  where
    h = 0.5
    w = 0.3
    y = (1-h)/2
    x = -border

spawnGcal = browserKiosk "calendar.google.com"
findGcal  = resource =? "google-chrome" <&&> (take 2 <$> words <$> title) =? ["Google","Calendar"]
floatGcal = floatRight

spawnGmail = browserKiosk "mail.google.com"
findGmail  = resource =? "google-chrome" <&&> (elem "Gmail" <$> words <$> title)
floatGmail = floatRight

spawnNotes  = myTerminal ++ " --name notes -e nvim ~/.messages"
findNotes   = resource =? "notes"
manageNotes = customFloating $ W.RationalRect x y w h
  where
    h = full
    w = 0.2
    y = -border
    x = -border

spawnTerm  = myTerminal ++ " --name scratchpad"
findTerm   = resource =? "scratchpad"
manageTerm = customFloating $ W.RationalRect x y w h
  where
    h = 0.15
    w = 0.29
    y = -border
    x = (1-w)/2

spawnPlover  = "plover"
findPlover   = title =? "Plover"
managePlover = customFloating $ W.RationalRect x y w h
  where
    h = 0.35
    w = 0.12
    y = (1-h) + border
    x = 0.006

spawnPython  = myTerminal ++ " --class python --override 'background = #321b32' -e nix-shell -p python37Packages.ipython --command ipython"
findPython   = className =? "python"
managePython = customFloating $ W.RationalRect x y w h
  where
    h = 0.3
    w = 0.29
    y = -border
    x = (1-w)/2

spawnGhci  = myTerminal ++ " --class ghci --override 'background = #231b32' --override 'cursor = #ffb86c' -e nix-shell -p ghc --command ghci"
findGhci   = className =? "ghci"
manageGhci = customFloating $ W.RationalRect x y w h
  where
    h = 0.3
    w = 0.29
    y = -border
    x = (1-w)/2
