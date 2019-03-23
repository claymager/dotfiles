{-# LANGUAGE OverloadedStrings #-}
module MyWindowHooks
    ( myManageHook
    , myHandleEventHook
    , runScratchpad
    , dismiss
    )
where

import           XMonad
import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.ManageHelpers
import           XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet               as W
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import Control.Monad (filterM)
import Data.Maybe (fromJust)
import           Data.List  (intercalate)

import           MySettings                     ( myTerminal )

-- Exports

-- | Hides the active window, killing it if not a scratchpad
--
dismiss :: X()
dismiss = withWindowSet $ \s -> do
    if null (filter ((== scratchpadWorkspaceTag) . W.tag) (W.workspaces s))
       then addHiddenWorkspace scratchpadWorkspaceTag
       else return ()
    focused <- return $ fmap W.focus . W.stack . W.workspace . W.current $ s
    activePads <- filterM (beingUsed focused) scratchpads
    case activePads of
      [] -> kill
      _  -> windows . W.shiftWin scratchpadWorkspaceTag $ fromJust focused
    where
      beingUsed :: Maybe Window -> NamedScratchpad -> X Bool
      beingUsed mWin pad = maybe (return False) (runQuery $ query pad) mWin

myManageHook = composeOne
    [ className =? "Gimp"     -?> doFloat
    -- , className =? "Anki" <&&> title =? "Anki" -?> doFloatAt 0 0
    , className =? "Xmessage" -?> floatCenter
    , className =? "small_float" -?> smallFloat
    , className =? "VirtualBox Machine" -?> doIgnore
    , className =? "ws-label" -?> floatLabel
    , className =? "Matplotlib" -?> doFloat
    , isDialog                -?> doFloat
    , return True             -?> doF W.swapDown
    ] <+> namedScratchpadManageHook scratchpads

runScratchpad = namedScratchpadAction scratchpads

floatLabel = customFloating $ W.RationalRect x y w h
  where
    (x, y) = (0.006, 0.014)
    (w, h) = (0.118, 0.10)

type Command = String -> String

asClass :: String -> Command
asClass className cmd = cmd ++ " --class " ++ className

withNix :: String -> Command
withNix nixExpr cmd  = cmd ++ " -e nix-shell -p \"" ++ nixExpr ++ "\" --command "

pyEnv version pkgs = 
  let packages = intercalate " " $ ["black", "python-language-server", "ipython"] ++ pkgs
  in "python" ++ version ++ ".withPackages (ps: with ps; [" ++ packages ++ "])"

runCommand :: String -> Command
runCommand cmd base = base ++ "\"" ++ cmd ++ "\""

editFile :: String -> Command
editFile path = runCommand ("nvim " ++ path)

myHandleEventHook = composeAll
    [ dynamicPropertyChange "WM_CLASS" (findDiscord --> floatDiscord)
    , dynamicPropertyChange "WM_NAME"  (findGcal    --> floatGcal)
    , dynamicPropertyChange "WM_NAME"  (findGmail   --> floatGmail)
    , dynamicPropertyChange "WM_CLASS" (findSpotify --> floatSpotify)
    ]

scratchpads =
    [ NS "discord"  spawnDiscord findDiscord floatDiscord
    , NS "gcal"     spawnGcal    findGcal    doFloat
    , NS "gmail"    spawnGmail   findGmail   doFloat
    , NS "notes"    spawnNotes   findNotes   manageNotes
    , NS "spotify"  spawnSpotify findSpotify doFloat
    , NS "terminal" spawnTerm    findTerm    manageTerm
    , NS "ankiAdd" "" (className =? "Anki" <&&> title =? "Add") doFloat

    -- , NS "plover"   spawnPlover  findPlover  managePlover
    , anki, ipython, ghci, pycalc
    ]

anki =
  NS
   "anki"
   "anki"
   (className =? "Anki" <&&> title =? "User 1 - Anki")
   (doF W.swapDown)
-- Settings
--
-- tag of the scratchpad workspace
scratchpadWorkspaceTag :: String
scratchpadWorkspaceTag = "NSP"

browserKiosk = ("google-chrome-stable --new-window --kiosk " ++)
border = 0.0025
full = 1 + (2 * border)

floatRight = customFloating $ W.RationalRect x y w h
  where
    x = (1-w) + border -- distance from left
    y = (1-h)/2        -- distance from top
    w = 0.6            -- width of window
    h = 0.8

floatCenter = customFloating $ W.RationalRect x y w h
  where
    h = 0.5
    w = 0.3
    y = (1 - h) / 2
    x = (1 - w) / 2

smallFloat = customFloating $ W.RationalRect x y w h
 where
   h = 0.2
   w = 0.15
   y = 0.7
   x = 0.05


floatRepl = customFloating $ W.RationalRect x y w h
  where
    h = 0.3
    w = 0.29
    y = -border
    x = (1 - w) / 2

-- Instance definitions
spawnSpotify = "spotify"
findSpotify = className =? "Spotify"
floatSpotify = floatCenter

spawnDiscord = "Discord &> /dev/null"
findDiscord = className =? "discord"
floatDiscord = customFloating $ W.RationalRect x y w h
  where
    h = 0.8
    w = 0.3
    y = (1 - h) / 2
    x = -border

spawnGcal = browserKiosk "calendar.google.com"
findGcal  = resource =? "google-chrome" <&&> (take 2 . words <$> title) =?  ["Google", "Calendar"]
floatGcal = floatRight

spawnGmail = browserKiosk "mail.google.com"
findGmail  = resource =? "google-chrome" <&&> (elem "Gmail" . words <$> title)
floatGmail = floatRight

spawnNotes  = myTerminal ++ " --name notes -e nvim ~/.messages"
findNotes   = resource =? "notes"
manageNotes = customFloating $ W.RationalRect x y w h
  where
    h = 0.8
    w = 0.2
    y = 0.1
    x = -border

spawnTerm  = myTerminal ++ " --name scratchpad"
findTerm   = resource =? "scratchpad"
manageTerm = customFloating $ W.RationalRect x y w h
  where
    h = 0.15
    w = 0.29
    y = -border
    x = (1 - w) / 2

spawnPlover  = "plover"
findPlover   = title =? "Plover"
managePlover = customFloating $ W.RationalRect x y w h
  where
    h = 0.35
    w = 0.12
    y = (1 - h) + border
    x = 0.006


-- spawnPython  = myTerminal ++ " --class python --override 'background = #321b32' -e ipython"
spawnPython  = runCommand "ipython" . withNix (pyEnv "37" []) $ asClass "python" myTerminal
findPython   = className =? "python"
managePython = floatRepl

-- spawnGhci  = myTerminal ++ " --class ghci --override 'background = #231b32' --override 'cursor = #ffb86c' -e ghci"
-- spawnGhci  = runCommand "ghci" . withNix "ghc" $ asClass "ghci" myTerminal
-- findGhci   = className =? "ghci"
-- manageGhci = floatRepl

mkScratchpad name cmd nix manager =
  let _class = name ++ "_scratchpad"
  in
    NS
      name
      (runCommand cmd . withNix nix $ asClass _class myTerminal)
      (className =? _class)
      manager

simplePad name nix manager = mkScratchpad name name nix manager

ipython = simplePad "ipython" (pyEnv "37" ["numpy"]) floatRepl
ghci    = simplePad "ghci" "ghc" floatRepl
pycalc  = mkScratchpad "calc" "nvim ~/tmp/_calc.py" (pyEnv "37" ["numpy", "matplotlib"]) manageNotes
-- spawnCalc = (runCommand "nvim ~/tmp/_calc.py" .
              -- withNix (pyEnv "37" ["numpy"]) $
              -- asClass "calc_vim" myTerminal)
-- findCalc = className =? "calc_vim"
