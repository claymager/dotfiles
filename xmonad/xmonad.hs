{-# LANGUAGE OverloadedStrings #-}

import Graphics.X11.ExtraTypes.XF86
import System.Exit 
import XMonad
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicLog (ppOutput, dynamicLogWithPP)
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Util.NamedScratchpad
import qualified Data.Map        as M
import qualified XMonad.StackSet as W

import MyConkyPP (conkyPP)
import MyKeys (myModMask, myKeys)
import MySettings
import MyWindowHooks

------------------------------------------------------------------------
-- Layouts
--
myBorderWidth = 3
myLayout =
  spacingRaw False (Border 10 10 10 440) True (Border 10 10 10 10) True $
  ThreeColMid  1 (3/100) (5/12)  |||
  Mirror (Tall 4 (3/100) (2/3)) |||
  spiral (6/7) |||
  Full


------------------------------------------------------------------------
-- Mouse bindings
--
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
  , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
  , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
  -- may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Hooks
--
myInitCommands =
  [ "feh --bg-fill .wallpaper"
  , "pkill conky; conky"
  ]

myStartupHook = do
  mapM_ spawn myInitCommands

myLogHook = dynamicLogWithPP conkyPP >> updatePointer (0.25,0.25) (0.25,0.25)

debugLogHook = dynamicLogWithPP $ def {
  ppOutput = writeFile "/home/john/tmp/xmonadStatus"
  }

------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main = do
  xmonad mySettings

mySettings = def {
  -- simple stuff
  terminal           = myTerminal,
  focusFollowsMouse  = myFocusFollowsMouse,
  borderWidth        = myBorderWidth,
  modMask            = myModMask,
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

