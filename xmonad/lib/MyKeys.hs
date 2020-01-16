{-# LANGUAGE OverloadedStrings #-}
module MyKeys (myModMask, myKeys) where

import Graphics.X11.ExtraTypes.XF86
import System.Exit
import XMonad
import XMonad.Actions.NoBorders
import XMonad.Actions.DeManage
import XMonad.Actions.ShowText
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Util.NamedScratchpad
import qualified Data.Map        as M
import qualified XMonad.StackSet as W

import MyWindowHooks (runScratchpad)
import MySettings (xpconfig, textConfig)
import Pass (passPrompt, passGeneratePrompt)
import Clip (clipPrompt, clipSavePrompt)

notify msg = flashText textConfig 0.1 msg
------------------------------------------------------------------------
-- Key bindings
--
-- mod1Mask => left alt
-- mod3Mask => right alt
-- mod4Mask => super
--
myModMask = mod4Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  -- Launchers
  [ ((modMask, xK_t),                 spawn $ XMonad.terminal conf)
  , ((modMask .|. mod1Mask, xK_t),    spawn "xterm")
  , ((modMask, xK_c),                 runScratchpad "gcal")
  , ((modMask .|. mod1Mask, xK_m),    runScratchpad "gmail")
  , ((modMask, xK_f),                 spawn "qutebrowser")
  , ((modMask, xK_s),                 runScratchpad "terminal")
  , ((modMask, xK_d),                 runScratchpad "discord")
  , ((modMask .|. controlMask, xK_p), runScratchpad "python")
  , ((modMask .|. controlMask, xK_h), runScratchpad "ghci")
  , ((modMask .|. controlMask, xK_s), runScratchpad "plover")
  , ((modMask, xK_z),                 runScratchpad "spotify")
  , ((modMask, xK_a),                 runScratchpad "notes")
  , ((modMask .|. mod1Mask, xK_n),    spawn "google-chrome-stable --kiosk --new-window netflix.com")
  , ((modMask .|. mod1Mask, xK_c),    spawn "google-chrome-stable --new-window creddle.io")

  -- Password-store interface
 , ((modMask, xK_p),                  passPrompt xpconfig)
  , ((modMask .|. shiftMask, xK_p),   passGeneratePrompt xpconfig)
  , ((modMask .|. shiftMask .|. controlMask, xK_apostrophe),   clipSavePrompt xpconfig)
  , ((modMask .|. shiftMask, xK_apostrophe),   clipPrompt xpconfig)

  -- Screenshots
  , ((modMask,               xK_x),   spawn "scrot -ue 'mv $f ~/pictures/screenshots/'")
  , ((modMask .|. shiftMask, xK_x),   spawn "scrot -e 'mv $f ~/pictures/screenshots/'")

  -- Audio
  , ((modMask, xF86XK_AudioMute),     spawn "pavucontrol")
  , ((0, xF86XK_AudioMute),           spawn "amixer -q set Master toggle")
  , ((0, xF86XK_AudioLowerVolume),    spawn "amixer -q set Master 8%-")
  , ((0, xF86XK_AudioRaiseVolume),    spawn "amixer -q set Master 8%+")
  , ((0, 0x1008FF16),                 spawn "playerctl previous")
  , ((0, 0x1008FF14),                 spawn "playerctl play-pause")
  , ((0, 0x1008FF17),                 spawn "playerctl next")

  -- Backlight
  , ((0, xF86XK_MonBrightnessUp),         spawn "light -A 12")
  , ((0, xF86XK_MonBrightnessDown),       spawn "light -U 12")
  , ((modMask, xF86XK_MonBrightnessDown), spawn "light 1")

  -- Notifications
  , ((modMask .|. shiftMask .|. controlMask, xK_q), notify "colemak")
  , ((modMask .|. shiftMask .|. controlMask, xK_s), notify "steno")
  , ((modMask .|. shiftMask .|. controlMask, xK_v), notify "neovim")


  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --
  -- Layout management
  , ((modMask, xK_l),                 sendMessage NextLayout)
  , ((modMask, xK_comma),             sendMessage (IncMasterN 1))
  , ((modMask, xK_period),            sendMessage (IncMasterN (-1)))
  , ((modMask, xK_o),                 sendMessage Shrink)
  , ((modMask, xK_i),                 sendMessage Expand)

  -- Spacing
  , ((modMask, xK_k),                 incScreenSpacing 5 >> incWindowSpacing 5)
  , ((modMask, xK_j),                 decScreenSpacing 5 >> decWindowSpacing 5)
  , ((modMask .|. shiftMask, xK_g),   withFocused toggleBorder)
  , ((modMask, xK_g),                 toggleWindowSpacingEnabled >>
                                      toggleScreenSpacingEnabled)

  -- Resize viewed windows to the correct size.
  -- , ((modMask, xK_r),                 refresh)
  , ((modMask, xK_Escape),            setLayout $ XMonad.layoutHook conf)

  -- Window management
  , ((modMask .|. controlMask, xK_c), kill)
  , ((modMask, xK_h),                 windows W.focusMaster  )
  , ((modMask, xK_n),                 windows W.focusDown)
  , ((modMask, xK_e),                 windows W.focusUp  )
  , ((modMask .|. shiftMask, xK_h),   windows W.swapMaster)
  , ((modMask .|. shiftMask, xK_n),   windows W.swapDown)
  , ((modMask .|. shiftMask, xK_e),   windows W.swapUp)
  , ((modMask, xK_m),                 withFocused $ windows . W.sink)

  -- Quit xmonad.
  , ((modMask, xK_q),                                restart "xmonad" True)
  , ((modMask .|. shiftMask, xK_q),                  io exitSuccess)
  -- , ((modMask .|. shiftMask .|. controlMask, xK_q),  spawn "systemctl hibernate")
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



