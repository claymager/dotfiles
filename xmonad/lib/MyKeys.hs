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
-- mod3Mask => f24
-- mod4Mask => super
--
myModMask = mod4Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) =
  let
    withMask m = \k -> (m, k)
    hyperMask  = controlMask .|. shiftMask .|. altMask
    mehMask    = hyperMask .|. mod4Mask
    altMask    = mod1Mask
    noMask     = withMask 0
    super      = withMask modMask
    withShift  = withMask $ modMask .|. shiftMask
    withCtl    = withMask $ modMask .|. controlMask
    scratchpad = withLAlt
    withLAlt   = withMask $ modMask .|. mod1Mask
    hyper      = withMask $ hyperMask
    alt        = withLAlt
    notifyMask = withMask $ modMask .|. shiftMask .|. controlMask .|. mod3Mask
  in M.fromList $

  -- Launchers
  [ (super xK_t,                 spawn $ XMonad.terminal conf)
  , (withShift xK_t,             spawn "kitty -d=$focusdir")
  , (alt xK_f,                   catchIO $ setEnv "focusdir" "/home/john/lab/haskell/proj/")
  , (alt xK_t,                   spawn "env > /home/john/tmp/env.txt")
  , (super xK_f,                 spawn "qutebrowser -l warning")
  , (scratchpad xK_c,            runScratchpad "gcal")
  , (scratchpad xK_m,            runScratchpad "gmail")
  , (scratchpad xK_s,            runScratchpad "terminal")
  , (scratchpad xK_d,            runScratchpad "discord")
  , (scratchpad xK_p,            runScratchpad "ipython")
  , (scratchpad xK_h,            runScratchpad "ghci")
  -- , (scratchpad xK_s,            runScratchpad "plover")
  , (scratchpad xK_z,            runScratchpad "spotify")
  -- , (scratchpad xK_a,            runScratchpad "notes")
  , (scratchpad xK_a,            runScratchpad "anki")
  , (withCtl xK_n,               spawn "google-chrome-stable --kiosk --new-window netflix.com")
  , (withMask mod3Mask xK_g,     spawn "pavucontrol")
  , (scratchpad xK_l,            runScratchpad "calc")
  -- , (withLAlt xK_c,              spawn "google-chrome-stable --new-window creddle.io")

  -- Password-store interface
  , (super xK_p,                 passPrompt xpconfig)
  , (withShift xK_p,             passGeneratePrompt xpconfig)
  , ((modMask .|. shiftMask .|. controlMask, xK_apostrophe),   clipSavePrompt xpconfig)
  , (super xK_eth,          clipPrompt xpconfig)

  -- Screenshots
  , (super xK_x,                 spawn "scrot -ue 'mv $f ~/pictures/screenshots/'")
  , (withShift xK_x,             spawn "scrot -e 'mv $f ~/pictures/screenshots/'")

  -- Audio
  , (super xF86XK_AudioMute,           spawn "pavucontrol")
  , (noMask xF86XK_AudioMute,          spawn "amixer -q set Master toggle")
  , (noMask xF86XK_AudioLowerVolume,   spawn "amixer -q set Master 8%-")
  , (noMask xF86XK_AudioRaiseVolume,   spawn "amixer -q set Master 8%+")
  , (noMask xF86XK_AudioPrev,          spawn "playerctl previous")
  , (noMask xF86XK_AudioPlay,          spawn "playerctl play-pause")
  , (noMask xF86XK_AudioNext,          spawn "playerctl next")

  -- -- Backlight
  -- , (noMask xF86XK_MonBrightnessUp,    spawn "light -A 12")
  -- , (noMask xF86XK_MonBrightnessDown,  spawn "light -U 12")
  -- , (super xF86XK_MonBrightnessDown,   spawn "light 1")

  -- -- Notifications
  -- , (notifyMask xK_q,                  notify "colemak")
  -- , (notifyMask xK_s,                  notify "steno")
  -- , (notifyMask xK_v,                  notify "neovim")

  , (super xK_F2,                      spawn "kitty nvim $HOME/github/config/xmonad/xmonad.hs")

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --
  -- Layout management
  , (super xK_l,                 sendMessage NextLayout)
  , (super xK_comma,             sendMessage (IncMasterN 1))
  , (super xK_period,            sendMessage (IncMasterN (-1)))
  , (super xK_o,                 sendMessage Shrink)
  , (super xK_i,                 sendMessage Expand)

  -- Spacing
  , (super xK_k,                 incScreenSpacing 5 >> incWindowSpacing 5)
  , (super xK_j,                 decScreenSpacing 5 >> decWindowSpacing 5)
  , (withShift xK_g,             withFocused toggleBorder)
  , (super xK_g,                 toggleWindowSpacingEnabled >>
                                      toggleScreenSpacingEnabled)

  -- Resize viewed windows to the correct size.
  -- , ((modMask, xK_r),                 refresh)
  , (super xK_Escape,            setLayout $ XMonad.layoutHook conf)

  -- Window management
  , ((modMask .|. controlMask, xK_c), kill)
  , (super xK_h,                 windows W.focusMaster  )
  , (super xK_n,                 windows W.focusDown)
  , (super xK_e,                 windows W.focusUp  )
  , (withShift xK_h,             windows W.swapMaster)
  , (withShift xK_n,             windows W.swapDown)
  , (withShift xK_e,             windows W.swapUp)
  , (super xK_m,                 withFocused $ windows . W.sink)

  -- Quit xmonad.
  , (super     xK_q,             restart "xmonad" True)
  , (withShift xK_q,             io exitSuccess)
  , (withCtl   xK_q,             spawn "systemctl hibernate")
  , (withCtl   xK_t,             spawn "nixos-rebuild test")
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
    | (key, sc) <- zip [xK_y, xK_u, xK_semicolon] [0..]
    , (f, m)    <- [(W.view, 0), (W.shift, shiftMask)]
  ]



