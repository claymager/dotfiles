{-# LANGUAGE OverloadedStrings #-}
module MySettings
    ( foreground
    , background
    , myNormalBorderColor
    , myFocusedBorderColor
    , conkyFocus
    , conkyActive
    , xpconfig
    , myTerminal
    )
where

import           XMonad.Prompt

myTerminal = "kitty"

xpconfig = def { bgColor  = background
               , font     = "xft:Fantasque Sans Mono-16"
               , position = CenteredAt 0.4 0.5
               , height   = 40
               }

myNormalBorderColor = color8
myFocusedBorderColor = color5

conkyFocus = tail color13
conkyActive = tail color4

{- color scheme, automatically generated-}
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
