{-# LANGUAGE OverloadedStrings #-}
module MyConkyPP
    ( conkyPP
    )
where

import           XMonad
import           XMonad.Hooks.DynamicLog
import           MySettings                     ( conkyFocus , conkyActive)

conkyPP :: PP
conkyPP = def
    { ppOutput          = writeFile "/home/john/tmp/xmonadStatus" . workspaceFont . ("${color}" ++)
    , ppCurrent         = processWorkspace conkyFocus (unicode "25c9")
    , ppVisible         = processWorkspace conkyActive (unicode "25c9")
    , ppHidden          = processWorkspace conkyActive (unicode "25cb")
    , ppHiddenNoWindows = processWorkspace "" (unicode "2219")
    , ppOrder           = myLogOrder
    , ppSep             = "\n"
    , ppWsSep           = "\n"
    , ppTitle           = (titlePrefix ++) . const "" --shorten 40
    , ppTitleSanitize   = id
    , ppLayout          = (layoutSectionPrefix ++) . layoutFont . layoutMap
    }

-- Symbols to display for layouts
layoutSymbols = [tall, threeCol, spiral, full]
  where
    threeCol = unicode "f58d"
    tall     = unicode "f0db"
    spiral   = unicode "f2f9"
    full     = unicode "f24d"

-- Order of layouts
layoutPosition x = case x of
    "Tall"     -> 0
    "ThreeCol" -> 1
    "Spiral"   -> 2
    "Full"     -> 3
    x          -> -1

-- Font information
workspaceFont = applyFont "Hasklig" 60
workspaceRowPrefix = "${voffset -32}${offset 77}"
layoutFont = applyFont "Font Awesome 5 Free Solid" 25
layoutSectionPrefix = "${voffset -186}"
layoutSymbolPrefix = "${offset 310}${voffset 5}"
titlePrefix = "${alignc}${voffset -48}${font}"

processWorkspace :: String -> String -> String -> String
processWorkspace color symbol name =
  if name == "NSP"
    then "" -- filters scratchpad workspace
    else colorWrap color symbol

myLogOrder :: [String] -> [String]
myLogOrder (ws : rest) = splitToRows ws ++ rest
  where
    splitToRows ws =
        (workspaceRowPrefix ++) <$> unwords <$> ($lines ws) <$> splitFunctions
    splitFunctions =
        [ take 3 ,                        -- [ [ 1 2 3 ]
          take 3 . drop 3 ,               --   [ 4 5 6 ]
          drop 6 ]                        --   [ 7 8 9 ] ]

layoutMap :: String -> String
layoutMap =
    unlines . map (layoutSymbolPrefix ++) . highlightSymbol . last . words
  where
    highlightSymbol :: String -> [String]
    highlightSymbol x =
        zipWith ($) (highlightPosition $ layoutPosition x) layoutSymbols

    highlightPosition i = foo i (colorWrap conkyFocus)
    foo :: Int -> (a -> a) -> [a -> a]
    foo n f | n < 0     = repeat id
            | n == 0    = f : repeat id
            | otherwise = id : foo (n - 1) f

colorWrap :: String -> String -> String
colorWrap color base = "${color " ++ color ++ "}" ++ base ++ "${color}"

applyFont :: String -> Int -> String -> String
applyFont font size text =
    "${font " ++ font ++ ":pixelsize=" ++ show size ++ "}" ++ text

unicode :: String -> String
unicode code = "${exec printf \\\\u" ++ code ++ "}"

conkyVOffset :: Int -> String -> String
conkyVOffset n text =
    "${voffset " ++ show n ++ "}" ++ text ++ "${voffset " ++ show (-n) ++ "}"
