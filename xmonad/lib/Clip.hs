
-- module XMonad.Prompt.Clip (
module Clip (
                            -- * Usage
                            -- $usage
                              clipPrompt
                            , clipSavePrompt
                            , clipRemovePrompt
                            -- , clipTypePrompt
                            ) where

import XMonad.Core
import XMonad.Prompt ( XPrompt
                     , showXPrompt
                     , commandToComplete
                     , nextCompletion
                     , getNextCompletion
                     , XPConfig
                     , mkXPrompt
                     , searchPredicate)
import System.Directory (getHomeDirectory)
import System.FilePath (takeExtension, dropExtension, combine)
import System.Posix.Env (getEnv)
import XMonad.Util.Run (runProcessWithInput)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt.Clip
--
-- Then add a keybinding for 'clipPrompt', 'clipGeneratePrompt' or 'clipRemovePrompt':
--
-- >   , ((modMask , xK_p)                              , clipPrompt xpconfig)
-- >   , ((modMask .|. controlMask, xK_p)               , clipGeneratePrompt xpconfig)
-- >   , ((modMask .|. controlMask  .|. shiftMask, xK_p), clipRemovePrompt xpconfig)
--
-- For detailed instructions on:
--
-- - editing your key bindings, see "XMonad.Doc.Extending#Editing_key_bindings".
--
-- - how to setup the register store, see <http://git.zx2c4.com/register-store/about/>
--

type Predicate = String -> String -> Bool

getClipCompl :: [String] -> Predicate -> String -> IO [String]
getClipCompl compls p s = return $ filter (p s) compls

type PromptLabel = String

newtype Clip = Clip PromptLabel

instance XPrompt Clip where
  showXPrompt       (Clip prompt) = prompt ++ ": "
  commandToComplete _ c           = c
  nextCompletion      _           = getNextCompletion

-- | Default register store folder in $HOME/.register-store
--
registerStoreFolderDefault :: String -> String
registerStoreFolderDefault home = combine home ".registers"

-- | Compute the register store's location.
-- Use the PASSWORD_STORE_DIR environment variable to set the register store.
-- If empty, return the register store located in user's home.
--
registerStoreFolder :: IO String
registerStoreFolder =
  getEnv "REGISTERS_DIR" >>= computeRegisterStoreDir
  where computeRegisterStoreDir Nothing         = fmap registerStoreFolderDefault getHomeDirectory
        computeRegisterStoreDir (Just storeDir) = return storeDir

-- | A clip prompt factory
--
mkClipPrompt :: PromptLabel -> (String -> X ()) -> XPConfig -> X ()
mkClipPrompt promptLabel registerFunction xpconfig = do
  registers <- io (registerStoreFolder >>= getRegisters)
  mkXPrompt (Clip promptLabel) xpconfig (getClipCompl registers $ searchPredicate xpconfig) registerFunction

-- | A prompt to retrieve a register from a given entry.
--
clipPrompt :: XPConfig -> X ()
clipPrompt = mkClipPrompt "Copy from register" fromRegister

-- | A prompt to to a register for a given entry.
-- This can be used to override an already stored entry.
-- (Beware that no confirmation is asked)
--
clipSavePrompt :: XPConfig -> X ()
clipSavePrompt = mkClipPrompt "Paste to register" toRegister

-- | A prompt to remove a register for a given entry.
-- (Beware that no confirmation is asked)
--
clipRemovePrompt :: XPConfig -> X ()
clipRemovePrompt = mkClipPrompt "Clear register" removeRegister

-- | A prompt to type in a register for a given entry.
-- This doesn't touch the clipboard.
--
-- clipTypePrompt :: XPConfig -> X ()
-- clipTypePrompt = mkClipPrompt "Type to register" typeRegister

-- | from a register.
--
fromRegister :: String -> X ()
fromRegister clipLabel = do
  reg <- io registerStoreFolder
  spawn $ "xclip -selection clipboard -in \"" ++ reg ++ "/" ++ escapeQuote clipLabel ++ "\""

-- | Generate a 30 characters register for a given entry.
-- If the entry already exists, it is updated with a new register.
--
toRegister :: String -> X ()
toRegister clipLabel = do
  reg <- io registerStoreFolder
  spawn $ "xclip -selection clipboard -out > \"" ++ reg ++ "/" ++ escapeQuote clipLabel ++ "\""

-- | Remove a register stored for a given entry.
--
removeRegister :: String -> X ()
removeRegister clipLabel = spawn $ "rm \"" ++ escapeQuote clipLabel ++ "\""

-- | Type a register stored for a given entry using xdotool.
--
-- typeRegister :: String -> X ()
-- typeRegister clipLabel = spawn $ "xclip \"" ++ escapeQuote clipLabel
--   ++ "\"|head -n1|tr -d '\n'|xdotool type --clearmodifiers --file -"

escapeQuote :: String -> String
escapeQuote = concatMap escape
  where escape :: Char -> String
        escape '"' = ['\\', '\"']
        escape x = return x

-- | Retrieve the list of registers from the register store 'registerStoreDir
getRegisters :: FilePath -> IO [String]
getRegisters registerStoreDir = do
  files <- runProcessWithInput "find" [
    registerStoreDir,
    "-type", "f",
    "-printf", "%P\n"] []
  return $ lines files
