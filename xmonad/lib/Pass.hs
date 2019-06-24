
-- module XMonad.Prompt.Pass (
module Pass (
                            -- * Usage
                            -- $usage
                              passPrompt
                            , passGeneratePrompt
                            , passRemovePrompt
                            , passTypePrompt
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
-- > import XMonad.Prompt.Pass
--
-- Then add a keybinding for 'passPrompt', 'passGeneratePrompt' or 'passRemovePrompt':
--
-- >   , ((modMask , xK_p)                              , passPrompt xpconfig)
-- >   , ((modMask .|. controlMask, xK_p)               , passGeneratePrompt xpconfig)
-- >   , ((modMask .|. controlMask  .|. shiftMask, xK_p), passRemovePrompt xpconfig)
--
-- For detailed instructions on:
--
-- - editing your key bindings, see "XMonad.Doc.Extending#Editing_key_bindings".
--
-- - how to setup the password store, see <http://git.zx2c4.com/password-store/about/>
--

type Predicate = String -> String -> Bool

getPassCompl :: [String] -> Predicate -> String -> IO [String]
getPassCompl compls p s = return $ filter (p s) compls

type PromptLabel = String

newtype Pass = Pass PromptLabel

instance XPrompt Pass where
  showXPrompt       (Pass prompt) = prompt ++ ": "
  commandToComplete _ c           = c
  nextCompletion      _           = getNextCompletion

-- | Default password store folder in $HOME/.password-store
--
passwordStoreFolderDefault :: String -> String
passwordStoreFolderDefault home = combine home ".password-store"

-- | Compute the password store's location.
-- Use the PASSWORD_STORE_DIR environment variable to set the password store.
-- If empty, return the password store located in user's home.
--
passwordStoreFolder :: IO String
passwordStoreFolder =
  getEnv "PASSWORD_STORE_DIR" >>= computePasswordStoreDir
  where computePasswordStoreDir Nothing         = fmap passwordStoreFolderDefault getHomeDirectory
        computePasswordStoreDir (Just storeDir) = return storeDir

-- | A pass prompt factory
--
mkPassPrompt :: PromptLabel -> (String -> X ()) -> XPConfig -> X ()
mkPassPrompt promptLabel passwordFunction xpconfig = do
  passwords <- io (passwordStoreFolder >>= getPasswords)
  mkXPrompt (Pass promptLabel) xpconfig (getPassCompl passwords $ searchPredicate xpconfig) passwordFunction

-- | A prompt to retrieve a password from a given entry.
--
passPrompt :: XPConfig -> X ()
passPrompt = mkPassPrompt "Select password" selectPassword

-- | A prompt to generate a password for a given entry.
-- This can be used to override an already stored entry.
-- (Beware that no confirmation is asked)
--
passGeneratePrompt :: XPConfig -> X ()
passGeneratePrompt = mkPassPrompt "Generate password" generatePassword

-- | A prompt to remove a password for a given entry.
-- (Beware that no confirmation is asked)
--
passRemovePrompt :: XPConfig -> X ()
passRemovePrompt = mkPassPrompt "Remove password" removePassword

-- | A prompt to type in a password for a given entry.
-- This doesn't touch the clipboard.
--
passTypePrompt :: XPConfig -> X ()
passTypePrompt = mkPassPrompt "Type password" typePassword

-- | Select a password.
--
selectPassword :: String -> X ()
selectPassword passLabel = spawn $ "pass --clip \"" ++ escapeQuote passLabel ++ "\""

-- | Generate a 30 characters password for a given entry.
-- If the entry already exists, it is updated with a new password.
--
generatePassword :: String -> X ()
generatePassword passLabel = spawn $ "pass generate --force \"" ++ escapeQuote passLabel ++ "\" 30"

-- | Remove a password stored for a given entry.
--
removePassword :: String -> X ()
removePassword passLabel = spawn $ "pass rm --force \"" ++ escapeQuote passLabel ++ "\""

-- | Type a password stored for a given entry using xdotool.
--
typePassword :: String -> X ()
typePassword passLabel = spawn $ "pass \"" ++ escapeQuote passLabel
  ++ "\"|head -n1|tr -d '\n'|xdotool type --clearmodifiers --file -"

escapeQuote :: String -> String
escapeQuote = concatMap escape
  where escape :: Char -> String
        escape '"' = ['\\', '\"']
        escape x = return x

-- | Retrieve the list of passwords from the password store 'passwordStoreDir
getPasswords :: FilePath -> IO [String]
getPasswords passwordStoreDir = do
  files <- runProcessWithInput "find" [
    passwordStoreDir,
    "-type", "f",
    "-name", "*.gpg",
    "-printf", "%P\n"] []
  return . map removeGpgExtension $ lines files

removeGpgExtension :: String -> String
removeGpgExtension file | takeExtension file == ".gpg" = dropExtension file
                        | otherwise                    = file
