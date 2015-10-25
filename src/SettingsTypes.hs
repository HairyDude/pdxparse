module SettingsTypes
    ( L10n
    , Settings
        -- Export everything EXCEPT L10n
        ( steamDir
        , steamApps
        , game
        , language
        , gameVersion
        , currentFile
        , currentIndent
        )
    , emptySettings
    , setGameL10n
    , setL10n
    , PP
    , indentUp
    , withCurrentIndent
    , getGameL10n
    , getGameL10nDefault
    , getGameL10nIfPresent
    , withCurrentFile
    ) where

import Debug.Trace

import Control.Applicative
import Control.Monad.Reader

import Data.Maybe

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)

type L10n = HashMap Text Text

data Settings = Settings {
        steamDir    :: FilePath
    ,   steamApps   :: FilePath
    ,   game        :: String
    ,   language    :: String
    ,   gameVersion :: Text
    ,   gameL10n   :: L10n
    ,   l10n        :: L10n -- this app's messages
    -- Local state
    ,   currentFile :: Maybe FilePath
    ,   currentIndent :: Maybe Int
    } deriving (Show)

-- All undefined settings.
emptySettings :: Settings
emptySettings = Settings
    { steamDir = undefined
    , steamApps = undefined
    , game = undefined
    , language = undefined
    , gameVersion = undefined
    , gameL10n = undefined
    , l10n = undefined
    , currentFile = undefined
    , currentIndent = undefined
    }

setGameL10n :: Settings -> L10n -> Settings
setGameL10n settings l10n = settings { gameL10n = l10n }

setL10n :: Settings -> L10n -> Settings
setL10n settings l10n = settings { l10n = l10n }

type PP a = Reader Settings a

-- Increase current indentation by 1 for the given action.
-- If there is no current indentation, set it to 1.
indentUp :: PP a -> PP a
indentUp go = do
    mindent <- asks currentIndent
    let mindent' = maybe (Just 1) (Just . succ) mindent
    local (\s -> s { currentIndent = mindent' }) go

-- Pass the current indent to the action.
-- If there is no current indent, set it to 1.
withCurrentIndent :: (Int -> PP a) -> PP a
withCurrentIndent go = do
    mindent <- asks currentIndent
    local (\s ->
            if isNothing mindent
            then s { currentIndent = Just 1 }
            else s)
          (go . fromJust =<< asks currentIndent)

getGameL10n :: Text -> PP Text
getGameL10n key = HM.lookupDefault key key <$> asks gameL10n

getGameL10nDefault :: Text -> Text -> PP Text
getGameL10nDefault def key = HM.lookupDefault def key <$> asks gameL10n

getGameL10nIfPresent :: Text -> PP (Maybe Text)
getGameL10nIfPresent key = HM.lookup key <$> asks gameL10n

-- Pass the current file to the action.
-- If there is no current file, set it to "(unknown)".
withCurrentFile :: (String -> PP a) -> PP a
withCurrentFile go = do
    mfile <- asks currentFile
    local (\s -> if isNothing mfile
                    then s { currentFile = Just "(unknown)" }
                    else s)
          (go . fromJust =<< asks currentFile)