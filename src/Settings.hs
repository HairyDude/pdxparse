{-# LANGUAGE OverloadedStrings #-}
module Settings (
        Settings (..)
    ,   readSettings
    ,   module SettingsTypes
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Yaml

import Control.Applicative ((<$>), Applicative (..))

import System.Directory
import System.Exit
import System.IO
import qualified System.Info
import System.FilePath

import Control.Monad

import Abstract
import Localization
import SettingsTypes

-- intermediate structure. Maybe values don't need to be present in the
-- settings file.
data SettingsInput = SettingsInput {
        steamDriveI  :: Maybe String
    ,   steamDirI    :: Maybe FilePath
    ,   steamAppsI   :: Maybe FilePath
    ,   gameI        :: String
    ,   languageI    :: String
    ,   gameVersionI :: String
    } deriving (Show)
-- Settings is defined in SettingsTypes

instance FromJSON SettingsInput where
    parseJSON (Object o) = do
        settings <- o .: "settings"
        case settings of
            Object o' -> SettingsInput
                            <$> liftM (fmap T.unpack) (o' .:? "steam_drive")
                            <*> liftM (fmap T.unpack) (o' .:? "steam_dir")
                            <*> liftM (fmap T.unpack) (o' .:? "steam_apps")
                            <*> liftM T.unpack (o' .: "game")
                            <*> liftM T.unpack (o' .: "language")
                            <*> liftM T.unpack (o' .: "version")
            _ -> fail "bad settings file"

data Platform
    = Linux
    | MacOS
    | WindowsXP
    | Windows -- 7 or later (and Vista?)
    | Unknown -- AFAIK, these are the only platforms that support Steam.
    deriving (Eq, Show)
platform :: Platform
platform = case System.Info.os of
    "linux" -> Linux
    "darwin" -> MacOS
    osid -> -- Windows: running under either Cygwin or MinGW (more likely the latter).
        if      osid == "CYGWIN_NT-5.1" then WindowsXP
        else if take 6 osid == "CYGWIN" then Windows
        else if osid == "MINGW32_NT-5.1" then WindowsXP
        else if osid == "MINGW64_NT-5.1" then Unknown -- Steam doesn't support 64 bit XP afaik
        else if take 5 osid == "MINGW" then Windows
        else Unknown
{-# INLINE platform #-}

-- | Read the settings and localization files. If we can't, abort.
readSettings :: IO Settings
readSettings = do
    result <- decodeFileEither "settings.yml"
    case result of
        Right settings -> do
            steamDirCanonicalized <- case steamDirI settings of
                Just path -> return path
                Nothing -> case platform of
                    Linux -> do
                        home <- getHomeDirectory
                        return $ home </> ".local/share"
                    MacOS -> do
                        home <- getHomeDirectory
                        return $ home </> "Library/Application Support"
                    -- TODO: allow user to specify drive only.
                    WindowsXP -> return $ maybe "C" id (steamDriveI settings) ++ ":"
                                      </> maybe "Program Files" id (steamDirI settings)
                    Windows -> return $ maybe "C" id (steamDriveI settings) ++ ":"
                                      </> maybe "Program Files (x86)" id (steamDirI settings)
                    Unknown -> fail $ "Unknown platform: " ++ System.Info.os
            let steamAppsCanonicalized = maybe "Steam/steamapps/common" id (steamAppsI settings)
                provisionalSettings = emptySettings
                            { steamDir = steamDirCanonicalized
                            , steamApps = steamAppsCanonicalized
                            , game = gameI settings
                            , language = languageI settings
                            , gameVersion = T.pack (gameVersionI settings)
                            , currentFile = Nothing
                            , currentIndent = Nothing }
            game_l10n <- readL10n provisionalSettings
            l10n <- -- TODO: internationalize this
                    return HM.empty
            return $ provisionalSettings
                    `setGameL10n` game_l10n
                    `setL10n` l10n
        Left exc -> do
            hPutStrLn stderr $ "Couldn't parse settings: " ++ show exc
            exitFailure
