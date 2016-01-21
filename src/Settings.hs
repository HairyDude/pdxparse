{-# LANGUAGE OverloadedStrings #-}
module Settings (
        Settings (..)
    ,   readSettings
    ,   module SettingsTypes
    ) where

import Data.Maybe

import Data.Text (Text)
import qualified Data.Text as T

import Data.Yaml

import System.Directory
import System.Exit
import System.IO
import qualified System.Info
import System.FilePath

import Control.Monad

--import Abstract
import Localization
import SettingsTypes
import Paths_pdxparse

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
    parseJSON _ = fail "bad settings file"

data Platform
    = Linux
    | MacOS
    | WindowsXP
    | Windows -- 7 or later (and Vista?)
    | Unknown -- AFAIK, these are the only platforms that Steam supports.
    deriving (Eq, Show)
platform :: Platform
platform = case System.Info.os of
    "linux" -> Linux
    "darwin" -> MacOS
    "CYGWIN_NT-5.1"  -> WindowsXP
    "MINGW32_NT-5.1" -> WindowsXP
    "MINGW64_NT-5.1" -> Unknown -- Steam doesn't support 64 bit XP afaik
    osid -- Windows: running under either Cygwin or MinGW (more likely the latter).
        | take 6 osid == "CYGWIN"  -> Windows
        | take 5 osid == "MINGW"   -> Windows
        | otherwise                -> Unknown
{-# INLINE platform #-}

-- | Read the settings and localization files. If we can't, abort.
--
-- The argument is an action to run after all other settings have been
-- initialized, in order to get extra information.
readSettings :: (Settings a -> IO (Maybe a)) -> IO (Settings a)
readSettings getExtra = do
    settingsFile <- getDataFileName "settings.yml"
    result <- decodeFileEither settingsFile
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
                    WindowsXP -> return $ fromMaybe "C" (steamDriveI settings) ++ ":"
                                      </> fromMaybe "Program Files" (steamDirI settings)
                    Windows -> return $ fromMaybe "C" (steamDriveI settings) ++ ":"
                                      </> fromMaybe "Program Files (x86)" (steamDirI settings)
                    Unknown -> fail $ "Unknown platform: " ++ System.Info.os
            let steamAppsCanonicalized = fromMaybe "Steam/steamapps/common" (steamAppsI settings)
                provisionalSettings = emptySettings
                            { steamDir = steamDirCanonicalized
                            , steamApps = steamAppsCanonicalized
                            , game = gameI settings
                            , language = languageI settings
                            , gameVersion = T.pack (gameVersionI settings)
                            , currentFile = Nothing
                            , currentIndent = Nothing }
            game_l10n <- readL10n provisionalSettings
            let provisionalSettings' = provisionalSettings `setGameL10n` game_l10n
            extraInfo <- getExtra provisionalSettings'
            return provisionalSettings' { info = extraInfo }
        Left exc -> do
            hPutStrLn stderr $ "Couldn't parse settings: " ++ show exc
            exitFailure
