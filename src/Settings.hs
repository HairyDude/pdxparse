{-# LANGUAGE OverloadedStrings #-}
module Settings (
        Settings (..)
    ,   readSettings
    ,   module SettingsTypes
    ) where

import Data.Maybe

import Data.Char (toLower)
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
        settingsIn <- o .: "settings"
        case settingsIn of
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
platform = case map toLower System.Info.os of
    "linux" -> Linux
    "darwin" -> MacOS
    "cygwin_nt-5.1"  -> WindowsXP
    "mingw32_nt-5.1" -> WindowsXP
    "mingw64_nt-5.1" -> Unknown -- Steam doesn't support 64 bit XP afaik
    osid -- Windows: running under either Cygwin or MinGW (more likely the latter).
        | take 6 osid == "cygwin"  -> Windows
        | take 5 osid == "mingw"   -> Windows
        | otherwise                -> Unknown
{-# INLINE platform #-}

-- | Read the settings and localization files. If we can't, abort.
--
-- The argument is an action to run after all other settings have been
-- initialized, in order to get extra information.
readSettings :: (Settings () -> IO a) -> IO (Settings a)
readSettings getExtra = do
    settingsFile <- getDataFileName "settings.yml"
    result <- decodeFileEither settingsFile
    case result of
        Right settingsIn -> do
            steamDirCanonicalized <- case steamDirI settingsIn of
                Just path -> return path
                Nothing -> case platform of
                    Linux -> do
                        home <- getHomeDirectory
                        return $ home </> ".local/share"
                    MacOS -> do
                        home <- getHomeDirectory
                        return $ home </> "Library/Application Support"
                    -- TODO: allow user to specify drive only.
                    WindowsXP -> return $ fromMaybe "C" (steamDriveI settingsIn) ++ ":"
                                      </> fromMaybe "Program Files" (steamDirI settingsIn)
                    Windows -> return $ fromMaybe "C" (steamDriveI settingsIn) ++ ":"
                                      </> fromMaybe "Program Files (x86)" (steamDirI settingsIn)
                    Unknown -> fail $ "Unknown platform: " ++ System.Info.os
            let steamAppsCanonicalized = fromMaybe "Steam/steamapps/common" (steamAppsI settingsIn)
                provisionalSettings = (settings ())
                            { steamDir = steamDirCanonicalized
                            , steamApps = steamAppsCanonicalized
                            , game = gameI settingsIn
                            , language = languageI settingsIn
                            , gameVersion = T.pack (gameVersionI settingsIn)
                            , currentFile = Nothing
                            , currentIndent = Nothing }
            game_l10n <- readL10n provisionalSettings
            let provisionalSettings' = provisionalSettings `setGameL10n` game_l10n
            extraInfo <- getExtra provisionalSettings'
            return provisionalSettings' { info = extraInfo }
        Left exc -> do
            hPutStrLn stderr $ "Couldn't parse settings: " ++ show exc
            exitFailure
