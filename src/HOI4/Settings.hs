{-# LANGUAGE OverloadedStrings #-}
module HOI4.Settings (
        fillSettings
    ,   writeHOI4Scripts
    ,   module HOI4.Types
    ) where

import Control.Arrow (second)
import Control.Monad.State

import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

import System.Directory
import System.FilePath

import Abstract
import FileIO
import SettingsTypes
import HOI4.Types
import Yaml

-- Handlers
import HOI4.Events (parseHOI4Events, writeHOI4Events)

fillSettings :: Settings -> IO (Settings, GameState)
fillSettings settings = do
    l10n' <- addTags (gamePath settings) (gameL10n settings)
    return $
        (settings {
                game = GameHOI4 {
                    readScripts = ScriptReader readHOI4Scripts
                ,   parseScripts = ScriptParser parseHOI4Scripts
                ,   writeScripts = ScriptWriter writeHOI4Scripts
                ,   hoi4data = HOI4Data HM.empty
                }
            ,   gameL10n = l10n'
            }
        , HOI4State (HOI4 {
                scopeStack = []
            }) Nothing Nothing
        )

-- | Add localizations for plain tags.
--
-- HOI4 does not have localizations for plain tags, e.g. FRA. Instead it has a
-- localization for each tag/ideology combo, e.g.
--
--    * FRA_fascism: "Vichy France"
--    * FRA_democratic: "France"
--    * FRA_neutrality: "Bourbon France"
--    * FRA_communism: "French Commune"
--
-- We could use the 1936 names (by reading history files), but many of them
-- imply an ideology, and that is not really appropriate for use in the
-- triggers, because they apply regardless of ideology. For example "tag = SPR"
-- matches both Nationalist Spain and Republican Spain. The names of the files
-- in history/countries/ (e.g. SPR - Spain.txt) have more neutral names, with a
-- couple of exceptions. Since more countries might be added in later patches,
-- we use those instead of historical names or a manual list.
addTags :: FilePath -> L10n -> IO L10n
addTags path l10n = do
    countries <-
        HM.fromList .
        map (second (LocEntry 0 . T.drop 3) . -- drop spaces and hyphen
             T.splitAt 3) .                   -- take tag
        filter (not . ("." `T.isPrefixOf`)) . -- drop dotfiles and ., ..
        map (T.pack . dropExtension . takeFileName)
            <$> getDirectoryContents (path </> "history" </> "countries")
    -- Exceptions
    let countries' = HM.singleton "l_english" (HM.fromList
            [ ("HOL", LocEntry 0 "Netherlands")                -- Holland
            , ("PRC", LocEntry 0 "People's Republic of China") -- ComChina
            , ("ENG", LocEntry 0 "United Kingdom")             -- Britain
            , ("USA", LocEntry 0 "United States")              -- USA
            ] `HM.union` countries )
    return (HM.unionWith HM.union countries' l10n)

-- Read all scripts in a directory.
-- Return: for each file, its path relative to the game root and the parsed
--         script.
readHOI4Scripts :: PPT IO GameScripts
readHOI4Scripts = GameScriptsHOI4 <$> do
    let readHOI4Script :: String -> PPT IO (HashMap String GenericScript)
        readHOI4Script category = do
            settings <- get
            let sourceSubdir = {-case category of
                    "policies" -> "common" </> "policies"
                    "ideagroups" -> "common" </> "ideas"
                    _          ->-} category
                sourceDir = buildPath settings sourceSubdir
            files <- liftIO (filterM (doesFileExist . buildPath settings . (sourceSubdir </>))
                                     =<< getDirectoryContents sourceDir)
            results <- forM files $ \filename -> do
                let target = sourceSubdir </> filename
                content <- join (liftIO . flip readScript target <$> get)
                when (null content) $
                    liftIO $ hPutStrLn stderr $
                        "Warning: " ++ target
                            ++ " contains no scripts - failed parse? Expected feature type "
                            ++ category
                return (target, content)
            return $ foldl (flip (uncurry HM.insert)) HM.empty results

    events <- readHOI4Script "events"
    return $ HOI4Scripts {
            hoi4eventScripts = events
        }

parseHOI4Scripts :: Monad m => GameScripts -> PPT m ()
parseHOI4Scripts (GameScriptsHOI4 (HOI4Scripts {
                    hoi4eventScripts = eventScripts
                })) = do
    events <- parseHOI4Events eventScripts

    modify $ \s -> case game s of
        GameHOI4 { hoi4data = gdata }
            -> s {
                game = (game s) {
                    hoi4data = gdata {
                        hoi4events = events
                    }
                }
            }
        _ -> error "parseHOI4Scripts passed wrong kind of scripts!"

writeHOI4Scripts :: PPT IO ()
writeHOI4Scripts = do
    writeHOI4Events
