module HOI4.Settings (
        HOI4 (..)
    ,   module HOI4.Types
    ) where

import Control.Arrow (second)
import Control.Monad (join, when, forM, filterM, void)
import Control.Monad.Trans (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (MonadState (..), StateT (..), modify, gets)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>), takeFileName, dropExtension)
import System.IO (hPutStrLn, stderr)

import Abstract -- everything
import FileIO (buildPath, readScript)
import SettingsTypes (PPT, Settings (..), L10nScheme (..), Game (..)
                     ,  IsGame (..), IsGameData (..), IsGameState (..)
                     ,  L10n
                     ,  getGameL10nIfPresent
                     )
import HOI4.Types -- everything
import Yaml (LocEntry (..))

-- Handlers
import HOI4.Events (parseHOI4Events, writeHOI4Events)

data HOI4 = HOI4
instance IsGame HOI4 where
    locScheme HOI4 = L10nQYAML
    readScripts = readHOI4Scripts
    parseScripts = parseHOI4Scripts
    writeScripts = writeHOI4Scripts
    data GameData HOI4 = HOI4D { hoi4d :: HOI4Data }
    data GameState HOI4 = HOI4S { hoi4s :: HOI4State }
    runWithInitState HOI4 settings hoi4 =
        void (runReaderT
                (runStateT hoi4 (HOI4D $ HOI4Data {
                    hoi4events = HM.empty
                ,   hoi4settings = settings
                ,   hoi4eventScripts = HM.empty
                }))
                (HOI4S $ HOI4State {
                    hoi4currentFile = Nothing
                ,   hoi4currentIndent = Nothing
                ,   hoi4scopeStack = []
                }))
    type Scope HOI4 = HOI4Scope
    scope s = local $ \(HOI4S st) -> HOI4S $
        st { hoi4scopeStack = s : hoi4scopeStack st }
    getCurrentScope = do
        HOI4S ss <- ask
        return $ case hoi4scopeStack ss of
            [] -> Nothing
            (sc:_) -> Just sc

instance HOI4Info HOI4 where
    getEventTitle eid = do
        mevt_t <- gets ((hoi4evt_title =<<)
                         . HM.lookup eid
                         . hoi4events . hoi4d)
        fmap join (sequence (getGameL10nIfPresent <$> mevt_t))
    getEventScripts = do
        HOI4D sd <- get
        return (hoi4eventScripts sd)
    setEventScripts scr = modify $ \(HOI4D sd) -> HOI4D $ sd {
            hoi4eventScripts = scr
        }
    getEvents = do
        HOI4D sd <- get
        return (hoi4events sd)

instance IsGameData (GameData HOI4) where
    getSettings (HOI4D sd) = hoi4settings sd

instance IsGameState (GameState HOI4) where
    currentFile (HOI4S st) = hoi4currentFile st
    modifyCurrentFile cf (HOI4S st) = HOI4S $ st {
            hoi4currentFile = cf
        }
    currentIndent (HOI4S st) = hoi4currentIndent st
    modifyCurrentIndent ci (HOI4S st) = HOI4S $ st {
            hoi4currentIndent = ci
        }

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
readHOI4Scripts :: forall m. MonadIO m => PPT HOI4 m ()
readHOI4Scripts = do
    let readHOI4Script :: String -> PPT HOI4 m (HashMap String GenericScript)
        readHOI4Script category = do
            settings <- gets getSettings
            let sourceSubdir = {-case category of
                    "policies" -> "common" </> "policies"
                    "ideagroups" -> "common" </> "ideas"
                    _          ->-} category
                sourceDir = buildPath settings sourceSubdir
            files <- liftIO (filterM (doesFileExist . buildPath settings . (sourceSubdir </>))
                                     =<< getDirectoryContents sourceDir)
            results <- forM files $ \filename -> do
                let target = sourceSubdir </> filename
                content <- liftIO $ readScript settings target
                when (null content) $
                    liftIO $ hPutStrLn stderr $
                        "Warning: " ++ target
                            ++ " contains no scripts - failed parse? Expected feature type "
                            ++ category
                return (target, content)
            return $ foldl (flip (uncurry HM.insert)) HM.empty results

    events <- readHOI4Script "events"
    modify $ \(HOI4D st) -> HOI4D $ st {
            hoi4eventScripts = events
        }

parseHOI4Scripts :: Monad m => PPT HOI4 m ()
parseHOI4Scripts = do
    events <- parseHOI4Events
    modify $ \(HOI4D s) -> HOI4D $ s {
            hoi4events = events
        }

writeHOI4Scripts :: (HOI4Info g, MonadIO m) => PPT g m ()
writeHOI4Scripts = do
    writeHOI4Events
