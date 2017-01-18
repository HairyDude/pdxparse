{-# LANGUAGE OverloadedStrings #-}
module EU4.Settings (
        fillSettings
    ,   writeEU4Scripts 
    ,   module EU4.Types
    ) where

import Control.Monad (join, when, forM, filterM)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (MonadState (..), modify)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import System.Directory (getDirectoryContents, doesFileExist)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Abstract -- everything
import FileIO (buildPath, readScript)
import SettingsTypes (PPT, Settings (..), Game (..), GameScripts (..), GameState (..)
                     , ScriptReader (..) -- TODO: get rid of this
                     , ScriptParser (..) -- TODO: get rid of this
                     , ScriptWriter (..) -- TODO: get rid of this
                     )
import EU4.Types -- everything
--import Text.PrettyPrint.Leijen.Text (Doc)
--import qualified Text.PrettyPrint.Leijen.Text as PP

-- Handlers
import EU4.Decisions (parseEU4Decisions, writeEU4Decisions)
import EU4.IdeaGroups (parseEU4IdeaGroups, writeEU4IdeaGroups)
--import EU4.Missions (parseEU4Missions, writeEU4Missions)
import EU4.Events (parseEU4Events, writeEU4Events)
--import EU4.Policies (parseEU4Policies, writeEU4Policies)

fillSettings :: Settings -> IO (Settings, GameState)
fillSettings settings = return $
    (settings {
        game = GameEU4 {
            readScripts = ScriptReader readEU4Scripts
        ,   parseScripts = ScriptParser parseEU4Scripts
        ,   writeScripts = ScriptWriter writeEU4Scripts
        ,   eu4data = EU4Data HM.empty HM.empty HM.empty
        }
    }, EU4State (EU4 {
        scopeStack = []
       }) Nothing Nothing)

-- Read all scripts in a directory.
-- Return: for each file, its path relative to the game root and the parsed
--         script.
readEU4Scripts :: PPT IO GameScripts
readEU4Scripts = GameScriptsEU4 <$> do
    let readEU4Script :: String -> PPT IO (HashMap String GenericScript)
        readEU4Script category = do
            settings <- get
            let sourceSubdir = case category of
                    "policies" -> "common" </> "policies"
                    "ideagroups" -> "common" </> "ideas"
                    _          -> category
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

    ideaGroups <- readEU4Script "ideagroups"
    decisions <- readEU4Script "decisions"
    events <- readEU4Script "events"
    return $ EU4Scripts {
            eu4ideaGroupScripts = ideaGroups
        ,   eu4decisionScripts = decisions
        ,   eu4eventScripts = events
        }

parseEU4Scripts :: Monad m => GameScripts -> PPT m ()
parseEU4Scripts (GameScriptsEU4 (EU4Scripts {
                    eu4ideaGroupScripts = ideaGroupScripts
                ,   eu4decisionScripts = decisionScripts
                ,   eu4eventScripts = eventScripts
                })) = do
    ideaGroups <- parseEU4IdeaGroups ideaGroupScripts
    decisions <- parseEU4Decisions decisionScripts
    events <- parseEU4Events eventScripts
    
    modify $ \s -> case game s of
        GameEU4 { eu4data = gdata }
            -> s {
                game = (game s) {
                    eu4data = gdata {
                        eu4events = events
                    ,   eu4decisions = decisions
                    ,   eu4ideagroups = ideaGroups
                    }
                }
            }
        _ -> error "parseEU4Scripts passed wrong kind of scripts!"

writeEU4Scripts :: PPT IO ()
writeEU4Scripts = do
    writeEU4IdeaGroups
    writeEU4Events
    writeEU4Decisions
