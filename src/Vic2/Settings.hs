module Vic2.Settings (
        Vic2 (..)
    ,   writeVic2Scripts
    ,   module Vic2.Types
    ) where

import Control.Monad (filterM, forM, join, when, void)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State (MonadState (..), StateT (..), modify, gets)

import Data.Maybe (listToMaybe)

import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Abstract -- everything
import FileIO (buildPath, readScript)
import SettingsTypes (PPT, Settings, IsGame (..), L10nScheme (..))
import Vic2.Types (Vic2Scope (..), Vic2Decision (..), Vic2Event (..))

import Debug.Trace (trace)

-- Handlers
--import Vic2.Decisions (parseVic2Decisions, writeVic2Decisions)
--import Vic2.Missions (parseVic2Missions, writeVic2Missions)
--import Vic2.Events (parseVic2Events, writeVic2Events)
--import Vic2.Policies (parseVic2Policies, writeVic2Policies)

data Vic2 = Vic2
instance IsGame Vic2 where
    locScheme _  = L10nCSV
    readScripts  = readVic2Scripts
    parseScripts = parseVic2Scripts
    writeScripts = writeVic2Scripts
    newtype GameData Vic2 = Vic2D { v2d :: Vic2Data }
    newtype GameState Vic2 = Vic2S { v2s :: Vic2State }
    runWithInitState Vic2 settings st =
        void (runReaderT
                (runStateT st (Vic2D $ Vic2Data {
                    vic2events = HM.empty
                ,   vic2decisions = HM.empty
                ,   vic2settings = settings
                ,   vic2decisionScripts = HM.empty
                ,   vic2eventScripts = HM.empty
                }))
                (Vic2S $ Vic2State {
                    vic2CurrentFile = Nothing
                ,   vic2CurrentIndent = Nothing
                ,   vic2ScopeStack = []
                }))
    type Scope Vic2 = Vic2Scope
    scope s = local $ \(Vic2S st) ->
        Vic2S $ st { vic2ScopeStack = s : vic2ScopeStack st }
    getCurrentScope = asks $ listToMaybe . vic2ScopeStack . v2s

data Vic2Data = Vic2Data {
        vic2events :: HashMap Text Vic2Event
    ,   vic2decisions :: HashMap Text Vic2Decision
    ,   vic2settings :: Settings
    ,   vic2eventScripts :: HashMap String GenericScript
    ,   vic2decisionScripts :: HashMap String GenericScript
    -- etc.
    }
data Vic2State = Vic2State {
        vic2ScopeStack :: [Vic2Scope]
    ,   vic2CurrentFile :: Maybe FilePath
    ,   vic2CurrentIndent :: Maybe Int
    } deriving (Show)

{-
fillSettings :: Settings -> IO (Settings, GameState)
fillSettings settings = return $
    (settings {
        game = GameVic2 {
            readScripts = ScriptReader  readVic2Scripts
        ,   parseScripts = ScriptParser parseVic2Scripts
        ,   writeScripts = ScriptWriter writeVic2Scripts
        ,   vic2data = Vic2Data HM.empty HM.empty
        }
    }, Vic2State (Vic2 {
        scopeStack = []
       }) Nothing Nothing)
-}

-- Read all scripts in a directory.
-- Return: for each file, its path relative to the game root and the parsed
--         script.
readVic2Scripts :: forall m. MonadIO m => PPT Vic2 (ExceptT Text m) ()
readVic2Scripts = do
    let readVic2Script :: String -> PPT Vic2 (ExceptT Text m) (HashMap String GenericScript)
        readVic2Script category = do
            settings <- gets (vic2settings . v2d)
            --settings <- _ vic2settings
            let sourceSubdir = case category of
                    "policies" -> "common" </> "policies"
                    "ideagroups" -> "common" </> "ideas"
                    _          -> category
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

    decisions <- readVic2Script "decisions"
    events <- readVic2Script "events"
    modify $ \(Vic2D d) -> Vic2D $ d {
            vic2decisionScripts = decisions
        ,   vic2eventScripts = events
        }

parseVic2Scripts :: Monad m => PPT Vic2 m ()
parseVic2Scripts = do
    decisionScripts <- gets (vic2decisionScripts . v2d)
    eventScripts <- gets (vic2eventScripts . v2d)
    decisions <- parseVic2Decisions decisionScripts
    events <- parseVic2Events eventScripts

    modify $ \(Vic2D s) -> Vic2D $ s {
            vic2events = events
        ,   vic2decisions = decisions
        }

writeVic2Scripts :: MonadIO m => PPT Vic2 m ()
writeVic2Scripts = do
    writeVic2Events
    writeVic2Decisions

-- placeholders
parseVic2Decisions :: Monad m => HashMap String GenericScript -> PPT Vic2 m (HashMap Text Vic2Decision)
parseVic2Decisions _ = trace "Victoria 2 decisions not yet implemented" $ return HM.empty

parseVic2Events :: Monad m => HashMap String GenericScript -> PPT Vic2 m (HashMap Text Vic2Event)
parseVic2Events _ = trace "Victoria 2 events not yet implemented" $ return HM.empty

writeVic2Events :: MonadIO m => PPT Vic2 m ()
writeVic2Events = liftIO $ putStrLn "Victoria 2 events not yet implemented"

writeVic2Decisions :: MonadIO m => PPT Vic2 m ()
writeVic2Decisions = liftIO $ putStrLn "Victoria 2 decisions not yet implemented"
