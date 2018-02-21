module Stellaris.Settings (
        Stellaris (..)
    ,   module Stellaris.Types
    ) where

import Control.Monad (join, when, forM, filterM, void)
import Control.Monad.Trans (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (MonadState (..), StateT (..), modify, gets)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Abstract -- everything
import FileIO (buildPath, readScript)
import SettingsTypes (PPT, Game (..), L10nScheme (..)
                     , IsGameData (..), IsGameState (..)
                     , Settings (..), GameState (..)
                     , getGameL10nIfPresent)
import Stellaris.Types -- everything

-- Handlers
import Stellaris.Events (parseStellarisEvents, writeStellarisEvents)

data Stellaris = Stellaris
instance IsGame Stellaris where
    locScheme _  = L10nQYAML
    readScripts  = readStellarisScripts
    parseScripts = parseStellarisScripts
    writeScripts = writeStellarisScripts
    data GameData Stellaris = StD { std :: StellarisData }
    data GameState Stellaris = StS { sts :: StellarisState }
    runWithInitState Stellaris settings st =
        void (runReaderT
                (runStateT st (StD $ StellarisData {
                    stevents = HM.empty
                ,   stsettings = settings
                ,   steventScripts = HM.empty
                }))
                (StS $ StellarisState {
                    stCurrentFile = Nothing
                ,   stCurrentIndent = Nothing
                ,   stScopeStack = []
                }))
    type Scope Stellaris = StellarisScope
    scope s = local $ \(StS st) -> StS $
        st { stScopeStack = s : stScopeStack st }
    getCurrentScope = do
        StS ss <- ask
        case stScopeStack ss of
            [] -> return Nothing
            (sc:_) -> return (Just sc)

instance StellarisInfo Stellaris where
    getEventTitle eid = do
        mevt_t <- gets ((stevt_title =<<)
                         . HM.lookup eid
                         . stevents . std)
        fmap join (sequence (getGameL10nIfPresent <$> mevt_t))
    getEventScripts = do
        StD sd <- get
        return (steventScripts sd)
    setEventScripts scr = modify $ \(StD sd) -> StD $ sd {
            steventScripts = scr
        }
    getEvents = do
        StD sd <- get
        return (stevents sd)

instance IsGameData (GameData Stellaris) where
    getSettings (StD sd) = stsettings sd

instance IsGameState (GameState Stellaris) where
    currentFile (StS st) = stCurrentFile st
    modifyCurrentFile cf (StS st) = StS $ st { stCurrentFile = cf }
    currentIndent (StS st) = stCurrentIndent st
    modifyCurrentIndent ci (StS st) = StS $ st { stCurrentIndent = ci }

{-
fillSettings :: Settings -> IO (Settings, GameState)
fillSettings settings = return $
    (settings {
        game = GameStellaris {
            readScripts = ScriptReader readStellarisScripts
        ,   parseScripts = ScriptParser parseStellarisScripts
        ,   writeScripts = ScriptWriter writeStellarisScripts
        ,   stdata = StellarisData HM.empty
        }
    }, StellarisState {
        scopeStack = []
    }) Nothing Nothing)
-}

-- Read all scripts in a directory.
-- Return: for each file, its path relative to the game root and the parsed
--         script.
readStellarisScripts :: MonadIO m => PPT Stellaris m ()
readStellarisScripts = do
    let readStellarisScript :: MonadIO m => String -> PPT Stellaris m (HashMap String GenericScript)
        readStellarisScript category = do
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

    events <- readStellarisScript "events"
    modify $ \(StD s) -> StD $ s {
            steventScripts = events
        }

parseStellarisScripts :: Monad m => PPT Stellaris m ()
parseStellarisScripts = do
    events <- parseStellarisEvents
    modify $ \(StD s) -> StD $ s {
            stevents = events
        }

writeStellarisScripts :: (MonadIO m, StellarisInfo g) => PPT g m ()
writeStellarisScripts = do
    writeStellarisEvents
