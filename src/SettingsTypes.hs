{-# LANGUAGE OverloadedStrings #-}
--, RankNTypes, 
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving, FlexibleContexts, FlexibleInstances  #-}
module SettingsTypes (
        L10n
    ,   CLArgs (..)
    ,   L10nScheme (..)
    ,   Game (..), IsGame (..)
    ,   IsGameData (..)
    ,   IsGameState (..)
    ,   Settings (..)
    ,   setGameL10n
    ,   PP, PPT
    ,   hoistErrors, hoistExceptions
    ,   indentUp, indentDown
    ,   withCurrentIndent, withCurrentIndentZero
    ,   alsoIndent, alsoIndent'
    ,   getGameL10n
    ,   getGameL10nDefault
    ,   getGameL10nIfPresent
    ,   setCurrentFile, withCurrentFile
    ,   getLangs
    ,   unfoldM, concatMapM
    ,   fromReaderT, toReaderT
    ) where

import Control.Monad (liftM, join, void)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (Reader (..), ReaderT (..), MonadReader (..), asks)
import Control.Monad.State (StateT (..), MonadState (..), execStateT, gets)

import Data.Foldable (fold)
import Data.Maybe (isNothing, fromJust, listToMaybe)

import Data.Text (Text)
import Text.Shakespeare.I18N (Lang)
import Text.PrettyPrint.Leijen.Text (Doc)
--import qualified Text.PrettyPrint.Leijen.Text as PP

import qualified Data.HashMap.Strict as HM

import Abstract -- everything
import Yaml (L10n, L10nLang, LocEntry (..))

-- Command line arguments.
data CLArgs
    = Paths
    | Version
    deriving (Show, Eq)

-- Choice of localization scheme
data L10nScheme
    = L10nCSV   -- CSV (semicolon-delimited), for CK2 and earlier
    | L10nQYAML -- quasi-YAML, for EU4 and later
    deriving (Show, Eq)

----------------------------
-- Game specific settings --
----------------------------

{- Old settings types
data Game
    = GameUnknown
    | GameEU4 {
            readScripts :: ScriptReader
        ,   parseScripts :: ScriptParser
        ,   writeScripts :: ScriptWriter
        ,   eu4data :: EU4Data
        }
    | GameStellaris {
            readScripts :: ScriptReader
        ,   parseScripts :: ScriptParser
        ,   writeScripts :: ScriptWriter
        ,   stdata :: StellarisData
        }
    | GameHOI4 {
            readScripts :: ScriptReader
        ,   parseScripts :: ScriptParser
        ,   writeScripts :: ScriptWriter
        ,   hoi4data :: HOI4Data
        }
    | GameVic2 {
            readScripts :: ScriptReader
        ,   parseScripts :: ScriptParser
        ,   writeScripts :: ScriptWriter
        ,   vic2data :: Vic2Data
        }
    deriving (Show)
-}

-- Type for the Reader
class IsGameState s where
    currentFile :: s -> Maybe FilePath
    modifyCurrentFile :: Maybe FilePath -> s -> s
    currentIndent :: s -> Maybe Int
    modifyCurrentIndent :: Maybe Int -> s -> s

-- Type for the State
class IsGameData d where
    getSettings :: d -> Settings

class IsGame g where
    locScheme :: g -> L10nScheme
    readScripts :: MonadIO m => PPT g (ExceptT Text m) ()
    parseScripts :: Monad m => PPT g m ()
    writeScripts :: MonadIO m => PPT g m ()
    data GameData g -- should be IsGameData
    data GameState g -- should be IsGameState
    runWithInitState :: g -> Settings -> PPT g IO () -> IO ()
    type Scope g -- synonym rather than data, because the statement handlers need to know it.
    scope :: Monad m => Scope g -> PPT g m a -> PPT g m a
    getCurrentScope :: Monad m => PPT g m (Maybe (Scope g))

-- Example game. Define this in the Settings module, with the Game instance.
-- Do NOT define it in Types. Instead, have game-specific code be polymorphic
-- over Game.
data UnknownGame = UnknownGame
instance IsGame UnknownGame where
    locScheme _ = L10nQYAML
    readScripts = return ()
    parseScripts = return ()
    writeScripts = return ()
    newtype GameData UnknownGame = UGD { ugd :: UnknownGameData }
    newtype GameState UnknownGame = UGS { ugs :: UnknownGameState }
    runWithInitState UnknownGame settings st =
        void (runReaderT
                (runStateT st (UGD $ UnknownGameData {
                    ugSettings = settings
                }))
                (UGS $ UnknownGameState {
                    ugScopeStack = []
                ,   ugCurrentFile = Nothing
                ,   ugCurrentIndent = Nothing
                }))
    type Scope UnknownGame = UnknownGameScope
    scope s = local $ \(UGS st) ->
        UGS $ st { ugScopeStack = s : ugScopeStack st }
    getCurrentScope = asks $ listToMaybe . ugScopeStack . ugs

-- Define these in the Types module, including instances.
data UnknownGameScope = UnknownGameScope

data UnknownGameData = UnknownGameData {
            ugSettings :: Settings
        }
instance IsGameData (GameData UnknownGame) where
    getSettings (UGD d) = ugSettings d

data UnknownGameState = UnknownGameState {
            ugScopeStack :: [Scope UnknownGame]
        ,   ugCurrentFile :: Maybe FilePath
        ,   ugCurrentIndent :: Maybe Int
        }
instance IsGameState (GameState UnknownGame) where
    currentFile (UGS s) = ugCurrentFile s
    modifyCurrentFile mcf (UGS s) = UGS $ s { ugCurrentFile = mcf }
    currentIndent (UGS s) = ugCurrentIndent s
    modifyCurrentIndent mci (UGS s) = UGS $ s { ugCurrentIndent = mci }

-- | Existentially quantified game info type.
data Game where
    Game :: IsGame g => g -> Game

----------------------
-- Generic settings --
----------------------

data Settings = Settings {
        steamDir    :: FilePath -- Parent of the Steam directory
                                -- e.g. /home/username/.local/share or C:\Program Files (x86)
    ,   steamApps   :: FilePath -- Steam apps directory under steamDir
                                -- usually Steam/steamapps/common
    ,   l10nScheme  :: L10nScheme
    ,   game        :: Game
    ,   gameFolder  :: String   -- Folder under apps directory containing the game
                                -- usually same as game name, e.g. "Hearts of Iron IV"
    ,   gamePath    :: FilePath -- Full path to game directory
    ,   language    :: Text
    ,   languageS   :: String -- for FilePaths
    ,   gameVersion :: Text
    ,   gameL10n    :: L10n
    ,   langs       :: [Lang]
    ,   settingsFile :: FilePath
    ,   clargs      :: [CLArgs]
    ,   filesToProcess :: [FilePath]
    }

setGameL10n :: Settings -> L10n -> Settings
setGameL10n settings l10n = settings { gameL10n = l10n }

-- Pretty-printing monad, and its transformer version
-- Normally s will be an instance of IsGameState, either SomeGameState or a
-- game-specific instance.
type PP g = StateT (GameData g) (Reader (GameState g)) -- equal to PPT g Identity a
type PPT g m = StateT (GameData g) (ReaderT (GameState g) m)

-- Convert a PP wrapping errors into a PP returning Either.
-- TODO: generalize
hoistErrors :: Monad m => PPT g (Either e) a -> PPT g m (Either e a)
hoistErrors (StateT rd) =
    StateT $ \settings ->
        ReaderT $ \st -> case runReaderT (rd settings) st of
            Left err -> return (Left err, settings)
            Right (res, settings') -> return (Right res, settings')

hoistExceptions :: Monad m => PPT g (ExceptT e m) a -> PPT g m (Either e a)
hoistExceptions (StateT rd) =
    StateT $ \settings ->
        ReaderT $ \st -> do
            result <- runExceptT (runReaderT (rd settings) st)
            case result of
                Left e -> return (Left e, settings)
                Right (r, settings') -> return (Right r, settings')

-- Increase current indentation by 1 for the given action.
-- If there is no current indentation, set it to 1.
indentUp :: (IsGameState (GameState g), Monad m) => PPT g m a -> PPT g m a
indentUp go = do
    mindent <- asks currentIndent
    let mindent' = Just (maybe 1 succ mindent)
    local (modifyCurrentIndent mindent') go

-- Decrease current indent level by 1 for the given action.
-- For use where a level of indentation should be skipped.
indentDown :: (IsGameState (GameState g), Monad m) => PPT g m a -> PPT g m a
indentDown go = do
    mindent <- asks currentIndent
    let mindent' = Just (maybe 0 pred mindent)
    local (modifyCurrentIndent mindent') go

-- | Pass the current indent to the action.
-- If there is no current indent, set it to 1.
withCurrentIndent ::
    (IsGameState (GameState g), Monad m) => (Int -> PPT g m a) -> PPT g m a
withCurrentIndent = withCurrentIndentBaseline 1

-- | Pass the current indent to the action.
-- If there is no current indent, set it to 0.
withCurrentIndentZero ::
    (IsGameState (GameState g), Monad m) => (Int -> PPT g m a) -> PPT g m a
withCurrentIndentZero = withCurrentIndentBaseline 0

withCurrentIndentBaseline ::
    (IsGameState (GameState g), Monad m) =>
        Int -> (Int -> PPT g m a) -> PPT g m a
withCurrentIndentBaseline base go =
    local (\s ->
            if isNothing (currentIndent s)
            then modifyCurrentIndent (Just base) s
            else s)
          -- fromJust guaranteed to succeed
          (go . fromJust =<< asks currentIndent)

-- Bundle a value with the current indentation level.
alsoIndent ::
    (IsGameState (GameState g), Monad m) => PPT g m a -> PPT g m (Int, a)
alsoIndent mx = withCurrentIndent $ \i -> mx >>= \x -> return (i,x)
alsoIndent' :: (IsGameState (GameState g), Monad m) => a -> PPT g m (Int, a)
alsoIndent' x = withCurrentIndent $ \i -> return (i,x)

getCurrentLang :: (IsGameData (GameData g), Monad m) => PPT g m L10nLang
getCurrentLang = HM.lookupDefault HM.empty <$> gets (language . getSettings) <*> gets (gameL10n . getSettings)

getGameL10n :: (IsGameData (GameData g), Monad m) => Text -> PPT g m Text
getGameL10n key = content <$> HM.lookupDefault (LocEntry 0 key) key <$> getCurrentLang

getGameL10nDefault :: (IsGameData (GameData g), Monad m) => Text -> Text -> PPT g m Text
getGameL10nDefault def key = content <$> HM.lookupDefault (LocEntry 0 def) key <$> getCurrentLang

getGameL10nIfPresent :: (IsGameData (GameData g), Monad m) => Text -> PPT g m (Maybe Text)
getGameL10nIfPresent key = fmap content <$> HM.lookup key <$> getCurrentLang

-- Pass the current file to the action.
-- If there is no current file, set it to "(unknown)".
withCurrentFile :: (IsGameState (GameState g), Monad m) => (String -> PPT g m a) -> PPT g m a
withCurrentFile go = do
    mfile <- asks currentFile
    local (\s -> if isNothing mfile
                    then modifyCurrentFile (Just "(unknown)") s
                    else s)
          -- fromJust guaranteed to succeed
          (go . fromJust =<< asks currentFile)

-- Set the current file for the action.
setCurrentFile :: (IsGameState (GameState g), Monad m) => String -> PPT g m a -> PPT g m a
setCurrentFile f = local (modifyCurrentFile (Just f))

-- Get the list of output languages.
getLangs :: (IsGameData (GameData g), Monad m) => PPT g m [Lang]
getLangs = gets (langs . getSettings)

-- Misc. utilities

-- As unfoldr, but argument is monadic
unfoldM :: Monad m => (a -> m (Maybe (b, a))) -> a -> m [b]
unfoldM f = go where
    go x = do
        res <- f x
        case res of
            Nothing -> return []
            Just (next, x') -> do
                rest <- go x'
                return (next:rest)

concatMapM :: (Monad m, Traversable t, Monoid (t b)) => (a -> m (t b)) -> t a -> m (t b)
concatMapM f xs = liftM fold . mapM f $ xs

fromReaderT :: ReaderT r m a -> Reader r (m a)
fromReaderT mx = runReaderT mx <$> ask

toReaderT :: Reader r (m a) -> ReaderT r m a
toReaderT mx = ReaderT (runIdentity . runReaderT mx)
