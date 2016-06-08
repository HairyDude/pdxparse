{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module SettingsTypes (
        L10n
    ,   CLArgs (..)
    ,   Game (..)
    ,   GameState (..)
    ,   GameScripts (..)
    ,   ScriptReader (..)
    ,   ScriptParser (..)
    ,   ScriptWriter (..)
    ,   Handler (..)
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

import Control.Monad.Except
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader
import Control.Monad.State

import Data.Foldable (fold)
import Data.Maybe

import Data.Text (Text)
import Text.Shakespeare.I18N (Lang)

import qualified Data.HashMap.Strict as HM

import Abstract
import Doc
import EU4.Types
import HOI4.Types
import Stellaris.Types
import Yaml

-- Command line arguments.
data CLArgs
    = Paths
    | Version
    deriving (Show, Eq)

----------------------------
-- Game specific settings --
----------------------------

newtype ScriptReader = ScriptReader {
        runScriptReader :: PPT IO GameScripts
    }
instance Show ScriptReader where
    show _ = "<script reader>"
newtype Handler = Handler {
        runHandler :: GenericStatement -> PPT (Either Text) [Either Text (FilePath, Doc)]
    }
instance Show Handler where
    show _ = "<feature handler>"

newtype ScriptParser = ScriptParser {
        runScriptParser :: forall m. Monad m => GameScripts -> PPT m ()
    }
instance Show ScriptParser where
    show _ = "<script parser>"

newtype ScriptWriter = ScriptWriter {
        runScriptWriter :: PPT IO ()
    }
instance Show ScriptWriter where
    show _ = "<script parser>"

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
    deriving (Show)

-- State to store in a Reader.
data GameState
    = EU4State {
            gEU4 :: EU4
        ,   currentIndent :: Maybe Int
        ,   currentFile :: Maybe FilePath
        }
    | StellarisState {
            gStellaris :: Stellaris
        ,   currentIndent :: Maybe Int
        ,   currentFile :: Maybe FilePath
        }
    | HOI4State {
            gHOI4 :: HOI4
        ,   currentIndent :: Maybe Int
        ,   currentFile :: Maybe FilePath
        }
    deriving (Show)

-- Scripts after reading.
data GameScripts
    = GameScriptsEU4 EU4Scripts
    | GameScriptsHOI4 HOI4Scripts
    | GameScriptsStellaris StellarisScripts
    deriving (Show)

----------------------
-- Generic settings --
----------------------

data Settings = Settings {
        steamDir    :: FilePath -- Parent of the Steam directory
                                -- e.g. /home/username/.local/share or C:\Program Files (x86)
    ,   steamApps   :: FilePath -- Steam apps directory under steamDir
                                -- usually Steam/steamapps/common
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
    } deriving (Show)

setGameL10n :: Settings -> L10n -> Settings
setGameL10n settings l10n = settings { gameL10n = l10n }

-- Pretty-printing monad, and its transformer version
type PP = StateT Settings (Reader GameState) -- equal to PPT Identity a
type PPT m = StateT Settings (ReaderT GameState m)

-- Convert a PP wrapping errors into a PP returning Either.
-- TODO: generalize
hoistErrors :: Monad m => PPT (Either e) a -> PPT m (Either e a)
hoistErrors (StateT rd) =
    StateT $ \settings ->
        ReaderT $ \st -> case runReaderT (rd settings) st of
            Left err -> return (Left err, settings)
            Right (res, settings') -> return (Right res, settings')

hoistExceptions :: Monad m => PPT (ExceptT e m) a -> PPT m (Either e a)
hoistExceptions (StateT rd) =
    StateT $ \settings ->
        ReaderT $ \st -> do
            result <- runExceptT (runReaderT (rd settings) st)
            case result of
                Left e -> return (Left e, settings)
                Right (r, settings') -> return (Right r, settings')

-- Increase current indentation by 1 for the given action.
-- If there is no current indentation, set it to 1.
indentUp :: Monad m => PPT m a -> PPT m a
indentUp go = do
    mindent <- asks currentIndent
    let mindent' = Just (maybe 1 succ mindent)
    local (\s -> s { currentIndent = mindent' }) go

-- Decrease current indent level by 1 for the given action.
-- For use where a level of indentation should be skipped.
indentDown :: Monad m => PPT m a -> PPT m a
indentDown go = do
    mindent <- asks currentIndent
    let mindent' = Just (maybe 0 pred mindent)
    local (\s -> s { currentIndent = mindent' }) go

-- | Pass the current indent to the action.
-- If there is no current indent, set it to 1.
withCurrentIndent :: Monad m => (Int -> PPT m a) -> PPT m a
withCurrentIndent = withCurrentIndentBaseline 1

-- | Pass the current indent to the action.
-- If there is no current indent, set it to 0.
withCurrentIndentZero :: Monad m => (Int -> PPT m a) -> PPT m a
withCurrentIndentZero = withCurrentIndentBaseline 0

withCurrentIndentBaseline :: Monad m => Int -> (Int -> PPT m a) -> PPT m a
withCurrentIndentBaseline base go =
    local (\s ->
            if isNothing (currentIndent s)
            then s { currentIndent = Just base }
            else s)
          -- fromJust guaranteed to succeed
          (go . fromJust =<< asks currentIndent)

-- Bundle a value with the current indentation level.
alsoIndent :: Monad m => PPT m a -> PPT m (Int, a)
alsoIndent mx = withCurrentIndent $ \i -> mx >>= \x -> return (i,x)
alsoIndent' :: Monad m => a -> PPT m (Int, a)
alsoIndent' x = withCurrentIndent $ \i -> return (i,x)

getCurrentLang :: Monad m => PPT m L10nLang
getCurrentLang = HM.lookupDefault HM.empty <$> gets language <*> gets gameL10n

getGameL10n :: Monad m => Text -> PPT m Text
getGameL10n key = content <$> HM.lookupDefault (LocEntry 0 key) key <$> getCurrentLang

getGameL10nDefault :: Monad m => Text -> Text -> PPT m Text
getGameL10nDefault def key = content <$> HM.lookupDefault (LocEntry 0 def) key <$> getCurrentLang

getGameL10nIfPresent :: Monad m => Text -> PPT m (Maybe Text)
getGameL10nIfPresent key = fmap content <$> HM.lookup key <$> getCurrentLang

-- Pass the current file to the action.
-- If there is no current file, set it to "(unknown)".
withCurrentFile :: Monad m => (String -> PPT m a) -> PPT m a
withCurrentFile go = do
    mfile <- asks currentFile
    local (\s -> if isNothing mfile
                    then s { currentFile = Just "(unknown)" }
                    else s)
          -- fromJust guaranteed to succeed
          (go . fromJust =<< asks currentFile)

-- Set the current file for the action.
setCurrentFile :: Monad m => String -> PPT m a -> PPT m a
setCurrentFile f = local (\s -> s { currentFile = Just f })

-- Get the list of output languages.
getLangs :: Monad m => PPT m [Lang]
getLangs = gets langs

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
