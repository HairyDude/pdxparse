{- |
Module      : SettingsTypes
Description : Types for program configuration and structure
-}
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
    ,   unsnoc, safeLast, safeIndex
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

-- | Command line arguments.
data CLArgs
    = Paths
    | Version
    deriving (Show, Eq)

-- | Choice of localization scheme.
data L10nScheme
    = L10nCSV   -- ^ CSV (semicolon-delimited), for CK2 and earlier.
    | L10nQYAML -- ^ Quasi-YAML, for EU4 1.17 and later (EU4 before 1.17 not supported).
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

-- | Type for the Reader. This should include a scope stack if the game needs
-- one (most PDS games do). The top of a scope stack indicates the current
-- scope, which affects the interpretation of some statements - for example,
-- for EU4, @prestige = 1@ in bonus scope means "+1 yearly prestige", in a
-- command scope means "gain 1 prestige".
class IsGameState s where
    currentFile :: s -> Maybe FilePath
    modifyCurrentFile :: Maybe FilePath -> s -> s
    currentIndent :: s -> Maybe Int
    modifyCurrentIndent :: Maybe Int -> s -> s

-- | Type for the State. This should include fields for all the raw (AST)
-- scripts and processed data needed by the feature handlers.
class IsGameData d where
    getSettings :: d -> Settings

-- | Top level handlers for the game scripts, features etc.
class IsGame g where
    -- | Localization scheme used by this game.
    locScheme :: g -> L10nScheme
    -- | Action to read the game scripts and parse to AST. They should be
    -- stored with 'Control.Monad.State.modify'.
    readScripts :: MonadIO m => PPT g (ExceptT Text m) ()
    -- | Action to interpret the game scripts as usable data. The data should
    -- be stored with 'Control.Monad.State.modify'.
    parseScripts :: Monad m => PPT g m ()
    -- | Action to output the game scripts.
    writeScripts :: MonadIO m => PPT g m ()
    -- | Game data type. Should be an instance of 'IsGameData' and include all
    -- parsed data needed by the game's statement handlers.
    data GameData g
    -- | Parser state type. Should be an instance of 'IsGameState' and include
    -- a stack of context types (e.g. bonus, country, province, trade node,
    -- etc.). The topmost element indicates the current scope, which affects
    -- the interpretation of some statements for example, for EU4,
    -- @prestige = 1@ in bonus scope means "+1 yearly prestige", in a command
    -- scope means "gain 1 prestige".
    data GameState g
    runWithInitState :: g -> Settings -> PPT g IO () -> IO ()
    -- | Scopes for the scope stack. This is an associated type synonym rather
    -- than an associated data type, because the statement handlers need to
    -- know it; you should define this type in the game's @Types@ module.
    type Scope g
    -- | Push a new scope onto the stack.
    scope :: Monad m => Scope g -> PPT g m a -> PPT g m a
    -- | Query the current scope.
    getCurrentScope :: Monad m => PPT g m (Maybe (Scope g))
    -- | Query the previous scope (i.e. the type of PREV).
    getPrevScope :: Monad m => PPT g m (Maybe (Scope g))
    -- | Query the root scope (i.e. the type of ROOT).
    getRootScope :: Monad m => PPT g m (Maybe (Scope g))
    -- | Get the entire scope stack. Wanted only for debugging purposes.
    getScopeStack :: Monad m => PPT g m [Scope g]
    getScopeStack = undefined

-- Example game. Define your game and its 'IsGame' instance in your game's
-- 'Settings' module. Do NOT define it in Types. Instead, have game-specific
-- code be polymorphic over Game.
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
    getPrevScope = asks $ safeIndex 1 . ugScopeStack . ugs
    getRootScope = asks $ safeLast . ugScopeStack . ugs
    getScopeStack = asks $ ugScopeStack . ugs

-- | Example scope, data and state types. Define these in the 'Types' module,
-- including instances.
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

-- | Existentially quantified game handlers type.
-- 
-- To support a new game, create an actions type for it in the game's
-- @Settings@ module, and make it an instance of 'IsGame'. Do /not/ define it
-- in the game's @Types@ module. Instead, have game-specific code be
-- polymorphic over 'IsGame'.
--
-- See the source for "SettingsTypes" or "EU4.Settings" for an example IsGame
-- instance.
data Game where
    Game :: IsGame g => g -> Game

----------------------
-- Generic settings --
----------------------

-- | Program settings. This is read by readSettings in Main before any
-- game files are read, except for localization files.
data Settings = Settings {
        steamDir    :: FilePath -- ^ Parent of the Steam directory,
                                --   e.g. @/home/username/.local/share@ or
                                -- @C:\Program Files (x86)@
    ,   steamApps   :: FilePath -- ^ Steam apps directory under steamDir,
                                --   usually @Steam/steamapps/common@
    ,   l10nScheme  :: L10nScheme -- ^ Localization scheme being used
    ,   game        :: Game     -- ^ Game-specific actions.
    ,   gameFolder  :: String   -- ^ Folder under apps directory containing the
                                --   game files.  Usually the same as the
                                --   game's name, e.g. "Hearts of Iron IV".
    ,   gamePath    :: FilePath -- ^ Full path to game directory.
    ,   language    :: Text     -- ^ Localization language code (e.g. \"en\")
    ,   languageS   :: String   -- ^ Output language code, as String for easy
                                --   manipulation of FilePaths.  
    ,   gameVersion :: Text     -- ^ Version of the game (e.g. \"1.22\")
    ,   gameL10n    :: L10n     -- ^ Game localization table. See "Yaml" for
                                --   the definition of this type.  
    ,   langs       :: [Lang]   -- ^ Preferential list of output languages.
                                --   Currently only \"en\" is supported.
    ,   settingsFile :: FilePath -- ^ Path to @settings.yaml@.
    ,   clargs      :: [CLArgs] -- ^ Command line arguments.
    ,   filesToProcess :: [FilePath] -- ^ List of files being processed.
    }

-- | Set the game localization table. Used by "Settings".
setGameL10n :: Settings -> L10n -> Settings
setGameL10n settings l10n = settings { gameL10n = l10n }

-- | Output monad.
type PP g = StateT (GameData g) (Reader (GameState g)) -- equal to PPT g Identity a
-- | Transformer version of 'PP'. All statement handlers should be in it.
type PPT g m = StateT (GameData g) (ReaderT (GameState g) m)

-- TODO: generalize
-- | Convert a 'PP' action wrapping errors (the @'Either' e@ monad) into a 'PP'
-- action returning 'Either'.
hoistErrors :: Monad m => PPT g (Either e) a -> PPT g m (Either e a)
hoistErrors (StateT rd) =
    StateT $ \settings ->
        ReaderT $ \st -> case runReaderT (rd settings) st of
            Left err -> return (Left err, settings)
            Right (res, settings') -> return (Right res, settings')

-- | Convert a 'PP' action wrapping exceptions (the @'ExceptT' e m@ monad, for
-- some monad @m@) into a 'PP' action returning 'Either'.
hoistExceptions :: Monad m => PPT g (ExceptT e m) a -> PPT g m (Either e a)
hoistExceptions (StateT rd) =
    StateT $ \settings ->
        ReaderT $ \st -> do
            result <- runExceptT (runReaderT (rd settings) st)
            case result of
                Left e -> return (Left e, settings)
                Right (r, settings') -> return (Right r, settings')

-- | Increase current indentation by 1 for the given action. If there is no
-- current indentation, set it to 1.
indentUp :: (IsGameState (GameState g), Monad m) => PPT g m a -> PPT g m a
indentUp go = do
    mindent <- asks currentIndent
    let mindent' = Just (maybe 1 succ mindent)
    local (modifyCurrentIndent mindent') go

-- | Decrease current indent level by 1 for the given action. Ordinarily,
-- indentation is decreased simply by popping up the call stack (increasing it
-- is done via the 'Control.Monad.Reader.MonadReader' method 'local'); use this
-- where a level of indentation should be skipped.
indentDown :: (IsGameState (GameState g), Monad m) => PPT g m a -> PPT g m a
indentDown go = do
    mindent <- asks currentIndent
    let mindent' = Just (maybe 0 pred mindent)
    local (modifyCurrentIndent mindent') go

-- | Pass the current indent level to the action. If there is no current indent
-- level, set it to 1.
withCurrentIndent ::
    (IsGameState (GameState g), Monad m) => (Int -> PPT g m a) -> PPT g m a
withCurrentIndent = withCurrentIndentBaseline 1

-- | Pass the current indent level to the action. If there is no current indent
-- level, set it to 0.
withCurrentIndentZero ::
    (IsGameState (GameState g), Monad m) => (Int -> PPT g m a) -> PPT g m a
withCurrentIndentZero = withCurrentIndentBaseline 0

-- | Common implementation for 'withCurrentIndent' and 'withCurrentIndentZero'.
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

-- | Bundle the result of an action with the current indentation level.
alsoIndent ::
    (IsGameState (GameState g), Monad m) => PPT g m a -> PPT g m (Int, a)
alsoIndent mx = withCurrentIndent $ \i -> mx >>= \x -> return (i,x)
-- | Bundle a value with the current indentation level.
alsoIndent' :: (IsGameState (GameState g), Monad m) => a -> PPT g m (Int, a)
alsoIndent' x = withCurrentIndent $ \i -> return (i,x)

-- | Get the current game language.
getCurrentLang :: (IsGameData (GameData g), Monad m) => PPT g m L10nLang
getCurrentLang = HM.lookupDefault HM.empty <$> gets (language . getSettings) <*> gets (gameL10n . getSettings)

-- | Get the localization string for a given key. If it doesn't exist, use the
-- key itself.
getGameL10n :: (IsGameData (GameData g), Monad m) => Text -> PPT g m Text
getGameL10n key = content <$> HM.lookupDefault (LocEntry 0 key) key <$> getCurrentLang

-- | Get the localization string for a given key. If it doesn't exist, use the
-- given default (the first argument) instead.
getGameL10nDefault :: (IsGameData (GameData g), Monad m) => Text -> Text -> PPT g m Text
getGameL10nDefault def key = content <$> HM.lookupDefault (LocEntry 0 def) key <$> getCurrentLang

-- | Get the localization string for a given key, if it exists.
getGameL10nIfPresent :: (IsGameData (GameData g), Monad m) => Text -> PPT g m (Maybe Text)
getGameL10nIfPresent key = fmap content <$> HM.lookup key <$> getCurrentLang

-- | Pass the current file to the action. If there is no current file, set it
-- to "(unknown)".
withCurrentFile :: (IsGameState (GameState g), Monad m) => (String -> PPT g m a) -> PPT g m a
withCurrentFile go = do
    mfile <- asks currentFile
    local (\s -> if isNothing mfile
                    then modifyCurrentFile (Just "(unknown)") s
                    else s)
          -- fromJust guaranteed to succeed
          (go . fromJust =<< asks currentFile)

-- | Set the current file for the action.
setCurrentFile :: (IsGameState (GameState g), Monad m) => String -> PPT g m a -> PPT g m a
setCurrentFile f = local (modifyCurrentFile (Just f))

-- | Get the list of output languages.
getLangs :: (IsGameData (GameData g), Monad m) => PPT g m [Lang]
getLangs = gets (langs . getSettings)

-- Misc. utilities

-- | As 'Data.List.unfoldr', but argument is monadic.
unfoldM :: Monad m => (a -> m (Maybe (b, a))) -> a -> m [b]
unfoldM f = go where
    go x = do
        res <- f x
        case res of
            Nothing -> return []
            Just (next, x') -> do
                rest <- go x'
                return (next:rest)

-- | Extract the embedded monadic value from a ReaderT.
fromReaderT :: ReaderT r m a -> Reader r (m a)
fromReaderT mx = runReaderT mx <$> ask

-- | Embed a monadic value in a ReaderT.
toReaderT :: Reader r (m a) -> ReaderT r m a
toReaderT mx = ReaderT (runIdentity . runReaderT mx)

-- | As 'Data.List.concatMap', but generalized to arbitrary Traversables and
-- the function passed in is monadic.
concatMapM :: (Monad m, Traversable t, Monoid (t b)) => (a -> m (t b)) -> t a -> m (t b)
concatMapM f xs = liftM fold . mapM f $ xs

-- | Find the last element of a list and chop it off, returning it along with
-- the rest of the list.
unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = Just (case unsnoc' [] xs of (xs', x) -> (reverse xs', x)) where
    unsnoc' _   [] = error "impossible: unsnoc' _ []"
    unsnoc' acc [x] = (acc, x)
    unsnoc' acc (x:xs) = unsnoc' (x:acc) xs

-- | Find the last element of a list.
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

-- | Find the nth element of a list (zero-based).
safeIndex :: Int -> [a] -> Maybe a
safeIndex _ [] = Nothing
safeIndex n _ | n < 0 = Nothing
safeIndex 0 (x:_) = Just x
safeIndex n (_:xs) = safeIndex (pred n) xs
