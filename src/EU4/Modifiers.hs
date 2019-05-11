{-|
Module      : EU4.Modifiers
Description : Country, ruler, province and opinion modifiers
-}
module EU4.Modifiers (
        parseEU4Modifiers, writeEU4Modifiers
    ,   parseEU4OpinionModifiers, writeEU4OpinionModifiers
    ) where

import Control.Arrow ((&&&))
import Control.Monad (foldM, forM, join)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Trans (MonadIO (..))

import Data.Maybe (fromJust, catMaybes)
import Data.Monoid ((<>))

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Text as T

import Abstract -- everything
import QQ (pdx)
import SettingsTypes ( PPT{-, Settings (..)-}{-, Game (..)-}
                     {-, IsGame (..)-}, IsGameData (..), IsGameState (..), GameState (..)
                     {-, getGameL10n-}, getGameL10nIfPresent
                     , setCurrentFile, withCurrentFile
                     , hoistErrors, hoistExceptions)
import EU4.Types -- everything

import Debug.Trace (trace, traceM)

parseEU4Modifiers :: (IsGameData (GameData g), IsGameState (GameState g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text EU4Modifier)
parseEU4Modifiers scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr ->
                setCurrentFile sourceFile $ mapM parseEU4Modifier scr)
            scripts
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing modifiers: " ++ T.unpack err
            return HM.empty
        Right modifiersFilesOrErrors ->
            flip HM.traverseWithKey modifiersFilesOrErrors $ \sourceFile emods ->
                fmap (mkModMap . catMaybes) . forM emods $ \case
                    Left err -> do
                        traceM $ "Error parsing modifiers in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right mmod -> return mmod
                where mkModMap :: [EU4Modifier] -> HashMap Text EU4Modifier
                      mkModMap = HM.fromList . map (modName &&& id)

parseEU4Modifier :: (IsGameData (GameData g), IsGameState (GameState g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe EU4Modifier))
parseEU4Modifier [pdx| $modid = @effects |]
    = withCurrentFile $ \file -> do
        mlocid <- getGameL10nIfPresent modid
        return . Right . Just $ EU4Modifier {
                modName = modid
            ,   modLocName = mlocid
            ,   modPath = file
            ,   modEffects = effects
            }
parseEU4Modifier _ = withCurrentFile $ \file ->
    throwError ("unrecognised form for modifier in " <> T.pack file)

-- | Present the parsed modifiers as wiki text and write them to the
-- appropriate files.
writeEU4Modifiers :: (EU4Info g, MonadError Text m, MonadIO m) => PPT g m ()
writeEU4Modifiers = throwError "Sorry, writing all modifiers currently not supported."

parseEU4OpinionModifiers :: (IsGameState (GameState g), IsGameData (GameData g), Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text EU4OpinionModifier)
parseEU4OpinionModifiers scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr ->
                setCurrentFile sourceFile $ mapM parseEU4OpinionModifier scr)
            scripts
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing opinion modifiers: " ++ T.unpack err
            return HM.empty
        Right modifiersFilesOrErrors ->
            flip HM.traverseWithKey modifiersFilesOrErrors $ \sourceFile emods ->
                fmap (mkModMap . catMaybes) . forM emods $ \case
                    Left err -> do
                        traceM $ "Error parsing modifiers in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right mmod -> return mmod
                where mkModMap :: [EU4OpinionModifier] -> HashMap Text EU4OpinionModifier
                      mkModMap = HM.fromList . map (omodName &&& id)

newEU4OpinionModifier id locid path = EU4OpinionModifier id locid path Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Parse a statement in an opinion modifiers file. Some statements aren't
-- modifiers; for those, and for any obvious errors, return Right Nothing.
parseEU4OpinionModifier :: (IsGameState (GameState g), IsGameData (GameData g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe EU4OpinionModifier))
parseEU4OpinionModifier (StatementBare _) = throwError "bare statement at top level"
parseEU4OpinionModifier [pdx| %left = %right |] = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> throwError "internal error: custom lhs"
        IntLhs _ -> throwError "int lhs at top level"
        AtLhs _ -> return (Right Nothing)
        GenericLhs id [] -> withCurrentFile $ \file -> do
            locid <- getGameL10nIfPresent id
            mmod <- hoistErrors $ foldM opinionModifierAddSection
                                        (Just (newEU4OpinionModifier id locid file))
                                        parts
            case mmod of
                Left err -> return (Left err)
                Right Nothing -> return (Right Nothing)
                Right (Just mod) -> withCurrentFile $ \file ->
                    return (Right (Just mod ))
        _ -> throwError "unrecognized form for opinion modifier"
    _ -> throwError "unrecognized form for opinion modifier"
parseEU4OpinionModifier _ = withCurrentFile $ \file ->
    throwError ("unrecognised form for opinion modifier in " <> T.pack file)

-- | Interpret one section of an opinion modifier. If understood, add it to the
-- event data. If not understood, throw an exception.
opinionModifierAddSection :: (IsGameState (GameState g), MonadError Text m) =>
    Maybe EU4OpinionModifier -> GenericStatement -> PPT g m (Maybe EU4OpinionModifier)
opinionModifierAddSection Nothing _ = return Nothing
opinionModifierAddSection mmod stmt
    = sequence (opinionModifierAddSection' <$> mmod <*> pure stmt)
    where
        opinionModifierAddSection' mod stmt@[pdx| opinion = !rhs |]
            = return (mod { omodOpinion = Just rhs })
        opinionModifierAddSection' mod stmt@[pdx| max = !rhs |]
            = return (mod { omodMax = Just rhs })
        opinionModifierAddSection' mod stmt@[pdx| min = !rhs |]
            = return (mod { omodMin = Just rhs })
        opinionModifierAddSection' mod stmt@[pdx| yearly_decay = !rhs |]
            = return (mod { omodYearlyDecay = Just rhs })
        opinionModifierAddSection' mod stmt@[pdx| months = !rhs |]
            = return (mod { omodMonths = Just rhs })
        opinionModifierAddSection' mod stmt@[pdx| years = !rhs |]
            = return (mod { omodYears = Just rhs })
        opinionModifierAddSection' mod stmt@[pdx| max_vassal = !rhs |]
            = return (mod { omodMaxVassal = Just rhs })
        opinionModifierAddSection' mod stmt@[pdx| max_in_other_direction = !rhs |]
            = return (mod { omodMaxInOtherDirection = Just rhs })
        opinionModifierAddSection' mod [pdx| $other = %_ |]
            = trace ("unknown opinion modifier section: " ++ T.unpack other) $ return mod
        opinionModifierAddSection' mod _
            = trace ("unrecognised form for opinion modifier section") $ return mod

writeEU4OpinionModifiers :: (IsGameState (GameState g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe EU4OpinionModifier))
writeEU4OpinionModifiers _ = throwError "Sorry, writing all opinion modifiers not yet supported."
