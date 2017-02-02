{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, LambdaCase #-}
module EU4.Decisions (
        parseEU4Decisions
    ,   writeEU4Decisions
    ) where

import Debug.Trace (trace, traceM)

import Control.Arrow ((&&&))
import Control.Monad (foldM, forM)
import Control.Monad.Except (ExceptT (..), MonadError (..))
import Control.Monad.State (MonadState (..), gets)
import Control.Monad.Trans (MonadIO (..))

import Data.Maybe (catMaybes)
import Data.Monoid ((<>))

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract -- everything
import qualified Doc
import FileIO (Feature (..), writeFeatures)
import Messages -- everything
import QQ (pdx)
import SettingsTypes ( PPT, Settings (..), Game (..)
                     , IsGame (..), IsGameData (..), IsGameState (..)
                     , getGameL10n, getGameL10nIfPresent
                     , setCurrentFile, withCurrentFile
                     , hoistExceptions)
import EU4.Common -- everything

-- Starts off Nothing/empty everywhere, except name (will get filled in immediately).
newDecision :: EU4Decision
newDecision = EU4Decision undefined undefined Nothing [] [] [] Nothing Nothing

parseEU4Decisions :: (IsGameData (GameData g),
                      IsGameState (GameState g),
                      Monad m) =>
    HashMap String GenericScript -> PPT g m (HashMap Text EU4Decision)
parseEU4Decisions scripts = do
    tryParse <- hoistExceptions . flip HM.traverseWithKey scripts $ \f script ->
                    setCurrentFile f (concat <$> mapM parseEU4DecisionGroup script)
    case tryParse of
        Left err -> do
            -- TODO: use logging instead of trace
            traceM $ "Completely failed parsing decisions: " ++ T.unpack err
            return HM.empty
        Right files -> fmap (HM.unions . HM.elems) . flip HM.traverseWithKey files $
            \sourceFile edecs ->
                    fmap (HM.fromList . map (dec_name &&& id) . catMaybes)
                        . forM edecs $ \case
                Left err -> do
                    -- TODO: use logging instead of trace
                    traceM $ "Error parsing " ++ sourceFile
                             ++ ": " ++ T.unpack err
                    return Nothing
                Right dec -> return (Just dec)

--parseEU4Event :: MonadError Text m => FilePath -> GenericStatement -> PPT g m (Either Text (Maybe EU4Event))

parseEU4DecisionGroup :: (IsGameData (GameData g),
                          IsGameState (GameState g),
                          Monad m) =>
    GenericStatement -> PPT g (ExceptT Text m) [Either Text EU4Decision]
parseEU4DecisionGroup [pdx| $left = @scr |]
    | left `elem` ["country_decisions", "religion_decisions"]
    = forM scr $ \stmt -> (Right <$> parseEU4Decision stmt)
                            `catchError` (return . Left)
    | otherwise = throwError "unrecognized form for decision block (LHS)"
parseEU4DecisionGroup [pdx| $_ = %_ |]
    = throwError "unrecognized form for decision block (RHS)"
parseEU4DecisionGroup _ = throwError "unrecognized form for decision block (LHS)"

parseEU4Decision :: (IsGameData (GameData g),
                     IsGameState (GameState g),
                     Monad m) =>
    GenericStatement -> PPT g (ExceptT Text m) EU4Decision
parseEU4Decision [pdx| $decName = %rhs |] = case rhs of
    CompoundRhs parts -> do
        decName_loc <- getGameL10n (decName <> "_title")
        decText <- getGameL10nIfPresent (decName <> "_desc")
        withCurrentFile $ \sourcePath ->
            foldM decisionAddSection
                  newDecision { dec_name = decName
                              , dec_name_loc = decName_loc
                              , dec_text = decText
                              , dec_path = Just sourcePath }
                  parts
    _ -> throwError "unrecognized form for decision (RHS)"
parseEU4Decision _ = throwError "unrecognized form for decision (LHS)"

decisionAddSection :: (IsGameState (GameState g), Monad m) =>
    EU4Decision -> GenericStatement -> PPT g m EU4Decision
decisionAddSection dec [pdx| potential        = @scr |] = return dec { dec_potential = scr }
decisionAddSection dec [pdx| allow            = @scr |] = return dec { dec_allow = scr }
decisionAddSection dec [pdx| effect           = @scr |] = return dec { dec_effect = scr }
decisionAddSection dec [pdx| ai_will_do       = @scr |] = return dec { dec_ai_will_do = Just (aiWillDo scr) }
decisionAddSection dec [pdx| do_not_integrate = %_ |]   = return dec -- maybe mention this in AI notes
decisionAddSection dec [pdx| do_not_core      = %_ |]   = return dec -- maybe mention this in AI notes
decisionAddSection dec (Statement (GenericLhs "major") OpEq _)
        = return dec -- currently no field in the template for this
decisionAddSection dec (Statement (GenericLhs "ai_importance") OpEq _)
            -- TODO: use logging instead of trace
        = trace "notice: ai_importance not yet implemented" $
          return dec
decisionAddSection dec stmt = withCurrentFile $ \file -> do
    -- TODO: use logging instead of trace
    traceM ("warning: unrecognized decision section in " ++ file ++ ": " ++ show stmt)
    return dec

writeEU4Decisions :: (EU4Info g, MonadIO m) => PPT g m ()
writeEU4Decisions = do
    decisions <- getDecisions
    let pathedDecisions :: [Feature EU4Decision]
        pathedDecisions = map (\dec -> Feature {
                                        featurePath = dec_path dec
                                    ,   featureId = Just (dec_name dec)
                                    ,   theFeature = Right dec })
                              (HM.elems decisions)
    writeFeatures "decisions"
                  pathedDecisions
                  pp_decision

pp_decision :: (EU4Info g, Monad m) => EU4Decision -> PPT g m Doc
pp_decision dec = do
    version <- gets (gameVersion . getSettings)
    pot_pp'd    <- scope EU4Country (pp_script (dec_potential dec))
    allow_pp'd  <- scope EU4Country (pp_script (dec_allow dec))
    effect_pp'd <- scope EU4Country (pp_script (dec_effect dec))
    mawd_pp'd    <- mapM ((imsg2doc =<<) . ppAiWillDo) (dec_ai_will_do dec)
    let name = dec_name dec
        nameD = Doc.strictText name
    name_loc <- getGameL10n (name <> "_title")
    return . mconcat $
        ["<section begin=", nameD, "/>"
        ,"{{Decision", PP.line
        ,"| version = ", Doc.strictText version, PP.line
        ,"| decision_name = ", Doc.strictText name_loc, PP.line
        ,maybe mempty
               (\txt -> mconcat ["| decision_text = ", Doc.strictText txt, PP.line])
               (dec_text dec) 
        ,"| potential = ", PP.line, pot_pp'd, PP.line
        ,"| allow = ", PP.line, allow_pp'd, PP.line
        ,"| effect = ", PP.line, effect_pp'd, PP.line
        ] ++
        flip (maybe []) mawd_pp'd (\awd_pp'd ->
            ["| comment = AI decision factors:", PP.line
            ,awd_pp'd, PP.line]) ++
        ["}}" -- no line, causes unwanted extra space
        ,"<section end=", nameD, "/>"
        ]
