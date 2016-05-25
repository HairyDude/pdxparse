{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, LambdaCase #-}
module EU4.Decisions (
        parseEU4Decisions
    ,   writeEU4Decisions
    ) where

import Debug.Trace

import Control.Arrow ((&&&))
import Control.Monad.Except
import Control.Monad.State

import Data.Maybe
import Data.Monoid

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

import Abstract
import Doc
import FileIO
import Messages
import QQ
import SettingsTypes
import EU4.Common

-- Starts off Nothing/empty everywhere, except name (will get filled in immediately).
newDecision :: EU4Decision
newDecision = EU4Decision undefined undefined Nothing [] [] [] Nothing Nothing

parseEU4Decisions :: Monad m => HashMap String GenericScript -> PPT m (HashMap Text EU4Decision)
parseEU4Decisions scripts = do
    tryParse <- hoistExceptions . flip HM.traverseWithKey scripts $ \f script ->
                    setCurrentFile f (concat <$> mapM parseEU4DecisionGroup script)
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing decisions: " ++ T.unpack err
            return HM.empty
        Right files -> fmap (HM.unions . HM.elems) . flip HM.traverseWithKey files $
            \sourceFile edecs ->
                    fmap (HM.fromList . map (dec_name &&& id) . catMaybes)
                        . forM edecs $ \case
                Left err -> do
                    traceM $ "Error parsing " ++ sourceFile
                             ++ ": " ++ T.unpack err
                    return Nothing
                Right dec -> return (Just dec)

--parseEU4Event :: MonadError Text m => FilePath -> GenericStatement -> PPT m (Either Text (Maybe EU4Event))

parseEU4DecisionGroup :: Monad m => GenericStatement -> PPT (ExceptT Text m) [Either Text EU4Decision]
parseEU4DecisionGroup [pdx| $left = @scr |]
    | left `elem` ["country_decisions", "religion_decisions"]
    = forM scr $ \stmt -> (Right <$> parseEU4Decision stmt)
                            `catchError` (return . Left)
    | otherwise = throwError "unrecognized form for decision block (LHS)"
parseEU4DecisionGroup [pdx| $_ = %_ |]
    = throwError "unrecognized form for decision block (RHS)"
parseEU4DecisionGroup _ = throwError "unrecognized form for decision block (LHS)"

parseEU4Decision :: Monad m => GenericStatement -> PPT (ExceptT Text m) EU4Decision
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

decisionAddSection :: Monad m => EU4Decision -> GenericStatement -> PPT m EU4Decision
decisionAddSection dec [pdx| $sectname = @scr |]
    = case sectname of
        "potential" -> return dec { dec_potential = scr }
        "allow" -> return dec { dec_allow = scr }
        "effect" -> return dec { dec_effect = scr }
        "ai_will_do" -> return dec { dec_ai_will_do = Just (aiWillDo scr) }
        _ -> trace ("warning: unrecognized decision section: " ++ T.unpack sectname) $
             return dec
decisionAddSection dec (Statement (GenericLhs "major") OpEq _)
        = return dec -- currently no field in the template for this
decisionAddSection dec (Statement (GenericLhs "ai_importance") OpEq _)
        = trace "notice: ai_importance not yet implemented" $
          return dec
decisionAddSection dec stmt = trace ("warning: unrecognized decision section: " ++ show stmt) $
                              return dec

writeEU4Decisions :: PPT IO ()
writeEU4Decisions = do
    gdata <- gets game
    case gdata of
        GameEU4 { eu4data = EU4Data { eu4decisions = decisions } } ->
            writeFeatures "decisions"
                          pathedDecisions
                          pp_decision
            where
                pathedDecisions :: [Feature EU4Decision]
                pathedDecisions = map (\dec -> Feature {
                                                featurePath = dec_path dec
                                            ,   featureId = Just (dec_name dec)
                                            ,   theFeature = Right dec })
                                      (HM.elems decisions)
        GameUnknown -> error "writeEU4Decisions: unknown game!"

pp_decision :: Monad m => EU4Decision -> PPT m Doc
pp_decision dec = do
    version <- gets gameVersion
    pot_pp'd    <- scope EU4Country (pp_script (dec_potential dec))
    allow_pp'd  <- scope EU4Country (pp_script (dec_allow dec))
    effect_pp'd <- scope EU4Country (pp_script (dec_effect dec))
    mawd_pp'd    <- mapM ((imsg2doc =<<) . ppAiWillDo) (dec_ai_will_do dec)
    let name = dec_name dec
        nameD = strictText name
    name_loc <- getGameL10n (name <> "_title")
    return . mconcat $
        ["<section begin=", nameD, "/>"
        ,"{{Decision", line
        ,"| version = ", strictText version, line
        ,"| decision_name = ", strictText name_loc, line
        ,maybe mempty
               (\txt -> mconcat ["| decision_text = ", strictText txt, line])
               (dec_text dec) 
        ,"| potential = ", line, pot_pp'd, line
        ,"| allow = ", line, allow_pp'd, line
        ,"| effect = ", line, effect_pp'd, line
        ] ++
        flip (maybe []) mawd_pp'd (\awd_pp'd ->
            ["| comment = AI decision factors:", line
            ,awd_pp'd, line]) ++
        ["}}" -- no line, causes unwanted extra space
        ,"<section end=", nameD, "/>"
        ]
