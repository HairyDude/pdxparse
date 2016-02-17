{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module EU4.Decisions (
        Decision (..)
    ) where

import Debug.Trace

import Control.Arrow (first)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader

import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T

import System.FilePath (FilePath, (</>))

import Abstract
import Doc
import Messages
import SettingsTypes
import EU4.Common
import EU4.Feature

-- Object that accumulates info about a decision.
data Decision = Decision
    {   dec_name :: Text
    ,   dec_name_loc :: Text
    ,   dec_path :: FilePath
    ,   dec_text :: Maybe Text
    ,   dec_potential :: GenericScript
    ,   dec_allow :: GenericScript
    ,   dec_effect :: GenericScript
    ,   dec_ai_will_do :: Maybe AIWillDo
    } deriving (Show)
-- Starts off Nothing/empty everywhere, except name (will get filled in immediately).
newDecision = Decision undefined undefined undefined Nothing [] [] [] Nothing

instance Feature EU4 Decision where
    emptyFeature = newDecision
    featureDirectory _ = "decisions"
    featurePath = dec_path
    readFeatures = readDecisionGroup
    loadFeature = loadDecision
    getFeatures _ eu4 = decisions eu4
    ppFeature = pp_decision

readDecisionGroup :: MonadError Text m => GenericStatement -> PPT EU4 m [Either Text (Maybe Decision)]
readDecisionGroup (Statement (GenericLhs left) rhs)
    | left `elem` ["country_decisions", "religion_decisions"]
    = case rhs of
        CompoundRhs scr -> forM scr $ \stmt ->
            (Right . Just <$> processDecision stmt)
                `catchError` \e -> return (Left e)
        _ -> throwError "unrecognized form for decision block (RHS)"
readDecisionGroup _ = throwError "unrecognized form for decision block (LHS)"

processDecision :: MonadError Text m => GenericStatement -> PPT EU4 m Decision
processDecision (Statement (GenericLhs decName) rhs) = case rhs of
    CompoundRhs parts -> withCurrentFile $ \file -> do
        decName_loc <- getGameL10n (decName <> "_title")
        decText <- getGameL10nIfPresent (decName <> "_desc")
        foldM decisionAddSection
              newDecision { dec_name = decName
                          , dec_name_loc = decName_loc
                          , dec_path = file </> T.unpack decName
                          , dec_text = decText }
              parts
    _ -> throwError "unrecognized form for decision (RHS)"
processDecision _ = throwError "unrecognized form for decision (LHS)"

decisionAddSection :: Monad m => Decision -> GenericStatement -> PPT extra m Decision
decisionAddSection dec (Statement (GenericLhs sectname) (CompoundRhs scr))
    = case sectname of
        "potential" -> return dec { dec_potential = scr }
        "allow" -> return dec { dec_allow = scr }
        "effect" -> return dec { dec_effect = scr }
        "ai_will_do" -> return dec { dec_ai_will_do = Just (aiWillDo scr) }
        _ -> trace ("warning: unrecognized decision section: " ++ T.unpack sectname) $
             return dec
decisionAddSection dec (Statement (GenericLhs "major") _)
        = return dec -- currently no field in the template for this
decisionAddSection dec (Statement (GenericLhs "ai_importance") _)
        = trace "notice: ai_importance not yet implemented" $
          return dec
decisionAddSection dec stmt = trace ("warning: unrecognized decision section: " ++ show stmt) $
                              return dec

loadDecision :: Decision -> EU4 -> EU4
loadDecision dec tab@EU4 { decisions = decs }
    = tab { decisions = HM.insert (dec_name dec) dec decs }

pp_decision :: Monad m => Decision -> PPT EU4 m Doc
pp_decision dec = do
    version <- asks gameVersion
    pot_pp'd    <- scope Country (pp_script (dec_potential dec))
    allow_pp'd  <- scope Country (pp_script (dec_allow dec))
    effect_pp'd <- scope Country (pp_script (dec_effect dec))
    mawd_pp'd    <- mapM ((imsg2doc =<<) . ppAiWillDo) (dec_ai_will_do dec)
    let name = strictText (dec_name dec)
    return . mconcat $
        ["<section begin=", name, "/>"
        ,"{{Decision", line
        ,"| version = ", strictText version, line
        ,"| decision_name = ", strictText (dec_name_loc dec), line
        ,maybe mempty
               (\text -> mconcat ["| decision_text = ", strictText text, line])
               (dec_text dec) 
        ,"| potential = ", line, pot_pp'd, line
        ,"| allow = ", line, allow_pp'd, line
        ,"| effect = ", line, effect_pp'd, line
        ] ++
        flip (maybe []) mawd_pp'd (\awd_pp'd ->
            ["| comment = AI decision factors:", line
            ,awd_pp'd, line]) ++
        ["}}" -- no line, causes unwanted extra space
        ,"<section end=", name, "/>"
        ]
