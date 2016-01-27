{-# LANGUAGE OverloadedStrings #-}
module EU4.Decisions (
        processDecisionGroup
    ) where

import Debug.Trace

import Control.Monad.Identity
import Control.Monad.Reader

import Data.List
import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T

import Abstract
import Doc
import Messages
import SettingsTypes
import EU4.Common

-- Object that accumulates info about a decision.
data Decision = Decision
    {   dec_name :: Text
    ,   dec_name_loc :: Text
    ,   dec_text :: Maybe Text
    ,   dec_potential :: GenericScript
    ,   dec_allow :: GenericScript
    ,   dec_effect :: GenericScript
    ,   dec_ai_will_do :: Maybe AIWillDo
    } deriving (Show)
-- Starts off Nothing/empty everywhere, except name (will get filled in immediately).
newDecision = Decision undefined undefined Nothing [] [] [] Nothing

processDecisionGroup :: GenericStatement -> PP EU4 (Either Text Doc)
processDecisionGroup (Statement (GenericLhs left) rhs)
    | left `elem` ["country_decisions", "religion_decisions"]
    = case rhs of
        CompoundRhs scr -> fromReaderT $ do
            decs <- mapM (toReaderT . processDecision) scr
            return . mconcat . intersperse (mconcat [line, line]) $ decs
        _ -> return $ Left "unrecognized form for decision block (RHS)"
processDecisionGroup _ = return $ Left "unrecognized form for decision block (LHS)"

processDecision :: GenericStatement -> PP EU4 (Either Text Doc)
processDecision (Statement (GenericLhs decName) rhs) = case rhs of
    CompoundRhs parts -> do
        decName_loc <- getGameL10n (decName <> "_title")
        decText <- getGameL10nIfPresent (decName <> "_desc")
        dec <- foldM decisionAddSection
                     newDecision { dec_name = decName
                                 , dec_name_loc = decName_loc
                                 , dec_text = decText }
                     parts
        Right <$> pp_decision dec
    _ -> return $ Left "unrecognized form for decision (RHS)"
processDecision _ = return $ Left "unrecognized form for decision (LHS)"

decisionAddSection :: Decision -> GenericStatement -> PP extra Decision
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

pp_decision :: Decision -> PP EU4 Doc
pp_decision dec = do
    version <- asks gameVersion
    pot_pp'd    <- pp_script (dec_potential dec)
    allow_pp'd  <- pp_script (dec_allow dec)
    effect_pp'd <- pp_script (dec_effect dec)
    mawd_pp'd    <- mapM ((imsg2doc =<<) . ppAiWillDo) (dec_ai_will_do dec)
    let name = strictText (dec_name dec)
    return . mconcat $
        ["<section begin=", name, "/>", line
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
        ["}}", line
        ,"<section end=", name, "/>"
        ]
