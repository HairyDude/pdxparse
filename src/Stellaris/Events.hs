{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, QuasiQuotes, LambdaCase #-}
module Stellaris.Events (
        parseStellarisEvents
    ,   writeStellarisEvents
    ) where

import Prelude hiding (mapM)

import Debug.Trace

import Control.Arrow ((&&&))
import Control.Monad.Except
import Control.Monad.State hiding (mapM)

import Data.List (intersperse)
import Data.Maybe
import Data.Monoid

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>), (</>))

import Abstract
import Stellaris.Common
import FileIO
import Messages
import QQ
import SettingsTypes

-- Starts off Nothing everywhere.
newStellarisEvent :: StellarisScope -> StellarisEvent
newStellarisEvent escope = StellarisEvent Nothing Nothing Nothing Nothing escope Nothing Nothing Nothing Nothing Nothing Nothing
newStellarisOption :: StellarisOption
newStellarisOption = StellarisOption Nothing Nothing Nothing Nothing

-- Parse events and return them.
parseStellarisEvents :: Monad m => HashMap String GenericScript -> PPT m (HashMap Text StellarisEvent)
parseStellarisEvents scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr ->
                setCurrentFile sourceFile $ mapM parseStellarisEvent scr)
            scripts 
    case tryParse of
        Left err -> do
            traceM $ "Completely failed parsing events: " ++ T.unpack err
            return HM.empty
        Right eventsFilesOrErrors ->
            flip HM.traverseWithKey eventsFilesOrErrors $ \sourceFile eevts ->
                fmap (mkEvtMap . catMaybes) . forM eevts $ \case
                    Left err -> do
                        traceM $ "Error parsing events in " ++ sourceFile
                                 ++ ": " ++ T.unpack err
                        return Nothing
                    Right mevt -> return mevt
                where mkEvtMap :: [StellarisEvent] -> HashMap Text StellarisEvent
                      mkEvtMap = HM.fromList . map (fromJust . stevt_id &&& id)
                        -- Events returned from parseEvent are guaranteed to have an id.

writeStellarisEvents :: PPT IO ()
writeStellarisEvents = do
    gdata <- gets game
    case gdata of
        GameStellaris { stdata = StellarisData { stevents = events } } ->
            writeFeatures "events"
                          pathedEvents
                          (\e -> scope (stevt_scope e) $ pp_event e)
            where
                pathedEvents :: [Feature StellarisEvent]
                pathedEvents = map (\evt -> Feature {
                                            featurePath = stevt_path evt
                                        ,   featureId = stevt_id evt
                                        ,   theFeature = Right evt })
                                    (HM.elems events)
        _ -> error "writeStellarisEvents passed wrong game's data!"

-- Parse a statement in an events file. Some statements aren't events; for
-- those, and for any obvious errors, return Nothing.
parseStellarisEvent :: MonadError Text m => GenericStatement -> PPT m (Either Text (Maybe StellarisEvent))
parseStellarisEvent (StatementBare _) = throwError "bare statement at top level"
parseStellarisEvent [pdx| %left = %right |] = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> throwError "internal error: custom lhs"
        IntLhs _ -> throwError "int lhs at top level"
        GenericLhs etype ->
            let mescope = case etype of
                    "country_event" -> Just StellarisCountry
                    "fleet_event" -> Just StellarisFleet
                    "planet_event" -> Just StellarisPlanet
                    "pop_event" -> Just StellarisPop
                    "pop_faction_event" -> Just StellarisPopFaction
                    "ship_event" -> Just StellarisShip
                    "event" -> Just StellarisNoScope
                    _ -> Nothing
            in case mescope of
                Nothing -> throwError $ "unrecognized event type " <> etype
                Just escope -> do
                    mevt <- hoistErrors (foldM eventAddSection (Just (newStellarisEvent escope)) parts)
                    case mevt of
                        Left err -> return (Left err)
                        Right Nothing -> return (Right Nothing)
                        Right (Just evt) -> withCurrentFile $ \file ->
                            let pathedEvt = evt { stevt_path = Just file }
                            in  if isJust (stevt_id pathedEvt)
                                then return (Right (Just pathedEvt))
                                else return (Left $ "error parsing events in " <> T.pack file
                                             <> ": missing event id")

    _ -> return (Right Nothing)
parseStellarisEvent _ = throwError "operator other than ="

eventAddSection :: MonadError Text m => Maybe StellarisEvent -> GenericStatement -> PPT m (Maybe StellarisEvent)
eventAddSection Nothing _ = return Nothing
eventAddSection (Just evt) stmt@[pdx| $label = %rhs |] = withCurrentFile $ \file ->
    Just <$> case label of
        "id" -> case (textRhs rhs, floatRhs rhs) of
            (Just tid, _) -> return evt { stevt_id = Just tid }
            (_, Just nid) -> return evt { stevt_id = Just (T.pack $ show (nid::Int)) }
            _ -> throwError $ "bad id in " <> T.pack file <> ": " <> T.pack (show rhs)
        "title" -> case textRhs rhs of
            Just title -> return evt { stevt_title = Just title }
            _ -> throwError $ "bad title in " <> T.pack file
        "desc" -> let olddescs = stevt_desc evt in case rhs of
            (textRhs -> Just desc) -> return evt { stevt_desc = olddescs ++ [([], desc)] }
            -- research:
            -- 1) text = key
            -- 2) trigger = { conditions }
            CompoundRhs rhs -> 
            _ -> throwError "bad desc"
        "picture" -> case textRhs rhs of
            Just pic -> return evt { stevt_picture = Just pic }
            _ -> throwError "bad picture"
        "trigger" -> case rhs of
            CompoundRhs trigger_script -> case trigger_script of
                [] -> return evt -- empty, treat as if it wasn't there
                _ -> return evt { stevt_trigger = Just trigger_script }
            _ -> throwError "bad event trigger"
        "is_triggered_only" -> case rhs of
            GenericRhs "yes" -> return evt { stevt_is_triggered_only = Just True }
            -- no is the default, so I don't think this is ever used
            GenericRhs "no" -> return evt { stevt_is_triggered_only = Just False }
            _ -> throwError "bad trigger"
        "mean_time_to_happen" -> case rhs of
            CompoundRhs mtth -> return evt { stevt_mean_time_to_happen = Just mtth }
            _ -> throwError "bad MTTH"
        "immediate" -> case rhs of
            CompoundRhs immediate -> return evt { stevt_immediate = Just immediate }
            _ -> throwError "bad immediate section"
        "option" -> case rhs of
            CompoundRhs option -> do
                newStellarisOptions <- addStellarisOption (stevt_options evt) option
                return evt { stevt_options = newStellarisOptions }
            _ -> throwError "bad option"
        "fire_only_once" -> return evt -- do nothing
        "major" -> return evt -- do nothing
        "is_mtth_scaled_to_size" -> return evt -- do nothing (XXX)
        "hide_window" -> case rhs of
            GenericRhs "yes" -> return evt { stevt_hide_window = True }
            GenericRhs "no" -> return evt { stevt_hide_window = False }
            _ -> do
                traceM $ "warning: unrecognized event section: " ++ show stmt
                return evt
        _ -> throwError $ "unrecognized event section in " <> T.pack file <> ": " <> label
eventAddSection evt _ = return evt

addStellarisOption :: Monad m => Maybe [StellarisOption] -> GenericScript -> PPT m (Maybe [StellarisOption])
addStellarisOption Nothing opt = addStellarisOption (Just []) opt
addStellarisOption (Just opts) opt = do
    optn <- foldM optionAddStatement newStellarisOption opt
    return $ Just (opts ++ [optn])

optionAddStatement :: Monad m => StellarisOption -> GenericStatement -> PPT m StellarisOption
optionAddStatement opt stmt@[pdx| $label = %rhs |] =
    case label of
        "name" -> case textRhs rhs of
            Just name -> return $ opt { stopt_name = Just name }
            _ -> error "bad option name"
        "ai_chance" -> case rhs of
            CompoundRhs ai_chance -> return $ opt { stopt_ai_chance = Just ai_chance }
            _ -> error "bad option ai_chance"
        "trigger" -> case rhs of
            CompoundRhs trigger_script -> return $ opt { stopt_trigger = Just trigger_script }
            _ -> error "bad option trigger"
        -- Other statements are just effects.
        _ -> do
            effects_pp'd <- optionAddEffect (stopt_effects opt) stmt
            return $ opt { stopt_effects = effects_pp'd }
optionAddStatement opt stmt = do
    -- Not a GenericLhs - presumably an effect.
    effects_pp'd <- optionAddEffect (stopt_effects opt) stmt
    return $ opt { stopt_effects = effects_pp'd }

optionAddEffect :: Monad m => Maybe GenericScript -> GenericStatement -> PPT m (Maybe GenericScript)
optionAddEffect Nothing stmt = optionAddEffect (Just []) stmt
optionAddEffect (Just effs) stmt = return $ Just (effs ++ [stmt])

-- Pretty-print an event, or fail.
pp_event :: forall m. MonadError Text m => StellarisEvent -> PPT m Doc
pp_event evt = case (stevt_id evt
                    ,stevt_title evt
                    ,stevt_options evt) of
    (Just eid, Just title, Just options)
        | (isJust (stevt_is_triggered_only evt) ||
           isJust (stevt_mean_time_to_happen evt)) -> do
        -- Valid event
        version <- gets gameVersion
        (conditional, options_pp'd) <- pp_options options
        titleLoc <- getGameL10n title
        descLoc <- getGameL10n `mapM` stevt_desc evt
        let evtArg :: Text -> (StellarisEvent -> Maybe a) -> (a -> PPT m Doc) -> PPT m [Doc]
            evtArg fieldname field fmt
                = maybe (return [])
                    (\field_content -> do
                        content_pp'd <- fmt field_content 
                        return
                            ["| ", strictText fieldname, " = "
                            ,line
                            ,content_pp'd
                            ,line])
                    (field evt)
            isTriggeredOnly = fromMaybe False $ stevt_is_triggered_only evt
            evtId = strictText eid
        trigger_pp'd <- evtArg "trigger" stevt_trigger pp_script
        mmtth_pp'd <- mapM pp_mtth (stevt_mean_time_to_happen evt)
        immediate_pp'd <- evtArg "immediate" stevt_immediate pp_script
        return . mconcat $
            ["<section begin=", evtId, "/>", line
            ,"{{Event", line
            ,"| version = ", strictText version, line
            ,"| event_name = ", strictText titleLoc, line
            ] ++
            maybe [] (\desc ->
                        ["| event_text = "
                        ,text . TL.fromStrict . nl2br $ desc
                        ,line])
                      descLoc ++
            -- For triggered only events, mean_time_to_happen is not
            -- really mtth but instead describes weight modifiers, for
            -- scripts that trigger them with a probability based on a
            -- weight (e.g. on_bi_yearly_pulse).
            (if isTriggeredOnly then
                ["| triggered only = (please describe trigger here)",line
                ]
                ++ maybe [] (:[line]) mmtth_pp'd
            else []) ++
            trigger_pp'd ++
            -- mean_time_to_happen is only really mtth if it's *not*
            -- triggered only.
            (if isTriggeredOnly then [] else case mmtth_pp'd of
                Nothing -> []
                Just mtth_pp'd ->
                    ["| mtth = ", line
                    ,mtth_pp'd]) ++
            immediate_pp'd ++
            (if conditional then ["| option conditions = yes", line] else []) ++
            -- option_conditions = no (not implemented yet)
            ["| options = ", options_pp'd, line
            ,"| collapse = yes", line
            ,"}}", line
            ,"<section end=", evtId, "/>"
            ]

    (Nothing, _, _) -> throwError "stevt_id missing"
    (Just eid, Nothing, _) ->
        throwError ("title missing for event id " <> eid)
    (Just eid, _, Nothing) ->
        throwError ("options missing for event id " <> eid)
    (Just eid, _, _) ->
        throwError ("is_triggered_only and mean_time_to_happen missing for event id " <> eid)

pp_options :: MonadError Text m => [StellarisOption] -> PPT m (Bool, Doc)
pp_options opts = do
    let triggered = any (isJust . stopt_trigger) opts
    options_pp'd <- mapM (pp_option triggered) opts
    return (triggered, mconcat . (line:) . intersperse line $ options_pp'd)

pp_option :: MonadError Text m => Bool -> StellarisOption -> PPT m Doc
pp_option triggered opt = do
    optNameLoc <- getGameL10n `mapM` stopt_name opt
    case optNameLoc of
        -- NB: some options have no effect, e.g. start of Peasants' War.
        Just name_loc ->
            let mtrigger = stopt_trigger opt
            in do
                effects_pp'd <- pp_script (fromMaybe [] (stopt_effects opt))
                mtrigger_pp'd <- sequence (pp_script <$> mtrigger)
                return . mconcat $
                    ["{{Option\n"
                    ,"| option_text = ", strictText name_loc, line
                    ,"| effect =", line, effects_pp'd, line]
                    ++ (if triggered then
                            maybe
                                ["| trigger = Always enabled:", line] -- no trigger
                            (\trigger_pp'd ->
                                ["| trigger = Enabled if:", line -- trigger
                                ,trigger_pp'd, line]
                            ) mtrigger_pp'd
                        else [])
                    ++
                    -- 1 = no
                    ["}}"
                    ]
        Nothing -> throwError $ "some required option sections missing - dumping: " <> T.pack (show opt)
