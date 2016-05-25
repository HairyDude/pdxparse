{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, QuasiQuotes, LambdaCase #-}
module EU4.Events (
        parseEU4Events
    ,   writeEU4Events
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
import EU4.Common
import FileIO
import Messages
import QQ
import SettingsTypes

-- Starts off Nothing everywhere.
newEU4Event :: EU4Scope -> EU4Event
newEU4Event escope = EU4Event Nothing Nothing Nothing Nothing escope Nothing Nothing Nothing Nothing Nothing Nothing
newEU4Option :: EU4Option
newEU4Option = EU4Option Nothing Nothing Nothing Nothing

-- Parse events and return them.
parseEU4Events :: Monad m => HashMap String GenericScript -> PPT m (HashMap Text EU4Event)
parseEU4Events scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr ->
                setCurrentFile sourceFile $ mapM parseEU4Event scr)
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
                where mkEvtMap :: [EU4Event] -> HashMap Text EU4Event
                      mkEvtMap = HM.fromList . map (fromJust . eu4evt_id &&& id)
                        -- Events returned from parseEvent are guaranteed to have an id.

writeEU4Events :: PPT IO ()
writeEU4Events = do
    gdata <- gets game
    case gdata of
        GameEU4 { eu4data = EU4Data { eu4events = events } } ->
            writeFeatures "events"
                          pathedEvents
                          pp_event
            where
                pathedEvents :: [Feature EU4Event]
                pathedEvents = map (\evt -> Feature {
                                            featurePath = eu4evt_path evt
                                        ,   featureId = eu4evt_id evt
                                        ,   theFeature = Right evt })
                                    (HM.elems events)
        _ -> error "writeEU4Events passed wrong game's data!"

-- Parse a statement in an events file. Some statements aren't events; for
-- those, and for any obvious errors, return Nothing.
parseEU4Event :: MonadError Text m => GenericStatement -> PPT m (Either Text (Maybe EU4Event))
parseEU4Event (StatementBare _) = throwError "bare statement at top level"
parseEU4Event [pdx| %left = %right |] = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> throwError "internal error: custom lhs"
        IntLhs _ -> throwError "int lhs at top level"
        GenericLhs etype ->
            let mescope = case etype of
                    "country_event" -> Just EU4Country
                    "province_event" -> Just EU4Province
                    _ -> Nothing
            in case mescope of
                Nothing -> throwError $ "unrecognized event type " <> etype
                Just escope -> do
                    mevt <- hoistErrors (foldM eventAddSection (Just (newEU4Event escope)) parts)
                    case mevt of
                        Left err -> return (Left err)
                        Right Nothing -> return (Right Nothing)
                        Right (Just evt) -> withCurrentFile $ \file ->
                            let pathedEvt = evt { eu4evt_path = Just file }
                            in  if isJust (eu4evt_id pathedEvt)
                                then return (Right (Just pathedEvt))
                                else return (Left $ "error parsing events in " <> T.pack file
                                             <> ": missing event id")

    _ -> return (Right Nothing)
parseEU4Event _ = throwError "operator other than ="

eventAddSection :: MonadError Text m => Maybe EU4Event -> GenericStatement -> PPT m (Maybe EU4Event)
eventAddSection Nothing _ = return Nothing
eventAddSection (Just evt) [pdx| $label = %rhs |] = withCurrentFile $ \file ->
    Just <$> case label of
        "id" -> case (textRhs rhs, floatRhs rhs) of
            (Just tid, _) -> return evt { eu4evt_id = Just tid }
            (_, Just nid) -> return evt { eu4evt_id = Just (T.pack $ show (nid::Int)) }
            _ -> throwError $ "bad id in " <> T.pack file <> ": " <> T.pack (show rhs)
        "title" -> case textRhs rhs of
            Just title -> return evt { eu4evt_title = Just title }
            _ -> throwError $ "bad title in " <> T.pack file
        "desc" -> case textRhs rhs of
            Just desc -> return evt { eu4evt_desc = Just desc }
            _ -> throwError "bad desc"
        "picture" -> case textRhs rhs of
            Just pic -> return evt { eu4evt_picture = Just pic }
            _ -> throwError "bad picture"
        "trigger" -> case rhs of
            CompoundRhs trigger_script -> case trigger_script of
                [] -> return evt -- empty, treat as if it wasn't there
                _ -> return evt { eu4evt_trigger = Just trigger_script }
            _ -> throwError "bad event trigger"
        "is_triggered_only" -> case rhs of
            GenericRhs "yes" -> return evt { eu4evt_is_triggered_only = Just True }
            -- no is the default, so I don't think this is ever used
            GenericRhs "no" -> return evt { eu4evt_is_triggered_only = Just False }
            _ -> throwError "bad trigger"
        "mean_time_to_happen" -> case rhs of
            CompoundRhs mtth -> return evt { eu4evt_mean_time_to_happen = Just mtth }
            _ -> throwError "bad MTTH"
        "immediate" -> case rhs of
            CompoundRhs immediate -> return evt { eu4evt_immediate = Just immediate }
            _ -> throwError "bad immediate section"
        "option" -> case rhs of
            CompoundRhs option -> do
                newEU4Options <- addEU4Option (eu4evt_options evt) option
                return evt { eu4evt_options = newEU4Options }
            _ -> throwError "bad option"
        "fire_only_once" -> return evt -- do nothing
        "major" -> return evt -- do nothing
        "is_mtth_scaled_to_size" -> return evt -- do nothing (XXX)
        _ -> throwError $ "unrecognized event section in " <> T.pack file <> ": " <> label
eventAddSection evt _ = return evt

addEU4Option :: Monad m => Maybe [EU4Option] -> GenericScript -> PPT m (Maybe [EU4Option])
addEU4Option Nothing opt = addEU4Option (Just []) opt
addEU4Option (Just opts) opt = do
    optn <- foldM optionAddStatement newEU4Option opt
    return $ Just (opts ++ [optn])

optionAddStatement :: Monad m => EU4Option -> GenericStatement -> PPT m EU4Option
optionAddStatement opt stmt@[pdx| $label = %rhs |] =
    case label of
        "name" -> case textRhs rhs of
            Just name -> return $ opt { eu4opt_name = Just name }
            _ -> error "bad option name"
        "ai_chance" -> case rhs of
            CompoundRhs ai_chance -> return $ opt { eu4opt_ai_chance = Just ai_chance }
            _ -> error "bad option ai_chance"
        "trigger" -> case rhs of
            CompoundRhs trigger_script -> return $ opt { eu4opt_trigger = Just trigger_script }
            _ -> error "bad option trigger"
        -- Other statements are just effects.
        _ -> do
            effects_pp'd <- optionAddEffect (eu4opt_effects opt) stmt
            return $ opt { eu4opt_effects = effects_pp'd }
optionAddStatement opt stmt = do
    -- Not a GenericLhs - presumably an effect.
    effects_pp'd <- optionAddEffect (eu4opt_effects opt) stmt
    return $ opt { eu4opt_effects = effects_pp'd }

optionAddEffect :: Monad m => Maybe GenericScript -> GenericStatement -> PPT m (Maybe GenericScript)
optionAddEffect Nothing stmt = optionAddEffect (Just []) stmt
optionAddEffect (Just effs) stmt = return $ Just (effs ++ [stmt])

-- Pretty-print an event, or fail.
pp_event :: forall m. MonadError Text m => EU4Event -> PPT m Doc
pp_event evt = case (eu4evt_id evt
                    ,eu4evt_title evt
                    ,eu4evt_options evt) of
    (Just eid, Just title, Just options)
        | (isJust (eu4evt_is_triggered_only evt) ||
           isJust (eu4evt_mean_time_to_happen evt)) -> do
        -- Valid event
        version <- gets gameVersion
        (conditional, options_pp'd) <- pp_options options
        titleLoc <- getGameL10n title
        descLoc <- getGameL10n `mapM` eu4evt_desc evt
        let evtArg :: Text -> (EU4Event -> Maybe a) -> (a -> PPT m Doc) -> PPT m [Doc]
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
            isTriggeredOnly = fromMaybe False $ eu4evt_is_triggered_only evt
            evtId = strictText eid
        trigger_pp'd <- evtArg "trigger" eu4evt_trigger pp_script
        mmtth_pp'd <- mapM pp_mtth (eu4evt_mean_time_to_happen evt)
        immediate_pp'd <- evtArg "immediate" eu4evt_immediate pp_script
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

    (Nothing, _, _) -> throwError "eu4evt_id missing"
    (Just eid, Nothing, _) ->
        throwError ("title missing for event id " <> eid)
    (Just eid, _, Nothing) ->
        throwError ("options missing for event id " <> eid)
    (Just eid, _, _) ->
        throwError ("is_triggered_only and mean_time_to_happen missing for event id " <> eid)

pp_options :: MonadError Text m => [EU4Option] -> PPT m (Bool, Doc)
pp_options opts = do
    let triggered = any (isJust . eu4opt_trigger) opts
    options_pp'd <- mapM (pp_option triggered) opts
    return (triggered, mconcat . (line:) . intersperse line $ options_pp'd)

pp_option :: MonadError Text m => Bool -> EU4Option -> PPT m Doc
pp_option triggered opt = do
    optNameLoc <- getGameL10n `mapM` eu4opt_name opt
    case optNameLoc of
        -- NB: some options have no effect, e.g. start of Peasants' War.
        Just name_loc ->
            let mtrigger = eu4opt_trigger opt
            in do
                effects_pp'd <- pp_script (fromMaybe [] (eu4opt_effects opt))
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
