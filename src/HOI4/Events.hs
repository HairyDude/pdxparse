{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, QuasiQuotes, LambdaCase, ViewPatterns #-}
module HOI4.Events (
        parseHOI4Events
    ,   writeHOI4Events
    ) where

import Prelude hiding (mapM)

import Debug.Trace

import Control.Arrow ((&&&))
import Control.Monad.Except
import Control.Monad.State hiding (mapM)

import Data.List (intersperse, foldl')
import Data.Maybe
import Data.Monoid

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>), (</>))

import Abstract
import HOI4.Common
import FileIO
import Messages
import QQ
import SettingsTypes

-- Starts off Nothing everywhere.
newHOI4Event :: HOI4Scope -> HOI4Event
newHOI4Event escope = HOI4Event Nothing Nothing [] Nothing escope Nothing Nothing Nothing Nothing False [] Nothing
newHOI4Option :: HOI4Option
newHOI4Option = HOI4Option Nothing Nothing Nothing Nothing

-- Parse events and return them.
parseHOI4Events :: Monad m => HashMap String GenericScript -> PPT m (HashMap Text HOI4Event)
parseHOI4Events scripts = HM.unions . HM.elems <$> do
    tryParse <- hoistExceptions $
        HM.traverseWithKey
            (\sourceFile scr ->
                setCurrentFile sourceFile $ mapM parseHOI4Event scr)
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
                where mkEvtMap :: [HOI4Event] -> HashMap Text HOI4Event
                      mkEvtMap = HM.fromList . map (fromJust . hoi4evt_id &&& id)
                        -- Events returned from parseEvent are guaranteed to have an id.

writeHOI4Events :: PPT IO ()
writeHOI4Events = do
    gdata <- gets game
    case gdata of
        GameHOI4 { hoi4data = HOI4Data { hoi4events = events } } ->
            writeFeatures "events"
                          pathedEvents
                          (\e -> scope (hoi4evt_scope e) $ pp_event e)
            where
                pathedEvents :: [Feature HOI4Event]
                pathedEvents = map (\evt -> Feature {
                                            featurePath = hoi4evt_path evt
                                        ,   featureId = hoi4evt_id evt
                                        ,   theFeature = Right evt })
                                    (HM.elems events)
        _ -> error "writeHOI4Events passed wrong game's data!"

-- Parse a statement in an events file. Some statements aren't events; for
-- those, and for any obvious errors, return Nothing.
parseHOI4Event :: MonadError Text m => GenericStatement -> PPT m (Either Text (Maybe HOI4Event))
parseHOI4Event (StatementBare _) = throwError "bare statement at top level"
parseHOI4Event [pdx| %left = %right |] = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> throwError "internal error: custom lhs"
        IntLhs _ -> throwError "int lhs at top level"
        GenericLhs etype ->
            let mescope = case etype of
                    "country_event" -> Just HOI4Country
                    "news_event" -> Just HOI4NoScope -- ?
                    "event" -> Just HOI4NoScope
                    _ -> Nothing
            in case mescope of
                Nothing -> throwError $ "unrecognized event type " <> etype
                Just escope -> do
                    mevt <- hoistErrors (foldM eventAddSection (Just (newHOI4Event escope)) parts)
                    case mevt of
                        Left err -> return (Left err)
                        Right Nothing -> return (Right Nothing)
                        Right (Just evt) -> withCurrentFile $ \file ->
                            let pathedEvt = evt { hoi4evt_path = Just file }
                            in  if isJust (hoi4evt_id pathedEvt)
                                then return (Right (Just pathedEvt))
                                else return (Left $ "error parsing events in " <> T.pack file
                                             <> ": missing event id")

    _ -> return (Right Nothing)
parseHOI4Event _ = throwError "operator other than ="

data EvtDescI = EvtDescI {
        edi_text :: Maybe Text
    ,   edi_trigger :: Maybe GenericScript
    }
evtDesc :: MonadError Text m => Maybe Text -> GenericScript -> m HOI4EvtDesc
evtDesc meid scr = case foldl' evtDesc' (EvtDescI Nothing Nothing) scr of
        EvtDescI (Just t) Nothing -- desc = { text = foo }
            -> return $ HOI4EvtDescSimple t
        EvtDescI Nothing (Just trig) -- desc = { trigger = { .. } } (invalid)
            -> return $ HOI4EvtDescCompound scr
        EvtDescI (Just t) (Just trig) -- desc = { trigger = { .. } text = foo }
                                      -- e.g. pirate.1
            -> return $ HOI4EvtDescConditional trig t
        EvtDescI Nothing Nothing -- desc = { switch { .. = { text = foo } } }
                                 -- e.g. action.39
            -> throwError $ "bad desc: no trigger nor text" <> case meid of
                Just eid -> " in event " <> eid
                Nothing -> ""
    where
        evtDesc' ed [pdx| trigger = @trig |] = ed { edi_trigger = Just trig }
        evtDesc' ed [pdx| text = ?txt |] = ed { edi_text = Just txt }
        evtDesc' ed [pdx| show_sound = %_ |] = ed

eventAddSection :: MonadError Text m => Maybe HOI4Event -> GenericStatement -> PPT m (Maybe HOI4Event)
eventAddSection mevt stmt = sequence (eventAddSection' <$> mevt <*> pure stmt) where
    eventAddSection' evt stmt@[pdx| id = %rhs |]
        = case (textRhs rhs, floatRhs rhs) of
            (Just tid, _) -> return evt { hoi4evt_id = Just tid }
            (_, Just nid) -> return evt { hoi4evt_id = Just (T.pack $ show (nid::Int)) }
            _ -> withCurrentFile $ \file ->
                throwError "bad id"
    eventAddSection' evt stmt@[pdx| title = %rhs |] = case textRhs rhs of
        Just title -> return evt { hoi4evt_title = Just title }
        _ -> withCurrentFile $ \file ->
            throwError $ "bad title" <> case hoi4evt_id evt of
                Just eid -> " in event " <> eid
                Nothing -> ""
    eventAddSection' evt stmt@[pdx| desc = %rhs |] =
        let olddescs = hoi4evt_desc evt in case rhs of
            (textRhs -> Just desc) -> return evt { hoi4evt_desc = olddescs ++ [HOI4EvtDescSimple desc] }
            CompoundRhs scr -> do
                let meid = hoi4evt_id evt
                desc <- evtDesc meid scr
                return evt { hoi4evt_desc = olddescs ++ [desc] }
            _ -> throwError $ "bad desc" <> case hoi4evt_id evt of
                    Just eid -> " in event " <> eid
                    Nothing -> ""
    eventAddSection' evt stmt@[pdx| picture = %rhs |] =
        case textRhs rhs of
            Just pic -> return evt { hoi4evt_picture = Just pic }
            _ -> throwError "bad picture"
    eventAddSection' evt stmt@[pdx| trigger = %rhs |] =
        case rhs of
            CompoundRhs trigger_script -> case trigger_script of
                [] -> return evt -- empty, treat as if it wasn't there
                _ -> return evt { hoi4evt_trigger = Just trigger_script }
            _ -> throwError "bad event trigger"
    eventAddSection' evt stmt@[pdx| is_triggered_only = %rhs |] =
        case rhs of
            GenericRhs "yes" -> return evt { hoi4evt_is_triggered_only = Just True }
            -- no is the default, so I don't think this is ever used
            GenericRhs "no" -> return evt { hoi4evt_is_triggered_only = Just False }
            _ -> throwError "bad trigger"
    eventAddSection' evt stmt@[pdx| mean_time_to_happen = %rhs |] =
        case rhs of
            CompoundRhs mtth -> return evt { hoi4evt_mean_time_to_happen = Just mtth }
            _ -> throwError "bad MTTH"
    eventAddSection' evt stmt@[pdx| immediate = %rhs |] =
        case rhs of
            CompoundRhs immediate -> return evt { hoi4evt_immediate = Just immediate }
            _ -> throwError "bad immediate section"
    eventAddSection' evt stmt@[pdx| option = %rhs |] =
        case rhs of
            CompoundRhs option -> do
                newHOI4Options <- addHOI4Option (hoi4evt_options evt) option
                return evt { hoi4evt_options = newHOI4Options }
            _ -> throwError "bad option"
    eventAddSection' evt stmt@[pdx| fire_only_once = %_ |] =
        return evt -- do nothing
    eventAddSection' evt stmt@[pdx| major = %_ |] =
        return evt -- do nothing
    eventAddSection' evt stmt@[pdx| show_major = %_ |] =
        return evt -- do nothing
    eventAddSection' evt stmt@[pdx| is_mtth_scaled_to_size = %_ |] =
        return evt -- do nothing (XXX)
    eventAddSection' evt stmt@[pdx| hidden = %rhs |]
        | GenericRhs "yes" <- rhs = return evt { hoi4evt_hide_window = True }
        | GenericRhs "no"  <- rhs = return evt { hoi4evt_hide_window = False }
    eventAddSection' evt stmt@[pdx| hide_window = %rhs |]
        | GenericRhs "yes" <- rhs = return evt { hoi4evt_hide_window = True }
        | GenericRhs "no"  <- rhs = return evt { hoi4evt_hide_window = False }
    eventAddSection' evt stmt@[pdx| show_sound = %_ |] =
        return evt -- do nothing
    eventAddSection' evt stmt@[pdx| location = %rhs |] =
        return evt -- TODO: show this, and add support on the wiki
    eventAddSection' evt stmt@[pdx| auto_opens = %_ |] =
        return evt -- I have no idea what this means
    eventAddSection' evt stmt@[pdx| is_advisor_event = %_ |] =
        return evt -- probably not important to note
    eventAddSection' evt stmt@[pdx| diplomatic = %_ |] =
        return evt -- probably not important to note
    eventAddSection' evt stmt@[pdx| picture_event_data = %_ |] =
        return evt -- probably not important to note
    eventAddSection' evt stmt@[pdx| $label = %_ |] =
        withCurrentFile $ \file ->
            throwError $ "unrecognized event section in " <> T.pack file <> ": " <> label
    eventAddSection' evt _ = return evt

addHOI4Option :: Monad m => [HOI4Option] -> GenericScript -> PPT m [HOI4Option]
addHOI4Option opts opt = do
    optn <- foldM optionAddStatement newHOI4Option opt
    return $ opts ++ [optn]

optionAddStatement :: Monad m => HOI4Option -> GenericStatement -> PPT m HOI4Option
optionAddStatement opt stmt@[pdx| $label = %rhs |] =
    case label of
        "name" -> case textRhs rhs of
            Just name -> return $ opt { hoi4opt_name = Just name }
            _ -> error "bad option name"
        "ai_chance" -> case rhs of
            CompoundRhs ai_chance -> return $ opt { hoi4opt_ai_chance = Just ai_chance }
            _ -> error "bad option ai_chance"
        "trigger" -> case rhs of
            CompoundRhs trigger_script -> return $ opt { hoi4opt_trigger = Just trigger_script }
            _ -> error "bad option trigger"
        -- Other statements are just effects.
        _ -> do
            effects_pp'd <- optionAddEffect (hoi4opt_effects opt) stmt
            return $ opt { hoi4opt_effects = effects_pp'd }
optionAddStatement opt stmt = do
    -- Not a GenericLhs - presumably an effect.
    effects_pp'd <- optionAddEffect (hoi4opt_effects opt) stmt
    return $ opt { hoi4opt_effects = effects_pp'd }

optionAddEffect :: Monad m => Maybe GenericScript -> GenericStatement -> PPT m (Maybe GenericScript)
optionAddEffect Nothing stmt = optionAddEffect (Just []) stmt
optionAddEffect (Just effs) stmt = return $ Just (effs ++ [stmt])

ppDescs :: Monad m => Bool -> [HOI4EvtDesc] -> PPT m Doc
ppDescs True _ = return "| cond_event_text = (This event is hidden and has no description.)"
ppDescs _ [] = return "| event_text = (No description)"
ppDescs _ [HOI4EvtDescSimple key] = ("| event_text = " <>) . strictText <$> getGameL10n key
ppDescs _ descs = ("| cond_event_text = " <>) .vsep <$> mapM ppDesc descs where
    ppDesc (HOI4EvtDescSimple key) = ("Otherwise:<br>:" <>) <$> fmtDesc key
    ppDesc (HOI4EvtDescConditional scr key) = mconcat <$> sequenceA
        [pure "The following description is used if:", pure line
        ,imsg2doc =<< ppMany scr, pure line
        ,pure ":", fmtDesc key
        ]
    ppDesc (HOI4EvtDescCompound scr) =
        (("| cond_event_text =" <> line) <>) <$> (imsg2doc =<< ppMany scr)
    fmtDesc key = flip liftM (getGameL10nIfPresent key) $ \case
        Nothing -> strictText key
        Just txt -> "''" <> strictText (nl2br txt) <> "''"

-- Pretty-print an event, or fail.
pp_event :: forall m. MonadError Text m => HOI4Event -> PPT m Doc
pp_event evt = case hoi4evt_id evt of
    Just eid
        | (isJust (hoi4evt_is_triggered_only evt) ||
           isJust (hoi4evt_mean_time_to_happen evt)) -> do
        -- Valid event
        version <- gets gameVersion
        (conditional, options_pp'd) <- pp_options (hoi4evt_hide_window evt) eid (hoi4evt_options evt)
        titleLoc <- fromMaybe eid <$> sequence (getGameL10n <$> hoi4evt_title evt)
        descLoc <- ppDescs (hoi4evt_hide_window evt) (hoi4evt_desc evt)
        let evtArg :: Text -> (HOI4Event -> Maybe a) -> (a -> PPT m Doc) -> PPT m [Doc]
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
            isTriggeredOnly = fromMaybe False $ hoi4evt_is_triggered_only evt
            evtId = strictText eid
        trigger_pp'd <- evtArg "trigger" hoi4evt_trigger pp_script
        mmtth_pp'd <- mapM pp_mtth (hoi4evt_mean_time_to_happen evt)
        immediate_pp'd <- evtArg "immediate" hoi4evt_immediate pp_script
        return . mconcat $
            ["<section begin=", evtId, "/>", line
            ,"{{Event", line
            ,"| version = ", strictText version, line
            ,"| event_name = ", strictText titleLoc, line
            ,descLoc, line
            ] ++
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
        | otherwise ->
            throwError ("is_triggered_only and mean_time_to_happen missing for event id " <> eid)

    Nothing -> throwError "hoi4evt_id missing"

pp_options :: MonadError Text m => Bool -> Text -> [HOI4Option] -> PPT m (Bool, Doc)
pp_options hidden eid opts = do
    let triggered = any (isJust . hoi4opt_trigger) opts
    options_pp'd <- mapM (pp_option hidden triggered eid) opts
    return (triggered, mconcat . (line:) . intersperse line $ options_pp'd)

pp_option :: MonadError Text m => Bool -> Bool -> Text -> HOI4Option -> PPT m Doc
pp_option hidden triggered eid opt = do
    optNameLoc <- getGameL10n `mapM` hoi4opt_name opt
    let optNameLoc' = if hidden
                        then maybe (Just "(No text)") Just optNameLoc
                        else optNameLoc
    case optNameLoc' of
        -- NB: some options have no effect, e.g. start of Peasants' War.
        Just name_loc ->
            let mtrigger = hoi4opt_trigger opt
            in do
                effects_pp'd <- pp_script (fromMaybe [] (hoi4opt_effects opt))
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
        Nothing -> throwError $ "option for non-hidden event " <> eid <> " has no text"
