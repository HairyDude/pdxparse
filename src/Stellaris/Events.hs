{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, LambdaCase, ViewPatterns #-}
module Stellaris.Events (
        parseStellarisEvents
    ,   writeStellarisEvents
    ) where

import Debug.Trace (traceM)

import Control.Arrow ((&&&))
import Control.Monad (liftM, mapM, forM, foldM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..), gets)
import Control.Monad.Trans (MonadIO (..))

import Data.List (intersperse, foldl')
import Data.Maybe (fromMaybe, isJust, fromJust, catMaybes)
import Data.Monoid ((<>))

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract -- everything
import qualified Doc
import Stellaris.Common -- everything
import Stellaris.Types -- everything
import FileIO (Feature (..), writeFeatures)
import Messages -- everything
import QQ (pdx)
import SettingsTypes ( Settings (..), PPT, Game (..)
                     , IsGame (..), IsGameData (..), IsGameState (..)
                     , getGameL10n, getGameL10nIfPresent
                     , withCurrentFile, setCurrentFile
                     , hoistErrors, hoistExceptions)

-- Starts off Nothing everywhere.
newStellarisEvent :: StellarisScope -> StellarisEvent
newStellarisEvent escope = StellarisEvent Nothing Nothing [] Nothing escope Nothing Nothing Nothing Nothing Nothing False [] Nothing
newStellarisOption :: StellarisOption
newStellarisOption = StellarisOption Nothing Nothing Nothing Nothing False

-- Parse events and return them.
parseStellarisEvents :: (StellarisInfo g, Monad m) =>
    PPT g m (HashMap Text StellarisEvent)
parseStellarisEvents = HM.unions . HM.elems <$> do
    scripts <- getEventScripts
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

writeStellarisEvents :: (MonadIO m, StellarisInfo g) => PPT g m ()
writeStellarisEvents = do
    events <- getEvents
    let pathedEvents :: [Feature StellarisEvent]
        pathedEvents = map (\evt -> Feature {
                                    featurePath = stevt_path evt
                                ,   featureId = stevt_id evt
                                ,   theFeature = Right evt })
                            (HM.elems events)
    writeFeatures "events"
                  pathedEvents
                  (\e -> scope (stevt_scope e) $ pp_event e)

-- Parse a statement in an events file. Some statements aren't events; for
-- those, and for any obvious errors, return Right Nothing.
parseStellarisEvent :: (IsGameState (GameState g), MonadError Text m) =>
    GenericStatement -> PPT g m (Either Text (Maybe StellarisEvent))
parseStellarisEvent (StatementBare _) = throwError "bare statement at top level"
parseStellarisEvent [pdx| %left = %right |] = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> throwError "internal error: custom lhs"
        IntLhs _ -> throwError "int lhs at top level"
        AtLhs _ -> return (Right Nothing)
        GenericLhs etype _ ->
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

data EvtDescI = EvtDescI {
        edi_text :: Maybe Text
    ,   edi_trigger :: Maybe GenericScript
    }
evtDesc :: MonadError Text m => Maybe Text -> GenericScript -> m StEvtDesc
evtDesc meid scr = case foldl' (evtDesc' meid) (EvtDescI Nothing Nothing) scr of
        EvtDescI (Just t) Nothing -- desc = { text = foo }
            -> return $ StEvtDescSimple t
        EvtDescI Nothing (Just trig) -- desc = { trigger = { .. } } (invalid)
            -> return $ StEvtDescCompound scr
        EvtDescI (Just t) (Just trig) -- desc = { trigger = { .. } text = foo }
                                      -- e.g. pirate.1
            -> return $ StEvtDescConditional trig t
        EvtDescI Nothing Nothing -- desc = { switch { .. = { text = foo } } }
                                 -- e.g. action.39
            -> throwError $ "bad desc: no trigger nor text" <> case meid of
                Just eid -> " in event " <> eid
                Nothing -> ""
    where
        evtDesc' _ ed [pdx| trigger = @trig |] = ed { edi_trigger = Just trig }
        evtDesc' _ ed [pdx| text = ?txt |] = ed { edi_text = Just txt }
        evtDesc' _ ed [pdx| show_sound = %_ |] = ed
        evtDesc' meid _  _ = error ("evtDesc got strange description"
                               ++ maybe "" (\eid -> " for event " ++ T.unpack eid) meid)

eventAddSection :: (IsGameState (GameState g), MonadError Text m) =>
    Maybe StellarisEvent -> GenericStatement -> PPT g m (Maybe StellarisEvent)
eventAddSection mevt stmt = sequence (eventAddSection' <$> mevt <*> pure stmt) where
    eventAddSection' evt stmt@[pdx| id = %rhs |]
        = case (textRhs rhs, floatRhs rhs) of
            (Just tid, _) -> return evt { stevt_id = Just tid }
            (_, Just nid) -> return evt { stevt_id = Just (T.pack $ show (nid::Int)) }
            _ -> withCurrentFile $ \file ->
                throwError $ "bad id in " <> T.pack file <> ": " <> T.pack (show rhs)
    eventAddSection' evt stmt@[pdx| title = %rhs |] = case textRhs rhs of
        Just title -> return evt { stevt_title = Just title }
        _ -> withCurrentFile $ \file ->
            throwError $ "bad title" <> case stevt_id evt of
                Just eid -> " in event " <> eid
                Nothing -> ""
    eventAddSection' evt stmt@[pdx| desc = %rhs |] =
        let olddescs = stevt_desc evt in case rhs of
            (textRhs -> Just desc) -> return evt { stevt_desc = olddescs ++ [StEvtDescSimple desc] }
            CompoundRhs scr -> do
                let meid = stevt_id evt
                desc <- evtDesc meid scr
                return evt { stevt_desc = olddescs ++ [desc] }
            _ -> throwError $ "bad desc" <> case stevt_id evt of
                    Just eid -> " in event " <> eid
                    Nothing -> ""
    eventAddSection' evt stmt@[pdx| picture = %rhs |] =
        case textRhs rhs of
            Just pic -> return evt { stevt_picture = Just pic }
            _ -> throwError "bad picture"
    eventAddSection' evt stmt@[pdx| trigger = %rhs |] =
        case rhs of
            CompoundRhs trigger_script -> case trigger_script of
                [] -> return evt -- empty, treat as if it wasn't there
                _ -> return evt { stevt_trigger = Just trigger_script }
            _ -> throwError "bad event trigger"
    eventAddSection' evt stmt@[pdx| is_triggered_only = %rhs |] =
        case rhs of
            GenericRhs "yes" Nothing -> return evt { stevt_is_triggered_only = Just True }
            -- no is the default, so I don't think this is ever used
            GenericRhs "no" Nothing -> return evt { stevt_is_triggered_only = Just False }
            _ -> throwError "bad trigger"
    eventAddSection' evt stmt@[pdx| mean_time_to_happen = %rhs |] =
        case rhs of
            CompoundRhs mtth -> return evt { stevt_mean_time_to_happen = Just mtth }
            _ -> throwError "bad MTTH"
    eventAddSection' evt stmt@[pdx| immediate = %rhs |] =
        case rhs of
            CompoundRhs immediate -> return evt { stevt_immediate = Just immediate }
            _ -> throwError "bad immediate section"
    eventAddSection' evt stmt@[pdx| after = %rhs |] =
        case rhs of
            CompoundRhs after -> return evt { stevt_after = Just after }
            _ -> throwError "bad after section"
    eventAddSection' evt stmt@[pdx| option = %rhs |] =
        case rhs of
            CompoundRhs option -> do
                newStellarisOptions <- addStellarisOption (stevt_options evt) option
                return evt { stevt_options = newStellarisOptions }
            _ -> throwError "bad option"
    eventAddSection' evt stmt@[pdx| fire_only_once = %_ |] =
        return evt -- do nothing
    eventAddSection' evt stmt@[pdx| major = %_ |] =
        return evt -- do nothing
    eventAddSection' evt stmt@[pdx| is_mtth_scaled_to_size = %_ |] =
        return evt -- do nothing (XXX)
    eventAddSection' evt stmt@[pdx| hide_window = %rhs |]
        | GenericRhs "yes" Nothing <- rhs = return evt { stevt_hide_window = True }
        | GenericRhs "no"  Nothing <- rhs = return evt { stevt_hide_window = False }
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

addStellarisOption :: Monad m => [StellarisOption] -> GenericScript -> PPT g m [StellarisOption]
addStellarisOption opts opt = do
    optn <- foldM optionAddStatement newStellarisOption opt
    return $ opts ++ [optn]

optionAddStatement :: Monad m => StellarisOption -> GenericStatement -> PPT g m StellarisOption
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

optionAddEffect :: Monad m => Maybe GenericScript -> GenericStatement -> PPT g m (Maybe GenericScript)
optionAddEffect Nothing stmt = optionAddEffect (Just []) stmt
optionAddEffect (Just effs) stmt = return $ Just (effs ++ [stmt])

ppDescs :: (StellarisInfo g, Monad m) => Bool -> [StEvtDesc] -> PPT g m Doc
ppDescs True _ = return "| cond_event_text = (This event is hidden and has no description.)"
ppDescs _ [] = return "| event_text = (No description)"
ppDescs _ [StEvtDescSimple key] = ("| event_text = " <>) . Doc.strictText <$> getGameL10n key
ppDescs _ descs = ("| cond_event_text = " <>) .PP.vsep <$> mapM ppDesc descs where
    ppDesc (StEvtDescSimple key) = ("Otherwise:<br>:" <>) <$> fmtDesc key
    ppDesc (StEvtDescConditional scr key) = mconcat <$> sequenceA
        [pure "The following description is used if:", pure PP.line
        ,imsg2doc =<< ppMany scr, pure PP.line
        ,pure ":", fmtDesc key
        ]
    ppDesc (StEvtDescCompound scr) =
        (("| cond_event_text =" <> PP.line) <>) <$> (imsg2doc =<< ppMany scr)
    fmtDesc key = flip liftM (getGameL10nIfPresent key) $ \case
        Nothing -> Doc.strictText key
        Just txt -> "''" <> Doc.strictText (Doc.nl2br txt) <> "''"

-- Pretty-print an event, or fail.
pp_event :: forall g m. (StellarisInfo g, MonadError Text m) =>
    StellarisEvent -> PPT g m Doc
pp_event evt = case stevt_id evt of
    Just eid
        | (isJust (stevt_is_triggered_only evt) ||
           isJust (stevt_mean_time_to_happen evt)) -> do
        -- Valid event
        version <- gameVersion <$> gets getSettings
        (conditional, options_pp'd) <- pp_options (stevt_hide_window evt) eid (stevt_options evt)
        titleLoc <- fromMaybe eid <$> sequence (getGameL10n <$> stevt_title evt)
        descLoc <- ppDescs (stevt_hide_window evt) (stevt_desc evt)
        let evtArg :: Text -> (StellarisEvent -> Maybe a) -> (a -> PPT g m Doc) -> PPT g m [Doc]
            evtArg fieldname field fmt
                = maybe (return [])
                    (\field_content -> do
                        content_pp'd <- fmt field_content 
                        return
                            ["| ", Doc.strictText fieldname, " = "
                            ,PP.line
                            ,content_pp'd
                            ,PP.line])
                    (field evt)
            isTriggeredOnly = fromMaybe False $ stevt_is_triggered_only evt
            evtId = Doc.strictText eid
        trigger_pp'd <- evtArg "trigger" stevt_trigger pp_script
        mmtth_pp'd <- mapM pp_mtth (stevt_mean_time_to_happen evt)
        immediate_pp'd <- evtArg "immediate" stevt_immediate pp_script
        after_pp'd <- evtArg "after" stevt_after pp_script
        return . mconcat $
            ["<section begin=", evtId, "/>", PP.line
            ,"{{Event", PP.line
            ,"| version = ", Doc.strictText version, PP.line
            ,"| event_name = ", Doc.strictText titleLoc, PP.line
            ,descLoc, PP.line
            ] ++
            -- For triggered only events, mean_time_to_happen is not
            -- really mtth but instead describes weight modifiers, for
            -- scripts that trigger them with a probability based on a
            -- weight (e.g. on_bi_yearly_pulse).
            (if isTriggeredOnly then
                ["| triggered only = (please describe trigger here)",PP.line
                ]
                ++ maybe [] (:[PP.line]) mmtth_pp'd
            else []) ++
            trigger_pp'd ++
            -- mean_time_to_happen is only really mtth if it's *not*
            -- triggered only.
            (if isTriggeredOnly then [] else case mmtth_pp'd of
                Nothing -> []
                Just mtth_pp'd ->
                    ["| mtth = ", PP.line
                    ,mtth_pp'd]) ++
            immediate_pp'd ++
            after_pp'd ++
            (if conditional then ["| option conditions = yes", PP.line] else []) ++
            -- option_conditions = no (not implemented yet)
            ["| options = ", options_pp'd, PP.line
            ,"| collapse = yes", PP.line
            ,"}}", PP.line
            ,"<section end=", evtId, "/>"
            ]
        | otherwise ->
            throwError ("is_triggered_only and mean_time_to_happen missing for event id " <> eid)

    Nothing -> throwError "stevt_id missing"

pp_options :: (StellarisInfo g, MonadError Text m) =>
    Bool -> Text -> [StellarisOption] -> PPT g m (Bool, Doc)
pp_options hidden eid opts = do
    let triggered = any (isJust . stopt_trigger) opts
    options_pp'd <- case reverse opts of
        [] -> return []
        (last:rest) -> mapM (pp_option hidden triggered eid) (reverse (last { stopt_last = True } : opts))
    return (triggered, mconcat . (PP.line:) . intersperse PP.line $ options_pp'd)

pp_option :: (StellarisInfo g, MonadError Text m) =>
    Bool -> Bool -> Text -> StellarisOption -> PPT g m Doc
pp_option hidden triggered eid opt = do
    optNameLoc <- getGameL10n `mapM` stopt_name opt
    let optNameLoc' = if hidden
                        then maybe (Just "(No text)") Just optNameLoc
                        else optNameLoc
    case optNameLoc' of
        -- NB: some options have no effect, e.g. start of Peasants' War.
        Just name_loc ->
            let mtrigger = stopt_trigger opt
            in do
                effects_pp'd <- pp_script (fromMaybe [] (stopt_effects opt))
                mtrigger_pp'd <- sequence (pp_script <$> mtrigger)
                return . mconcat $
                    ["{{Option", if stopt_last opt then "|end\n" else "\n"
                    ,"| option_text = ", Doc.strictText name_loc, PP.line
                    ,"| effect =", PP.line, effects_pp'd, PP.line]
                    ++ (if triggered then
                            maybe
                                ["| trigger = Always enabled:", PP.line] -- no trigger
                            (\trigger_pp'd ->
                                ["| trigger = Enabled if:", PP.line -- trigger
                                ,trigger_pp'd, PP.line]
                            ) mtrigger_pp'd
                        else [])
                    ++
                    -- 1 = no
                    ["}}"
                    ]
        Nothing -> throwError $ "option for non-hidden event " <> eid <> " has no text"
