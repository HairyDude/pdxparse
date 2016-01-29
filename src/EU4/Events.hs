{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module EU4.Events (
        processEvent
    ) where

import Prelude hiding (mapM)

import Debug.Trace

import Control.Arrow (first)
import Control.Monad.Except
import Control.Monad.Reader hiding (mapM)

import Data.List (intersperse)
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Traversable

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>), (</>))
import qualified Text.PrettyPrint.Leijen.Text as PP

import System.FilePath

import Abstract
import EU4.Common
import Messages
import SettingsTypes

-- Object that accumulates info about an event.
data Event = Event
    {   evt_id :: Maybe Text -- event id
    ,   evt_title :: Maybe Text -- event title l10n key
    ,   evt_title_loc :: Maybe Text -- localized event title
    ,   evt_desc :: Maybe Text -- event description l10n key
    ,   evt_desc_loc :: Maybe Text -- localized event description
    ,   evt_picture :: Maybe Text -- event picture
    ,   evt_trigger :: Maybe GenericScript
    ,   evt_is_triggered_only :: Maybe Bool
    ,   evt_mean_time_to_happen :: Maybe GenericScript
    ,   evt_immediate :: Maybe GenericScript
    ,   evt_options :: Maybe [Option]
    } deriving (Show)
data Option = Option
    {   opt_name :: Maybe Text
    ,   opt_name_loc :: Maybe Text
    ,   opt_trigger :: Maybe GenericScript
    ,   opt_ai_chance :: Maybe GenericScript
    ,   opt_effects :: Maybe GenericScript
    } deriving (Show)
-- Starts off Nothing everywhere.
newEvent = Event Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
newOption = Option Nothing Nothing Nothing Nothing Nothing

processEvent :: MonadError Text m => GenericStatement -> PPT EU4 m [Either Text (FilePath, Doc)]
processEvent (StatementBare _) = throwError "bare statement at top level"
processEvent (Statement left right) = fmap (:[]) . withCurrentFile $ \file -> case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> throwError "internal error: custom lhs"
        IntLhs _ -> throwError "int lhs at top level"
        GenericLhs _ -> hoistErrors
            (first (file </>) <$> (pp_event =<< foldM eventAddSection newEvent parts))

    _ -> return $ Right (file </> "administrivia", PP.empty)

eventAddSection :: MonadError Text m => Event -> GenericStatement -> PPT extra m Event
eventAddSection evt (Statement (GenericLhs label) rhs) = withCurrentFile $ \file ->
    case label of
        "id" -> case (textRhs rhs, floatRhs rhs) of
            (Just tid, _) -> return evt { evt_id = Just tid }
            (_, Just nid) -> return evt { evt_id = Just (T.pack $ show (nid::Int)) }
            _ -> throwError $ "bad id in " <> T.pack file <> ": " <> T.pack (show rhs)
        "title" -> case textRhs rhs of
            Just title -> do
                t_loc <- getGameL10nIfPresent title
                return evt { evt_title = Just title
                           , evt_title_loc = t_loc }
            _ -> throwError $ "bad title in " <> T.pack file
        "desc" -> case textRhs rhs of
            Just desc -> do
                desc_loc <- getGameL10nIfPresent desc
                return evt { evt_desc = Just desc
                           , evt_desc_loc = desc_loc }
            _ -> throwError "bad desc"
        "picture" -> case textRhs rhs of
            Just pic -> return evt { evt_picture = Just pic }
            _ -> throwError "bad picture"
        "trigger" -> case rhs of
            CompoundRhs trigger_script -> case trigger_script of
                [] -> return evt -- empty, treat as if it wasn't there
                _ -> return evt { evt_trigger = Just trigger_script }
            _ -> throwError "bad event trigger"
        "is_triggered_only" -> case rhs of
            GenericRhs "yes" -> return evt { evt_is_triggered_only = Just True }
            -- no is the default, so I don't think this is ever used
            GenericRhs "no" -> return evt { evt_is_triggered_only = Just False }
            _ -> throwError "bad trigger"
        "mean_time_to_happen" -> case rhs of
            CompoundRhs mtth -> return evt { evt_mean_time_to_happen = Just mtth }
            _ -> throwError "bad MTTH"
        "immediate" -> case rhs of
            CompoundRhs immediate -> return evt { evt_immediate = Just immediate }
            _ -> throwError "bad immediate section"
        "option" -> case rhs of
            CompoundRhs option -> do
                newOptions <- addOption (evt_options evt) option
                return evt { evt_options = newOptions }
            _ -> throwError "bad option"
        "fire_only_once" -> return evt -- do nothing
        "major" -> return evt -- do nothing
        "is_mtth_scaled_to_size" -> return evt -- do nothing (XXX)
        _ -> throwError $ "unrecognized event section in " <> T.pack file <> ": " <> label
eventAddSection evt _ = return evt

addOption :: Monad m => Maybe [Option] -> GenericScript -> PPT extra m (Maybe [Option])
addOption Nothing opt = addOption (Just []) opt
addOption (Just opts) opt = do
    opt <- foldM optionAddStatement newOption opt
    return $ Just (opts ++ [opt])

optionAddStatement :: Monad m => Option -> GenericStatement -> PPT extra m Option
optionAddStatement opt stmt@(Statement (GenericLhs label) rhs) =
    case label of
        "name" -> case textRhs rhs of
            Just name ->
                (\name_loc -> opt { opt_name = Just name
                                  , opt_name_loc = name_loc })
                <$> getGameL10nIfPresent name
            _ -> error "bad option name"
        "ai_chance" -> case rhs of
            CompoundRhs ai_chance -> return $ opt { opt_ai_chance = Just ai_chance }
            _ -> error "bad option ai_chance"
        "trigger" -> case rhs of
            CompoundRhs trigger_script -> return $ opt { opt_trigger = Just trigger_script }
            _ -> error "bad option trigger"
        -- Other statements are just effects.
        _ -> do
            effects_pp'd <- optionAddEffect (opt_effects opt) stmt
            return $ opt { opt_effects = effects_pp'd }
optionAddStatement opt stmt = do
    -- Not a GenericLhs - presumably an effect.
    effects_pp'd <- optionAddEffect (opt_effects opt) stmt
    return $ opt { opt_effects = effects_pp'd }

optionAddEffect :: Monad m => Maybe GenericScript -> GenericStatement -> PPT extra m (Maybe GenericScript)
optionAddEffect Nothing stmt = optionAddEffect (Just []) stmt
optionAddEffect (Just effs) stmt = return $ Just (effs ++ [stmt])

-- Pretty-print an event, or fail.
pp_event :: forall m. MonadError Text m => Event -> PPT EU4 m (FilePath, Doc)
pp_event evt = case (evt_id evt
                    ,evt_title_loc evt
                    ,evt_options evt) of
    (Just eid, Just title_loc, Just options)
        | (isJust (evt_is_triggered_only evt) ||
           isJust (evt_mean_time_to_happen evt)) -> do
        -- Valid event
        version <- asks gameVersion
        (conditional, options_pp'd) <- pp_options options
        let evtArg :: Text -> (Event -> Maybe a) -> (a -> PPT extra m Doc) -> PPT extra m [Doc]
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
            isTriggeredOnly = fromMaybe False $ evt_is_triggered_only evt
            evtId = strictText eid
        trigger_pp'd <- evtArg "trigger" evt_trigger pp_script
        mmtth_pp'd <- mapM pp_mtth (evt_mean_time_to_happen evt)
        immediate_pp'd <- evtArg "immediate" evt_immediate pp_script
        return . (,) (T.unpack eid) . mconcat $
            ["<section begin=", evtId, "/>", line
            ,"{{Event", line
            ,"| version = ", strictText version, line
            ,"| event_name = ", text (TL.fromStrict title_loc), line
            ] ++
            maybe [] (\desc ->
                        ["| event_text = "
                        ,text . TL.fromStrict . nl2br $ desc
                        ,line])
                      (evt_desc_loc evt) ++
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

    _ -> throwError "some required event sections missing"

pp_options :: MonadError Text m => [Option] -> PPT EU4 m (Bool, Doc)
pp_options opts = do
    let triggered = any (isJust . opt_trigger) opts
    options_pp'd <- mapM (pp_option triggered) opts
    return (triggered, mconcat . (line:) . intersperse line $ options_pp'd)

pp_option :: MonadError Text m => Bool -> Option -> PPT EU4 m Doc
pp_option triggered opt = case opt_name_loc opt of
    -- NB: some options have no effect, e.g. start of Peasants' War.
    Just name_loc ->
        let mtrigger = opt_trigger opt
        in do
            effects_pp'd <- pp_script (fromMaybe [] (opt_effects opt))
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
