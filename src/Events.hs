{-# LANGUAGE OverloadedStrings #-}
module Events where

import Debug.Trace

import Prelude hiding (mapM)

import Control.Applicative
import Control.Monad.Reader hiding (mapM)

import Data.List (foldl', unfoldr, intersperse)
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Traversable

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>))
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract
import Common
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

processEvent :: GenericStatement -> PP (Either Text Doc)
processEvent (StatementBare _) = return $ Left "bare statement at top level"
processEvent (Statement left right) = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> return $ Left "internal error: custom lhs"
        IntLhs _ -> return $ Left "int lhs at top level"
        GenericLhs _ -> do
            mfile <- asks currentFile
            maybe (return $ Left "no current file")
                  (\file ->
                      pp_event =<< foldM eventAddSection newEvent parts)
                  mfile

    _ -> return $ Right PP.empty -- assume this is one of the administrivia statements at the top

-- Do not call this with a state in which currentFile is Nothing.
eventAddSection :: Event -> GenericStatement -> PP Event
eventAddSection evt (Statement (GenericLhs label) rhs) = withCurrentFile $ \file -> do
    case label of
        "id" -> case rhs of
            StringRhs id -> return evt { evt_id = Just id }
            GenericRhs id -> return evt { evt_id = Just id }
            IntRhs id -> return evt { evt_id = Just (T.pack $ show id) }
            -- id is not supposed to be float, but it parses that way.
            FloatRhs id -> return evt { evt_id = Just (pp_float_t id) }
            _ -> error $ "bad id in " ++ file ++ ": " ++ show rhs
        "title" -> case rhs of
            StringRhs title -> do
                t_loc <- getGameL10nIfPresent title
                return evt { evt_title = Just title
                           , evt_title_loc = t_loc }
            GenericRhs title -> do
                t_loc <- getGameL10nIfPresent title
                return evt { evt_title = Just title
                           , evt_title_loc = t_loc }
            _ -> error $ "bad title in " ++ file
        "desc" -> case rhs of
            StringRhs desc -> do
                desc_loc <- getGameL10nIfPresent desc
                return evt { evt_desc = Just desc
                           , evt_desc_loc = desc_loc }
            GenericRhs desc -> do
                desc_loc <- getGameL10nIfPresent desc
                return evt { evt_desc = Just desc
                           , evt_desc_loc = desc_loc }
            _ -> error "bad desc"
        "picture" -> case rhs of
            GenericRhs pic -> return evt { evt_picture = Just pic }
            _ -> error "bad picture"
        "trigger" -> case rhs of
            CompoundRhs trigger_script -> return evt { evt_trigger = Just trigger_script }
            _ -> error "bad event trigger"
        "is_triggered_only" -> case rhs of
            GenericRhs "yes" -> return evt { evt_is_triggered_only = Just True }
            -- no is the default, so I don't think this is ever used
            GenericRhs "no" -> return evt { evt_is_triggered_only = Just False }
            _ -> error "bad trigger"
        "mean_time_to_happen" -> case rhs of
            CompoundRhs mtth -> return evt { evt_mean_time_to_happen = Just mtth }
            _ -> error "bad MTTH"
        "immediate" -> case rhs of
            CompoundRhs immediate -> return evt { evt_immediate = Just immediate }
            _ -> error "bad immediate section"
        "option" -> case rhs of
            CompoundRhs option -> do
                newOptions <- addOption (evt_options evt) option
                return evt { evt_options = newOptions }
            _ -> error "bad option"
        "fire_only_once" -> return evt -- do nothing
        "major" -> return evt -- do nothing
        "is_mtth_scaled_to_size" -> return evt -- do nothing (XXX)
        _ -> error $ "unrecognized event section in " ++ file ++ ": " ++ T.unpack label

addOption :: Maybe [Option] -> GenericScript -> PP (Maybe [Option])
addOption Nothing opt = addOption (Just []) opt
addOption (Just opts) opt = do
    opt <- foldM optionAddStatement newOption opt
    return $ Just (opts ++ [opt])

optionAddStatement :: Option -> GenericStatement -> PP Option
optionAddStatement opt stmt@(Statement (GenericLhs label) rhs) = do
    case label of
        "name" -> case rhs of
            StringRhs name ->
                (\name_loc -> opt { opt_name = Just name
                                  , opt_name_loc = name_loc })
                <$> getGameL10nIfPresent name
            GenericRhs name ->
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

optionAddEffect :: Maybe GenericScript -> GenericStatement -> PP (Maybe GenericScript)
optionAddEffect Nothing stmt = optionAddEffect (Just []) stmt
optionAddEffect (Just effs) stmt = return $ Just (effs ++ [stmt])

-- Pretty-print an event, or fail.
pp_event :: Event -> PP (Either Text Doc)
pp_event evt = do
    version <- asks gameVersion
    if isJust (evt_title_loc evt) && isJust (evt_options evt)
        && (isJust (evt_is_triggered_only evt) ||
            isJust (evt_mean_time_to_happen evt))
    then do -- Valid event, carry on
        eoptions_pp'd <- pp_options (fromJust (evt_options evt))
        case eoptions_pp'd of
            Left err -> return . Left $ "failed to pprint event options: " <> err
            Right (conditional, options_pp'd) -> do
                let evtArg :: (Event -> Maybe a) -> (a -> PP Doc) -> PP [Doc]
                    evtArg field fmt
                        = maybe (return [])
                            (\field_content -> do
                                content_pp'd <- fmt field_content 
                                return
                                    ["| immediate = "
                                    ,line
                                    ,content_pp'd
                                    ,line])
                            (field evt)
                trigger_pp'd <- evtArg evt_trigger pp_script
                mtth_pp'd <- evtArg evt_mean_time_to_happen pp_mtth
                immediate_pp'd <- evtArg evt_immediate pp_script
                return . Right . mconcat $
                    ["{{Event", line
                    ,"| version = ", strictText version, line
                    ,"| event_name = ", text (TL.fromStrict . fromJust $ evt_title_loc evt), line
                    ] ++
                    maybe [] (\desc ->
                                ["| event_text = "
                                ,text . TL.fromStrict . nl2br $ desc
                                ,line])
                              (evt_desc_loc evt) ++
                    maybe [] (\i_t_o ->
                                ["| triggered_only = "
                                ,(if i_t_o then "(please describe trigger here)"
                                  else "No")
                                ,line])
                              (evt_is_triggered_only evt) ++
                    trigger_pp'd ++
                    mtth_pp'd ++
                    immediate_pp'd ++
                    (if conditional then ["| option conditions = yes"] else []) ++
                    -- option_conditions = no (not implemented yet)
                    ["| options = "
                    ,options_pp'd
                    ,line
                    -- collapse = no
                    ,"}}"
                    ]

    else return $ Left "some required event sections missing"

pp_options :: [Option] -> PP (Either Text (Bool, Doc))
pp_options opts = do
    options_pp'd <- mapM pp_option opts
    return $ case partitionEithers options_pp'd of
        ([], triggered_opts_pp'd) ->
            let (triggered, opts_pp'd) = unzip triggered_opts_pp'd
                conditional = or triggered
            in Right (conditional, mconcat . (line:) . intersperse line $ opts_pp'd)
        (err:_, _) -> Left err

pp_option :: Option -> PP (Either Text (Bool, Doc))
pp_option opt = do
    if isJust (opt_name_loc opt)
        -- NB: some options have no effect, e.g. start of Peasants' War.
    then -- Valid option, carry on
        let mtrigger = opt_trigger opt
            triggered = isJust mtrigger
            the_trigger = fromJust mtrigger
        in do
            effects_pp'd <- pp_script (maybe [] id (opt_effects opt))
            trigger_pp'd <- pp_script the_trigger
            return $ Right (triggered, mconcat $
                ["{{Option\n"
                ,"| option_text = ", text (TL.fromStrict . fromJust $ opt_name_loc opt), line
                ,"| effect =", line, effects_pp'd, line]
                ++ (if triggered then
                ["| trigger =", line, trigger_pp'd, line]
                else [])
                ++
                -- 1 = no
                ["}}"
                ])
    else return . Left $ "some required option sections missing - dumping: " <> T.pack (show opt)
