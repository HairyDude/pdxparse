{-# LANGUAGE OverloadedStrings #-}
module Events where

import Data.List (foldl', intersperse)
import Data.Either
import Data.Maybe
import Data.Monoid

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>))
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract
import Common
import Localization (L10n)

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

processEvent :: FilePath -> L10n -> GenericStatement -> Either Text Doc
processEvent _ l10n (StatementBare _) = Left "bare statement at top level"
processEvent file l10n (Statement left right) = case right of
    CompoundRhs parts -> case left of
        CustomLhs _ -> Left "internal error: custom lhs"
        IntLhs _ -> Left "int lhs at top level"
        GenericLhs _ -> pp_event l10n $ foldl' (eventAddSection file l10n) newEvent parts

    _ -> Right PP.empty -- assume this is one of the administrivia statements at the top

eventAddSection :: FilePath -> L10n -> Event -> GenericStatement -> Event
eventAddSection file l10n evt (Statement (GenericLhs label) rhs) = case label of
    "id" -> case rhs of
        StringRhs id -> evt { evt_id = Just id }
        GenericRhs id -> evt { evt_id = Just id }
        IntRhs id -> evt { evt_id = Just (T.pack $ show id) }
        -- id is not supposed to be float, but it parses that way.
        FloatRhs id -> evt { evt_id = Just (pp_float_t id) }
        _ -> error $ "bad id in " ++ file ++ ": " ++ show rhs
    "title" -> case rhs of
        StringRhs title -> evt { evt_title = Just title
                               , evt_title_loc = HM.lookup title l10n }
        GenericRhs title -> evt { evt_title = Just title
                                , evt_title_loc = HM.lookup title l10n }
        _ -> error $ "bad title in " ++ file
    "desc" -> case rhs of
        StringRhs desc -> evt { evt_desc = Just desc
                              , evt_desc_loc = HM.lookup desc l10n }
        GenericRhs desc -> evt { evt_desc = Just desc
                               , evt_desc_loc = HM.lookup desc l10n }
        _ -> error "bad desc"
    "picture" -> case rhs of
        GenericRhs pic -> evt { evt_picture = Just pic }
        _ -> error "bad picture"
    "trigger" -> case rhs of
        CompoundRhs trigger_script -> evt { evt_trigger = Just trigger_script }
        _ -> error "bad event trigger"
    "is_triggered_only" -> case rhs of
        GenericRhs "yes" -> evt { evt_is_triggered_only = Just True }
        -- no is the default, so I don't think this is ever used
        GenericRhs "no" -> evt { evt_is_triggered_only = Just False }
        _ -> error "bad trigger"
    "mean_time_to_happen" -> case rhs of
        CompoundRhs mtth -> evt { evt_mean_time_to_happen = Just mtth }
        _ -> error "bad MTTH"
    "immediate" -> case rhs of
        CompoundRhs immediate -> evt { evt_immediate = Just immediate }
        _ -> error "bad immediate section"
    "option" -> case rhs of
        CompoundRhs option -> evt { evt_options = addOption l10n (evt_options evt) option }
        _ -> error "bad option"
    "fire_only_once" -> evt -- do nothing
    "major" -> evt -- do nothing
    "is_mtth_scaled_to_size" -> evt -- do nothing (XXX)
    _ -> error $ "unrecognized event section in " ++ file ++ ": " ++ T.unpack label

addOption :: L10n -> Maybe [Option] -> GenericScript -> Maybe [Option]
addOption l10n Nothing opt = addOption l10n (Just []) opt
addOption l10n (Just opts) opt = Just (opts ++ [foldl' (optionAddStatement l10n) newOption opt])

optionAddStatement :: L10n -> Option -> GenericStatement -> Option
optionAddStatement l10n opt stmt@(Statement (GenericLhs label) rhs) = case label of
    "name" -> case rhs of
        StringRhs name -> opt { opt_name = Just name
                              , opt_name_loc = HM.lookup name l10n }
        GenericRhs name -> opt { opt_name = Just name
                               , opt_name_loc = HM.lookup name l10n }
        _ -> error "bad option name"
    "ai_chance" -> case rhs of
        CompoundRhs ai_chance -> opt { opt_ai_chance = Just ai_chance }
        _ -> error "bad option ai_chance"
    "trigger" -> case rhs of
        CompoundRhs trigger_script -> opt { opt_trigger = Just trigger_script }
        _ -> error "bad option trigger"
    -- Other statements are just effects.
    _ -> opt { opt_effects = optionAddEffect l10n (opt_effects opt) stmt }
optionAddStatement l10n opt stmt =
    -- Not a GenericLhs - presumably an effect.
    opt { opt_effects = optionAddEffect l10n (opt_effects opt) stmt }

optionAddEffect :: L10n -> Maybe GenericScript -> GenericStatement -> Maybe GenericScript
optionAddEffect l10n Nothing stmt = optionAddEffect l10n (Just []) stmt
optionAddEffect l10n (Just effs) stmt = Just (effs ++ [stmt])

-- Pretty-print an event, or fail.
pp_event :: L10n -> Event -> Either Text Doc
pp_event l10n evt =
    if isJust (evt_title_loc evt) && isJust (evt_options evt)
        && (isJust (evt_is_triggered_only evt) ||
            isJust (evt_mean_time_to_happen evt))
    then -- Valid event, carry on
        case pp_options l10n (fromJust (evt_options evt)) of
            Left err -> Left $ "failed to pprint event options: " <> err
            Right (conditional, options_pp'd) -> Right . mconcat $
                ["{{Event", line
                ,"| event_name = ", text (TL.fromStrict . fromJust $ evt_title_loc evt), line
                ] ++
                maybe [] (\desc ->
                            ["| event_text = "
                            ,text (TL.fromStrict desc)
                            ,line])
                          (evt_desc_loc evt) ++
                maybe [] (\i_t_o ->
                            ["| triggered_only = "
                            ,(if i_t_o then "(please describe trigger here)"
                              else "No")
                            ,line])
                          (evt_is_triggered_only evt) ++
                maybe [] (\trigger ->
                            ["| trigger ="
                            ,line
                            ,pp_script 1 l10n trigger
                            ,line])
                         (evt_trigger evt) ++
                maybe [] (\mtth ->
                            ["| mtth = "
                            ,pp_mtth l10n mtth
                            ,line])
                          (evt_mean_time_to_happen evt) ++
                maybe [] (\immediate ->
                            ["| immediate = "
                            ,line
                            ,pp_script 1 l10n immediate
                            ,line])
                          (evt_immediate evt) ++
                (if conditional then ["| option conditions = yes"] else []) ++
                -- option_conditions = no (not implemented yet)
                ["| options = "
                ,options_pp'd
                ,line
                -- collapse = no
                ,"}}"
                ]

    else Left "some required event sections missing"

pp_options :: L10n -> [Option] -> Either Text (Bool, Doc)
pp_options l10n opts = case partitionEithers $ map (pp_option l10n) opts of
    ([], triggered_opts_pp'd) ->
        let (triggered, opts_pp'd) = unzip triggered_opts_pp'd
            conditional = or triggered
        in Right (conditional, mconcat . (line:) . intersperse line $ opts_pp'd)
    (err:_, _) -> Left err

pp_option :: L10n -> Option -> Either Text (Bool, Doc)
pp_option l10n opt =
    if isJust (opt_name_loc opt)
        -- NB: some options have no effect, e.g. start of Peasants' War.
    then -- Valid option, carry on
        let mtrigger = opt_trigger opt
            triggered = isJust mtrigger
            the_trigger = fromJust mtrigger
        in Right (triggered, mconcat $
            ["{{Option\n"
            ,"| option_text = ", text (TL.fromStrict . fromJust $ opt_name_loc opt), line
            ,"| effect =", line, pp_script 1 l10n (maybe [] id (opt_effects opt)), line]
            ++ (if triggered then
            ["| trigger =", line, pp_script 1 l10n the_trigger, line]
            else [])
            ++
            -- 1 = no
            ["}}"
            ])
    else Left $ "some required option sections missing - dumping: " <> T.pack (show opt)