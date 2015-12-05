{-# LANGUAGE OverloadedStrings #-}
module EU4.Common where

import Prelude hiding (sequence, mapM)

import Debug.Trace

import Control.Applicative
import Control.Monad.Reader hiding (sequence, mapM, forM)

import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Traversable

import Numeric (floatToDigits)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- TODO: get rid of these, do icon key lookups from another module
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Set (Set)
import qualified Data.Set as S

import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>), int, double)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract
import Localization

isTag :: Text -> Bool
isTag s = T.length s == 3 && T.all isUpper s

isPronoun :: Text -> Bool
isPronoun s = T.map toLower s `S.member` pronouns where
    pronouns = S.fromList
        ["root"
        ,"prev"
        ,"owner"
        ,"controller"
        ]

pp_script :: GenericScript -> PP Doc
pp_script [] = return "(Nothing)"
pp_script script = withCurrentIndent $ \indent -> do
    statements_pp'd <- mapM pp_statement' script
    return . hcat . punctuate line
        . map (mconcat (replicate indent "*" ++ [" "]) <>)
        $ statements_pp'd

-- Pretty-print a number, adding wiki formatting:
-- * {{green}} if good
-- * {{red}} if bad
-- * '''boldface''' if neutral
-- What is good or bad is determined by the first argument:
-- * if True, positive is good and negative is bad (e.g. stability)
-- * if False, negative is good and positive is bad (e.g. inflation)
-- * Either way, zero is neutral.
pp_hl_num :: (Show n, Ord n, PPSep n) => Bool -> (n -> Doc) -> n -> Doc
pp_hl_num = pp_hl_num' False

-- Format a number as a percentage (e.g. 0.5 -> 50%)
pp_hl_pc :: (Show n, Ord n, PPSep n) => Bool -> (n -> Doc) -> n -> Doc
pp_hl_pc = pp_hl_num' True

pp_hl_num' :: (Show n, Ord n, PPSep n) => Bool -> Bool -> (n -> Doc) -> n -> Doc
pp_hl_num' is_pc pos pp_num n =
    let sign = signum n
        positivity = if pos then sign else negate sign
        n_pp'd = pp_signed pp_num n <> if is_pc then "%" else ""
    in case positivity of
        -1 -> template "red" n_pp'd
        0 ->  bold n_pp'd
        1 ->  template "green" n_pp'd

-- Get the localization for a province ID, if available.
getProvLoc :: Int -> PP Text
getProvLoc n =
    let provid_t = T.pack (show n)
    in getGameL10nDefault provid_t ("PROV" <> provid_t)

-- Pretty-print a number, adding &#8239; (U+202F NARROW NO-BREAK SPACE) at
-- every power of 1000.
class Num a => PPSep a where
    pp_num_sep :: a -> Doc

group3 :: [a] -> [[a]]
group3 = unfoldr (\cs -> if null cs then Nothing else Just (splitAt 3 cs))

instance PPSep Integer where
    pp_num_sep n = strictText . T.pack $
            (if n < 0 then "-" else "") <> pp_num_sep' True (show (abs n))

-- Split into groups of 3 and intersperse the groups with narrow no-break
-- spaces.
-- If first arg is True, start grouping at the end (e.g. for integers).
pp_num_sep' :: Bool -> String -> String
pp_num_sep' int
    = mconcat
        . (if int then reverse else id)
        . intersperse "&#8239;"
        . (if int then map reverse else id)
        . group3 
        . (if int then reverse else id)

instance PPSep Int where
    pp_num_sep = pp_num_sep . toInteger

instance PPSep Double where
    pp_num_sep n
        = let absn = abs n
              (digits, exp) = floatToDigits 10 absn
              (_, fracDigits') = splitAt exp digits
              -- fracDigits' is [] if exp is a nonzero whole number
              fracDigits = if fracDigits' == [0] then [] else fracDigits'
          in (if n < 0 then "-" else "")
                <> text (TL.pack . pp_num_sep' True $ show (truncate absn))
                <> (if null fracDigits
                    then ""
                    else "."
                         <> text (TL.pack . pp_num_sep' False $
                             replicate (negate exp) '0' -- zeroes after decimal
                             ++ concatMap show fracDigits))

-- Simple template (one arg).
-- NB: This does not perform escaping of pipes (i.e. replacing them with
-- {{!}}), because I don't know how to do that with Docs.
template :: Text -> Doc -> Doc
template name content = hcat ["{{", strictText name, "|", content, "}}"]

-- Emit flag template if the argument is a tag.
flag :: Text -> PP Doc
flag name =
    if isTag name
        then template "flag" . strictText <$> getGameL10n name
        else return $ case T.map toUpper name of
                "ROOT" -> "Our country" -- will need editing for capitalization in some cases
                "PREV" -> "Previously mentioned country"
                -- Suggestions of a description for FROM are welcome.
                _ -> strictText name

-- Emit icon template.
icon :: Text -> Doc
icon what = template "icon" (strictText $ HM.lookupDefault what what scriptIconTable)

-- Set doc in italics.
italic :: Doc -> Doc
italic content = enclose "''" "''" content

-- Set doc in boldface.
bold :: Doc -> Doc
bold content = enclose "'''" "'''" content

-- Emit an icon template followed by some text, separated by space.
labelIcon :: Doc -> Doc -> Doc
labelIcon label content = hsep [template "icon" label, content]

-- Surround a doc in a <pre> element.
pre_statement :: GenericStatement -> Doc
pre_statement stmt = "<pre>" <> genericStatement2doc stmt <> "</pre>"

-- Pretty-print a statement, preceding it with one extra layer of bullets.
-- Most statements are expected to be of a particular form. If they're not, we
-- just echo the statement instead of failing. This is also what we do with
-- unrecognized statements.
pp_statement :: GenericStatement -> PP Doc
pp_statement = indentUp . pp_statement'

-- Pretty-print a statement.
pp_statement' :: GenericStatement -> PP Doc
pp_statement' stmt@(Statement lhs rhs) =
    let defaultdoc = pre_statement stmt
        compound = generic_compound defaultdoc
        -- not computed if not needed, thanks to laziness
    in case lhs of
        GenericLhs label -> case label of
            -- Statements where RHS is irrelevant (usually "yes")
            "add_cardinal"          -> return "Gain a cardinal"
            "kill_heir"             -> return "Heir dies"
            "kill_ruler"            -> return "Ruler dies"
            "remove_cardinal"       -> return "Lose a cardinal"
            -- Gain/lose, with optional icon
            -- Plain numbers
            "add_adm_power"         -> gain Plain Nothing True (Just "adm") "administrative power" stmt
            "add_army_tradition"    -> gain Plain Nothing True (Just "army tradition") "army tradition" stmt
            "add_authority"         -> gain Plain Nothing True Nothing "authority" stmt
            "add_base_tax"          -> gain Plain Nothing True (Just "base tax") "base tax" stmt
            "add_base_production"   -> gain Plain Nothing True (Just "base production") "base production" stmt
            "add_base_manpower"     -> gain Plain Nothing True (Just "manpower") "base manpower" stmt
            "add_dip_power"         -> gain Plain Nothing True (Just "dip") "diplomatic power" stmt
            "add_doom"              -> gain Plain Nothing False Nothing "doom" stmt
            "add_heir_claim"        -> gain Plain (Just "Heir") True Nothing "claim strength" stmt
            "add_devotion"          -> gain Plain Nothing True (Just "devotion") "devotion" stmt
            "add_horde_unity"       -> gain Plain Nothing True (Just "horde unity") "horde unity" stmt
            "add_imperial_influence" -> gain Plain Nothing False (Just "imperial authority") "imperial authority" stmt
            "add_karma"             -> gain Plain Nothing True (Just "high karma") "karma (Check this icon! low karma/high karma)" stmt
            "add_legitimacy"        -> gain Plain Nothing True (Just "legitimacy") "legitimacy" stmt
            "add_mil_power"         -> gain Plain Nothing True (Just "mil") "military power" stmt
            "add_navy_tradition"    -> gain Plain Nothing True (Just "navy tradition") "navy tradition" stmt
            "add_papal_influence"   -> gain Plain Nothing True (Just "papal influence") "papal influence" stmt
            "add_prestige"          -> gain Plain Nothing True (Just "prestige") "prestige" stmt
            "add_stability"         -> gain Plain Nothing True (Just "stability") "stability" stmt
            "add_war_exhaustion"    -> gain Plain Nothing False (Just "war exhaustion") "war exhaustion" stmt
            "add_yearly_manpower"   -> gain Plain Nothing True (Just "manpower") "years' worth of manpower" stmt
            "change_adm"            -> gain Plain (Just "Ruler") True (Just "adm") "administrative skill" stmt
            "change_dip"            -> gain Plain (Just "Ruler") True (Just "dip") "diplomatic skill" stmt
            "change_mil"            -> gain Plain (Just "Ruler") True (Just "mil") "military skill" stmt
            "change_siege"          -> gain Plain Nothing True Nothing "siege progress" stmt
            -- Reduced numbers
            "add_patriarch_authority"  -> gain Reduced Nothing True (Just "patriarch authority") "patriarch authority" stmt
            "add_piety"                -> gain Reduced Nothing True (Just "piety") "piety (Check this icon! pious/impious)" stmt
            "add_republican_tradition" -> gain Reduced Nothing True (Just "republican tradition") "republican tradition" stmt
            -- Percentages
            "add_inflation"      -> gain Percent Nothing False (Just "inflation") "inflation" stmt
            "add_local_autonomy" -> gain Percent Nothing False (Just "local autonomy") "local autonomy" stmt
            "add_reform_desire"  -> gain Percent (Just "Catholicism") False (Just "reform desire") "reform desire" stmt
            -- Reduced percentages
            "add_mercantilism"   -> gain ReducedPercent Nothing True (Just "mercantilism") "mercantilism" stmt
            -- Special
            "add_manpower" -> gain_manpower stmt
            -- Modifiers
            "add_country_modifier" -> add_modifier "country" stmt
            "add_permanent_province_modifier" -> add_modifier "permanent province" stmt
            "add_province_modifier" -> add_modifier "province" stmt
            "add_ruler_modifier" -> add_modifier "ruler" stmt
            "add_trade_modifier" -> add_modifier "trade" stmt
            "has_country_modifier" -> has_modifier "country" stmt
            "has_province_modifier" -> has_modifier "province" stmt
            "has_trade_modifier" -> has_modifier "trade" stmt
            "remove_country_modifier" -> remove_modifier "country" stmt
            "remove_province_modifier" -> remove_modifier "province" stmt
            -- Simple compound statements
            -- Note that "any" can mean "all" or "one or more" depending on context.
            "AND"  -> compound "All of" stmt
            "ROOT" -> compound "Our country" stmt
            -- These two are ugly, but without further analysis we can't know
            -- what it means.
            "FROM"                      -> compound "FROM" stmt
            "PREV"                      -> compound "PREV" stmt
            "NOT"                       -> compound "None of" stmt
            "OR"                        -> compound "At least one of" stmt
            -- There is a semantic distinction between "all" and "every",
            -- namely that the former means "this is true for all <type>" while
            -- the latter means "do this for every <type>." But their contexts
            -- are disjoint, so they can be presented the same way.
            "all_owned_province"        -> compound "Every owned province" stmt
            "area"                      -> compound "Area containing this province" stmt
            "any_active_trade_node"     -> compound "Any trade node with a merchant present" stmt
            "any_ally"                  -> compound "Any ally" stmt
            "any_core_country"          -> compound "Any country with a core" stmt
            "any_country"               -> compound "Any country" stmt
            "any_known_country"         -> compound "Any known country" stmt
            "any_neighbor_country"      -> compound "Any neighboring country" stmt
            "any_neighbor_province"     -> compound "Any neighboring province" stmt
            "any_owned_province"        -> compound "Any owned province" stmt
            "any_rival_country"         -> compound "Any rival" stmt
            "capital_scope"             -> compound "Capital" stmt
            "controller"                -> compound "Province controller" stmt
            "emperor"                   -> compound "The Holy Roman Emperor" stmt
            "every_country"             -> compound "Every country in the world" stmt
            "every_enemy_country"       -> compound "Every enemy country" stmt
            "every_known_country"       -> compound "Every known country" stmt
            "every_neighbor_country"    -> compound "Every neighboring country" stmt
            "every_neighbor_province"   -> compound "Every neighboring province" stmt
            "every_owned_province"      -> compound "Every owned province" stmt
            "every_province"            -> compound "Every province in the world" stmt
            "every_subject_country"     -> compound "Every subject country" stmt
            "hidden_effect"             -> compound "Hidden effect" stmt
            "if"                        -> compound "If" stmt
            "limit"                     -> compound "Limited to" stmt
            "owner"                     -> compound "Province owner" stmt
            "random_ally"               -> compound "One random ally" stmt
            "random_core_country"       -> compound "One random country with a core" stmt
            "random_country"            -> compound "One random country" stmt
            "random_list"               -> compound "One of the following at random" stmt
            "random_neighbor_country"   -> compound "One random neighboring country" stmt
            "random_neighbor_province"  -> compound "One random neighboring province" stmt
            "random_owned_province"     -> compound "One random owned province" stmt
            "random_province"           -> compound "One random province" stmt
            -- Random
            "random" -> random stmt
            -- Simple generic statements (RHS is a localizable atom)
            "change_government" -> simple_generic "Change government to" stmt mempty
            "continent"         -> simple_generic "Continent is" stmt mempty
            "culture"           -> simple_generic "Culture is" stmt mempty
            "culture_group"     -> simple_generic "Culture is in" stmt "culture group"
            "dynasty"           -> simple_generic "Ruler is of" stmt "dynasty"
            "end_disaster"      -> simple_generic "Disaster" stmt "ends"
            "government"        -> simple_generic "Government is" stmt mempty
            "has_advisor"       -> simple_generic "Has" stmt "advisor"
            "has_terrain"       -> simple_generic "Has" stmt "terrain"
            "infantry"          -> simple_generic "An infantry regiment spawns in" stmt mempty
            "kill_advisor"      -> simple_generic mempty stmt "dies"
            "primary_culture"   -> simple_generic "Primary culture is" stmt mempty
            "region"            -> simple_generic "Is in region" stmt mempty
            "remove_advisor"    -> simple_generic mempty stmt "leaves the country's court"
            "remove_estate"     -> simple_generic "Remove province from the" stmt "estate"
            -- Simple generic statements (RHS is a localizable atom that should
            -- be enclosed in single quotation marks)
            "has_disaster"      -> generic_squot "The" stmt "disaster is ongoing"
            -- RHS is a province ID
            "province_id"   -> simple_province "Province is" stmt mempty
            "owns"          -> simple_province "Owns" stmt mempty
            -- RHS is an advisor ID (TODO: parse advisor files)
            "advisor_exists"      -> numeric Plain "Advisor ID" stmt "exists"
            "is_advisor_employed" -> numeric Plain "Advisor ID" stmt "is employed"
            -- Simple generic statements (typewriter face)
            "clr_country_flag"  -> simple_generic_tt "Clear country flag" stmt
            "clr_province_flag" -> simple_generic_tt "Clear province flag" stmt
            "clr_ruler_flag"    -> simple_generic_tt "Clear ruler flag" stmt
            "has_country_flag"  -> simple_generic_tt "Has country flag" stmt
            "has_global_flag"   -> simple_generic_tt "Global flag is set:" stmt
            "has_province_flag" -> simple_generic_tt "Has province flag" stmt
            "has_ruler_flag"    -> simple_generic_tt "Has ruler flag" stmt
            "set_country_flag"  -> simple_generic_tt "Set country flag" stmt
            "set_global_flag"   -> simple_generic_tt "Set global flag" stmt
            "set_province_flag" -> simple_generic_tt "Set province flag" stmt
            "set_ruler_flag"    -> simple_generic_tt "Set ruler flag" stmt
            -- Simple generic statements with icon
            "advisor"           -> generic_icon "Has" stmt
            "change_trade_goods" -> generic_icon "Change trade goods produced to" stmt
            "create_advisor"    -> generic_icon "Gain" stmt
            "has_idea_group"    -> generic_icon "Has activated" stmt
            "trade_goods"       -> generic_icon "Produces" stmt
            "has_estate"        -> generic_icon "Has estate" stmt
            "set_estate"        -> generic_icon "Give to estate" stmt
            "is_monarch_leader" -> generic_icon "Ruler is" stmt
            -- Simple generic statements with flag
            "alliance_with"     -> generic_tag (Just "Allied with") stmt Nothing
            "cede_province"     -> generic_tag (Just "Cede province to") stmt Nothing
            "controlled_by"     -> generic_tag (Just "Is controlled by") stmt Nothing
            "defensive_war_with" -> generic_tag (Just "Is in a defensive war against") stmt Nothing
            "discover_country"  -> generic_tag (Just "Discovered by") stmt Nothing
            "add_claim"         -> generic_tag Nothing stmt (Just "gains a claim")
            "has_discovered"    -> generic_tag (Just "Has discovered") stmt Nothing
            "inherit"           -> generic_tag (Just "Inherit") stmt Nothing
            "is_neighbor_of"    -> generic_tag (Just "Neighbors") stmt Nothing
            "is_subject_of"     -> generic_tag (Just "Is a subject of") stmt Nothing
            "remove_core"       -> generic_tag Nothing stmt (Just "loses core")
            "marriage_with"     -> generic_tag (Just "Has a royal marriage with") stmt Nothing
            "offensive_war_with" -> generic_tag (Just "Is in an offensive war against") stmt Nothing
            "owned_by"          -> generic_tag (Just "Is owned by") stmt Nothing
            "release"           -> generic_tag (Just "Releases") stmt (Just "as a vassal")
            "sieged_by"         -> generic_tag (Just "Is under siege by") stmt Nothing
            "tag"               -> generic_tag (Just "Is") stmt Nothing
            "truce_with"        -> generic_tag (Just "Has a truce with") stmt Nothing
            "war_with"          -> generic_tag (Just "Is at war with") stmt Nothing
            "white_peace"       -> generic_tag (Just "Makes a white peace with") stmt Nothing
            -- Simple generic statements with flag or "yes"/"no"
            "exists"            -> generic_tag_bool "Exists" "Does NOT exist" Nothing stmt (Just "exists")
            -- Statements that may be an icon, a flag, or a pronoun (such as ROOT)
            -- Boolean argument is whether to emit an icon.
            "religion"          -> generic_icon_or_country True "Religion is" stmt
            "religion_group"    -> generic_icon_or_country False "Religion group is" stmt
            "change_religion"   -> generic_icon_or_country True "Change religion to" stmt
            -- Statements that may be either a tag or a province
            "is_core"  -> generic_tag_or_province (Just "Is core of") (Just "Has core on") stmt Nothing Nothing
            "is_claim" -> generic_tag_or_province Nothing (Just "Has claim on") stmt (Just "has a claim") Nothing
            -- Boolean statements
            "ai"                    -> is Nothing "AI controlled" stmt
            "has_cardinal"          -> has "a cardinal" stmt
            "has_heir"              -> has "an heir" stmt
            "has_owner_religion"    -> has "its owner's religion" stmt
            "has_port"              -> has "a port" stmt
            "has_seat_in_parliament" -> has "a seat in Parliament" stmt
            "has_regency"           -> is Nothing "in a regency" stmt
            "has_siege"             -> is Nothing "under siege" stmt
            "is_at_war"             -> is Nothing "at war" stmt
            "is_capital"            -> is Nothing "capital" stmt
            "is_city"               -> is (Just "Province") "a city" stmt
            "is_emperor"            -> is Nothing "Holy Roman Emperor" stmt
            "is_female"             -> is_female stmt
            "is_lesser_in_union"    -> is Nothing "the junior partner in a personal union" stmt
            "is_looted"             -> is Nothing "looted" stmt
            "is_overseas"           -> is Nothing "overseas" stmt
            "is_part_of_hre"        -> is Nothing "part of the Holy Roman Empire" stmt
            "is_reformation_center" -> is Nothing "a center of reformation" stmt
            "is_subject"            -> is Nothing "a subject nation" stmt
            "papacy_active"         -> is (Just "Papal interaction") "active" stmt
            "unit_in_siege"         -> is Nothing "under siege" stmt -- duplicate?
            "was_player"            -> has_been Nothing "player-controlled" stmt
            -- Numeric statements
            "colonysize"                -> numeric Plain "Colony has at least" stmt "settlers"
            "had_recent_war"            -> numeric Plain "Was at war within the last" stmt "months(?)"
            "heir_age"                  -> numeric Plain "Heir is at least" stmt "years old"
            "is_year"                   -> numeric Plain "Year is" stmt "or later"
            "monthly_income"            -> numeric Plain "Monthly income is at least {{icon|ducats}}" stmt "ducats"
            "num_of_loans"              -> numeric Plain "Has at least" stmt "loan(s)"
            "num_of_mercenaries"        -> numeric Plain "Has at least" stmt "mercenary regiment(s)"
            "num_of_ports"              -> numeric Plain "Has at least" stmt "port(s)"
            "num_of_rebel_armies"       -> numeric Plain "At least" stmt "rebel army/armies are present in the country"
            "num_of_trade_embargos"     -> numeric Plain "Is embargoing at least" stmt "other nation(s)"
            "units_in_province"         -> numeric Plain "Province contains at least" stmt "regiment(s)"
            -- Statements that may be numeric or a tag
            "num_of_cities"             -> numeric_or_tag "Owns" "many" stmt "cities"
            -- Signed numeric statements
            "tolerance_to_this" -> numeric_signed Plain "Tolerance to this religion is at least" stmt mempty
            -- Statements of numeric quantities with icons
            "adm"               -> numeric_icon Plain "adm" (Just "Ruler has") (Just "administrative skill of at least") Nothing stmt
            "adm_tech"          -> numeric_icon Plain "adm tech" Nothing (Just "Administrative technology is at least") Nothing stmt
            "army_tradition"    -> numeric_icon Plain "army tradition" Nothing (Just "Army tradition is at least") mempty stmt
            "base_manpower"     -> numeric_icon Plain "navy tradition" Nothing (Just "Base manpower is at least") mempty stmt
            "base_production"   -> numeric_icon Plain "base production" Nothing (Just "Base production is at least") mempty stmt
            "base_tax"          -> numeric_icon Plain "base tax" Nothing (Just "Base tax is at least") mempty stmt
            "create_admiral"    -> numeric_icon Plain "admiral" (Just "Gain") (Just "admiral with") (Just "tradition") stmt
            "create_general"    -> numeric_icon Plain "general" (Just "Gain") (Just "leader with") (Just "tradition") stmt
            "development"       -> numeric_icon Plain "development" (Just "Has at least") Nothing (Just "development") stmt
            "dip"               -> numeric_icon Plain "dip" (Just "Ruler has") (Just "diplomatic skill of at least") Nothing stmt
            "dip_tech"          -> numeric_icon Plain "dip tech" Nothing (Just "Diplomatic technology is at least") Nothing stmt
            "horde_unity"       -> numeric_icon Plain "horde unity" Nothing (Just "Horde unity is at least") mempty stmt
            "karma"             -> numeric_icon Plain "high karma" Nothing (Just "Karma is at least") Nothing stmt
            "legitimacy"        -> numeric_icon Plain "legitimacy" Nothing (Just "Legitimacy is at least") Nothing stmt
            "mil"               -> numeric_icon Plain "mil" (Just "Ruler has") (Just "military skill of at least") Nothing stmt
            "mil_tech"          -> numeric_icon Plain "mil tech" Nothing (Just "Military technology is at least") Nothing stmt
            "num_of_allies"     -> numeric_icon Plain "allies" (Just "Has at least") Nothing (Just "allies") stmt
            "num_of_cardinals"  -> numeric_icon Plain "cardinals" (Just "Controls at least") Nothing (Just "cardinals") stmt
            "num_of_colonists"  -> numeric_icon Plain "colonists" (Just "Has at least") Nothing (Just "colonist(s)") stmt
            "stability"         -> numeric_icon Plain "stability" Nothing (Just "Stability is at least") Nothing stmt
            "total_development" -> numeric_icon Plain "development" (Just "Total") (Just "development is at least") Nothing stmt
            "total_number_of_cardinals" -> numeric_icon Plain "cardinals" (Just "There are at least") Nothing (Just "cardinals") stmt
            "unrest"            -> numeric_icon Plain "unrest" Nothing (Just "Unrest is at least") Nothing stmt
            "war_exhaustion"    -> numeric_icon Plain "war exhaustion" Nothing (Just "War exhaustion is at least") Nothing stmt
            "war_score"         -> numeric_icon Plain "war score" Nothing (Just "Warscore is at least") Nothing stmt
            "republican_tradition" -> numeric_icon Reduced "republican tradition" Nothing (Just "Republican tradition is at least") Nothing stmt
            "inflation"         -> numeric_icon Percent "inflation" Nothing (Just "Inflation is at least") Nothing stmt
            "local_autonomy"    -> numeric_icon Percent "local autonomy" Nothing (Just "Local autonomy is at least") Nothing stmt
            "manpower_percentage" -> numeric_icon ReducedPercent "manpower" Nothing (Just "Manpower is at least") (Just "of maximum") stmt
            "mercantilism"      -> numeric_icon ReducedPercent "mercantilism" Nothing (Just "Mercantilism is at least") Nothing stmt
            -- Complex statements
            "add_casus_belli"         -> add_casus_belli True stmt
            "add_faction_influence"   -> faction_influence stmt
            "add_estate_loyalty"      -> estate_loyalty True stmt
            "add_estate_influence_modifier"
                                      -> estate_influence_modifier stmt
            "add_opinion"             -> opinion "Add" stmt
            "add_years_of_income"     -> add_years_of_income stmt
            "define_heir"             -> define_heir stmt
            "build_to_forcelimit"     -> build_to_forcelimit stmt
            "country_event"           -> trigger_event "country" stmt
            "declare_war_with_cb"     -> declare_war_with_cb stmt
            "define_advisor"          -> define_advisor stmt
            "define_ruler"            -> define_ruler stmt
            "estate_influence"        -> estate_influence stmt
            "estate_loyalty"          -> estate_loyalty False stmt
            "had_country_flag"        -> had_flag "country" stmt
            "had_province_flag"       -> had_flag "province" stmt
            "had_ruler_flag"          -> had_flag "ruler" stmt
            "has_estate_influence_modifier"
                                      -> has_estate_influence_modifier stmt
            "has_opinion_modifier"    -> opinion "Has" stmt
            "province_event"          -> trigger_event "province" stmt
            "reverse_add_casus_belli" -> add_casus_belli False stmt
            "trigger_switch"          -> trigger_switch stmt
            -- Rebels
            "can_spawn_rebels"  -> can_spawn_rebels stmt
            "create_revolt" -> spawn_rebels Nothing stmt
            "has_spawned_rebels" -> has_spawned_rebels stmt
            "likely_rebels" -> can_spawn_rebels stmt
            "nationalist_rebels" -> spawn_rebels (Just "nationalist_rebels") stmt
            "spawn_rebels" -> spawn_rebels Nothing stmt
            -- Special
            "add_core"  -> add_core stmt
            "has_dlc"   -> has_dlc stmt
            -- Ignored
            "custom_tooltip" -> return "(custom tooltip - delete this line)"
            "tooltip" -> return "(explanatory tooltip - delete this line)"
            -- default
            _ -> if isTag label
                 then case rhs of
                    CompoundRhs scr -> do
                        lflag <- flag label
                        script_pp'd <- indentUp (pp_script scr)
                        return $
                            lflag
                            <> ":"
                            <> line <> script_pp'd
                    _ -> return defaultdoc
                 else do
                    mloc <- getGameL10nIfPresent label
                    case mloc of
                        -- Check for localizable atoms, e.g. regions
                        Just loc -> compound loc stmt
                        Nothing -> return defaultdoc
        IntLhs n -> do -- Treat as a province tag
            let provN = T.pack (show n)
            prov_loc <- getGameL10nDefault ("Province " <> provN) ("PROV" <> provN)
            case rhs of
                CompoundRhs scr -> do
                    script_pp'd <- indentUp (pp_script scr)
                    return $ hcat
                        ["Province"
                        ,space
                        ,strictText prov_loc
                        ,":"
                        ,line
                        ,script_pp'd
                        ]
                _ -> return defaultdoc


------------------------------------------------------------------------
-- Script handlers that should be used directly, not via pp_statement --
------------------------------------------------------------------------

data MTTH = MTTH
        {   years :: Maybe Int
        ,   months :: Maybe Int
        ,   days :: Maybe Int
        ,   modifiers :: [MTTHModifier] -- TODO
        } deriving Show
data MTTHModifier = MTTHModifier
        {   mtthmod_factor :: Maybe Double
        ,   mtthmod_conditions :: GenericScript
        } deriving Show
newMTTH = MTTH Nothing Nothing Nothing []
newMTTHMod = MTTHModifier Nothing []
addField mtth _ = mtth -- unrecognized
pp_mtth :: GenericScript -> PP Doc
pp_mtth scr
    = pp_mtth' $ foldl' addField newMTTH scr
    where
        addField mtth (Statement (GenericLhs "years") (IntRhs n))
            = mtth { years = Just n }
        addField mtth (Statement (GenericLhs "years") (FloatRhs n))
            = mtth { years = Just (floor n) }
        addField mtth (Statement (GenericLhs "months") (IntRhs n))
            = mtth { months = Just n }
        addField mtth (Statement (GenericLhs "months") (FloatRhs n))
            = mtth { months = Just (floor n) }
        addField mtth (Statement (GenericLhs "days") (IntRhs n))
            = mtth { days = Just n }
        addField mtth (Statement (GenericLhs "days") (FloatRhs n))
            = mtth { days = Just (floor n) }
        addField mtth (Statement (GenericLhs "modifier") (CompoundRhs rhs))
        --            = addFactor mtth rhs
            = addMTTHMod mtth rhs -- TODO
        addMTTHMod mtth scr = mtth { modifiers = modifiers mtth ++ [foldl' addMTTHModField newMTTHMod scr] } where
            addMTTHModField mtthmod (Statement (GenericLhs "factor") rhs)
                = mtthmod { mtthmod_factor = floatRhs rhs }
            addMTTHModField mtthmod stmt -- anything else is a condition
                = mtthmod { mtthmod_conditions = mtthmod_conditions mtthmod ++ [stmt] }
        pp_mtth' mtth@(MTTH years months days modifiers) = do
            modifiers_pp'd <- indentUp (intersperse line <$> mapM pp_mtthmod modifiers)
            let hasYears = isJust years
                hasMonths = isJust months
                hasDays = isJust days
                hasModifiers = not (null modifiers)
            return . mconcat $
                ["*"]
                ++
                ((if hasYears then
                    [PP.int (fromJust years), space, "year(s)"]
                    ++
                    if hasMonths && hasDays then [",", space]
                    else if hasMonths || hasDays then ["and", space]
                    else []
                 else [])
                ++
                (if hasMonths then
                    [PP.int (fromJust months), space, "month(s)"]
                 else [])
                ++
                (if hasDays then
                    (if hasYears && hasMonths then ["and", space]
                     else []) -- if years but no months, already added "and"
                    ++
                    [PP.int (fromJust days), space, "day(s)"]
                 else []))
                ++
                (if hasModifiers then
                    [line, "''Modifiers''", line]
                    ++ modifiers_pp'd
                 else [])
        pp_mtthmod mtthmod@(MTTHModifier (Just factor) conditions) = do
            conditions_pp'd <- indentUp (pp_script conditions)
            return . mconcat $
                ["*"
                ,enclose "'''×" "''':" (pp_float factor)
                ,line
                ,conditions_pp'd
                ]
        pp_mtthmod mtthmod@(MTTHModifier Nothing conditions)
            = return "(invalid modifier! Bug in extractor?)"

--------------------------------
-- General statement handlers --
--------------------------------

generic_compound_doc :: Doc -> Doc -> GenericStatement -> PP Doc
generic_compound_doc _ header (Statement _ (CompoundRhs scr))
    = do
        script_pp'd <- indentUp (pp_script scr)
        return $ hcat
            [header, ":"
            ,line
            ,script_pp'd
            ]
generic_compound_doc defaultdoc _ _ = return defaultdoc

generic_compound :: Doc -> Text -> GenericStatement -> PP Doc
generic_compound defaultdoc header stmt
        = generic_compound_doc defaultdoc (strictText header) stmt

-- Statement with generic on both sides translating to the form
--  <string> <l10n value> <string>
simple_generic :: Text -> GenericStatement -> Text -> PP Doc
simple_generic premsg (Statement _ (GenericRhs name)) postmsg
    = (\name_loc -> hsep
        [strictText premsg
        ,strictText name_loc
        ,strictText postmsg
        ])
      <$> getGameL10n name
simple_generic _ stmt _ = return $ pre_statement stmt

-- Statement with generic on both sides translating to the form
--  <string>  ‘<l10n value>’ <string>
generic_squot :: Text -> GenericStatement -> Text -> PP Doc
generic_squot premsg (Statement _ (GenericRhs name)) postmsg
    = (\name_loc -> hsep
        [strictText premsg
        ,enclose "‘" "’" (strictText name_loc)
        ,strictText postmsg
        ])
      <$> getGameL10n name
generic_squot _ stmt _ = return $ pre_statement stmt

simple_province :: Text -> GenericStatement -> Text -> PP Doc
simple_province premsg (Statement lhs rhs) postmsg
    = let loc_key = "PROV" <> case rhs of
            IntRhs id -> show id
            -- Province IDs shouldn't parse as float, but unfortunately they
            -- do. Just ignore the fractional part.
            FloatRhs id -> show (round id)
      in simple_generic premsg (Statement lhs (GenericRhs (T.pack loc_key))) postmsg

-- Enclose a doc in an HTML element. Only simple tags are supported.
pp_elem :: Text -> Doc -> Doc
pp_elem tag = enclose (hcat ["<", dtag, ">"]) (hcat ["</", dtag, ">"])
    where dtag = strictText tag

-- As simple_generic but definitely no l10n. Set the RHS in typewriter face
simple_generic_tt :: Text -> GenericStatement -> PP Doc
simple_generic_tt premsg (Statement _ (GenericRhs name))
    = return $ mconcat [strictText $ premsg, space, pp_elem "tt" (strictText name)]
simple_generic_tt _ stmt = return $ pre_statement stmt

-- Table of script atom -> icon key. Only ones that are different are listed.
scriptIconTable :: HashMap Text Text
scriptIconTable = HM.fromList
    [("master_of_mint", "master of mint")
    ,("natural_scientist", "natural scientist")
    ,("colonial_governor", "colonial governor")
    ,("diplomat", "diplomat_adv")
    ,("naval_reformer", "naval reformer")
    ,("army_organizer", "army organizer")
    ,("army_reformer", "army reformer")
    ,("grand_captain", "grand captain")
    ,("master_recruiter", "master recruiter")
    ,("military_engineer", "military engineer")
    ,("spy_ideas", "espionage")
    ,("estate_church", "clergy")
    ,("estate_nobles", "nobles")
    ,("estate_burghers", "burghers")
    ,("estate_cossacks", "cossacks")
    ,("estate_nomadic_tribes", "tribes")
    ,("estate_dhimmi", "dhimmi")
    ,("base production", "production")
    ,("particularist", "particularists")
    ,("is_monarch_leader", "ruler general")
    ]

-- As simple_generic but also add an appropriate icon before the value.
generic_icon :: Text -> GenericStatement -> PP Doc
generic_icon premsg (Statement (GenericLhs category) (GenericRhs name))
    = (\name_loc -> hsep
        [strictText $ premsg
        ,icon (HM.lookupDefault
                -- If nothing specified above, at least change underscores to spaces
                (T.map (\c -> if c == '_' then ' ' else c) name)
                name scriptIconTable)
        ,strictText name_loc])
      <$> getGameL10n name
generic_icon _ stmt = return $ pre_statement stmt

-- As generic_icon but say "same as <foo>" if foo refers to a country
-- (in which case, add a flag if it's a specific country).
generic_icon_or_country :: Bool -> Text -> GenericStatement -> PP Doc
generic_icon_or_country doicon premsg (Statement (GenericLhs category) (GenericRhs name)) = do
    nflag <- flag name -- laziness means this might not get evaluated
    name_loc <- getGameL10n name
    return . hsep $ strictText premsg :
          if isTag name || isPronoun name
            then ["same", "as", nflag]
            else (if doicon
                    then [icon (HM.lookupDefault name name scriptIconTable)]
                    else [])
                ++ [strictText name_loc]
generic_icon_or_country _ _ stmt = return $ pre_statement stmt

generic_tag_or_province :: Maybe Text -> Maybe Text -> GenericStatement -> Maybe Text -> Maybe Text -> PP Doc
generic_tag_or_province pre_tag pre_prov (Statement _ rhs) post_tag post_prov
    = let eobject = case rhs of
            GenericRhs tag -> Left tag
            IntRhs provid -> Right provid
            FloatRhs provid -> Right (round provid)
      in case eobject of
            Left tag -> do -- is a tag
                tagflag <- flag tag
                return . hsep $
                    (if isJust pre_tag
                        then [strictText (fromJust pre_tag)]
                        else []) ++
                    [tagflag] ++
                    if isJust post_tag
                        then [strictText (fromJust post_tag)]
                        else []
            Right provid -> do -- is a province id
                prov_loc <- getProvLoc provid
                return . hsep $
                    (if isJust pre_prov
                        then [strictText (fromJust pre_prov)]
                        else []) ++
                    ["province", strictText prov_loc] ++
                    if isJust post_prov
                        then [strictText (fromJust post_prov)]
                        else []

-- Numeric statement. Allow additional text on both sides.
-- Don't add a + if the number is positive.
numeric :: NumType -> Text -> GenericStatement -> Text -> PP Doc
numeric ntype premsg (Statement _ rhs) postmsg
    | Just n <- floatRhs rhs = numeric' False ntype premsg n postmsg
numeric _ _ stmt _ = return $ pre_statement stmt

adjustNumber :: NumType -> Double -> Double
adjustNumber Plain          n = n
adjustNumber Reduced        n = n * 100
adjustNumber Percent        n = n
adjustNumber ReducedPercent n = n * 100

numeric' :: Bool -> NumType -> Text -> Double -> Text -> PP Doc
numeric' signed ntype premsg n postmsg
    = let num = adjustNumber ntype n
      in return . hsep $
            [strictText premsg
            ,(if signed -- assume negative will already get a "-"
                then if num >= 0 then "+" else mempty
                else mempty)
             <> pp_float num
             <> if ntype `elem` [Percent, ReducedPercent]
                 then "%"
                 else mempty
            ,strictText postmsg
            ]

numeric_or_tag :: Text -> Text -> GenericStatement -> Text -> PP Doc
numeric_or_tag pre quant (Statement _ rhs) post = do
    rest <- case rhs of
                IntRhs n -> return $ hsep ["at least", PP.int n, strictText post]
                FloatRhs n -> return $ hsep ["at least", pp_float n, strictText post]
                GenericRhs t -> do -- assume it's a tag
                    tflag <- flag t
                    return $ hsep ["at least as", strictText quant, strictText post, "as", tflag]
    return $ hsep [strictText pre, rest]

numeric_signed :: NumType -> Text -> GenericStatement -> Text -> PP Doc
numeric_signed ntype premsg (Statement _ rhs) postmsg
    = let n = case rhs of
                IntRhs n' -> fromIntegral n'
                FloatRhs n' -> n'
      in numeric' True ntype premsg n postmsg

-- "Has <something>"
has :: Text -> GenericStatement -> PP Doc
has what (Statement _ (GenericRhs yn)) | yn `elem` ["yes","no"]
    = return $ hsep
        [if yn == "yes" then "Has" else "Does NOT have"
        ,strictText what
        ]
has _ stmt = return $ pre_statement stmt

-- "Is <something>" (or "<Someone> is <something>")
is :: Maybe Text -> Text -> GenericStatement -> PP Doc
is who what (Statement _ (GenericRhs yn)) | yn `elem` ["yes","no"]
    = let know_who = isJust who
          no = yn == "no"
      in return . hsep $
            (if know_who
                then [strictText (fromJust who), "is"]
                else ["Is"]) ++
            (if no then ["NOT"] else []) ++
            [strictText what]
is _ _ stmt = return $ pre_statement stmt

-- "Has been <something>" (or "<Someone> has been <something>")
has_been :: Maybe Text -> Text -> GenericStatement -> PP Doc
has_been who what (Statement _ (GenericRhs yn)) | yn `elem` ["yes","no"]
    = let know_who = isJust who
          no = yn == "no"
      in return . hsep $
            (if know_who
                then [strictText (fromJust who), "has"]
                else ["Has"]) ++
            (if no then ["NOT"] else []) ++
            ["been", strictText what]
has_been _ _ stmt = return $ pre_statement stmt

-- "Is female" (= yes) or "Is male" (= no)
-- Better than "is NOT male" :)
is_female :: GenericStatement -> PP Doc
is_female (Statement _ (GenericRhs yn)) | yn `elem` ["yes","no"]
    = return $ hsep
        ["Ruler is"
        ,if yn == "yes" then "female" else "male"
        ]
is_female stmt = return $ pre_statement stmt

-- Generic statement referring to a country. Use a flag.
generic_tag :: Maybe Text -> GenericStatement -> Maybe Text -> PP Doc
generic_tag prefix (Statement _ (GenericRhs who)) suffix = do
    whoflag <- flag who
    return . hsep $
        (maybe [] ((:[]) . strictText) prefix) ++
        [whoflag] ++
        (maybe [] ((:[]) . strictText) suffix)
generic_tag _ stmt _ = return $ pre_statement stmt

-- Statement may have "yes"/"no" or a tag.
generic_tag_bool :: Text -> Text -> Maybe Text -> GenericStatement -> Maybe Text -> PP Doc
generic_tag_bool y_text n_text _ (Statement _ (GenericRhs "yes")) _ = return $ strictText y_text
generic_tag_bool y_text n_text _ (Statement _ (GenericRhs "no"))  _ = return $ strictText n_text
generic_tag_bool _ _ prefix stmt suffix = generic_tag prefix stmt suffix

numeric_icon :: NumType -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> GenericStatement -> PP Doc
numeric_icon numtype the_icon premesg what posttext (Statement _ rhs)
    = let amt = adjustNumber numtype $ case rhs of
            IntRhs n -> fromIntegral n
            FloatRhs n -> n
      in return . hsep $
               maybe [] ((:[]) . strictText) premesg
            ++ [icon the_icon]
            ++ maybe [] ((:[]) . strictText) what
            ++ [enclose "'''" "'''" $ (pp_float amt <>
                if numtype `elem` [Percent, ReducedPercent] then "%" else mempty)]
            ++ maybe [] ((:[]) . strictText) posttext

---------------------------------
-- Specific statement handlers --
---------------------------------

data FactionInfluence = FactionInfluence {
        faction :: Maybe Text
    ,   influence :: Maybe Double
    }
newInfluence = FactionInfluence Nothing Nothing
faction_influence :: GenericStatement -> PP Doc
faction_influence stmt@(Statement _ (CompoundRhs scr))
    = return . pp_influence $ foldl' addField newInfluence scr
    where
        pp_influence inf =
            if isJust (faction inf) && isJust (influence inf)
            then
                let fac = case fromJust (faction inf) of
                            -- Celestial empire
                            "enuchs" {- sic -} -> "eunuchs influence"
                            "temples" -> "temples influence"
                            "bureaucrats" -> "bureaucrats influence"
                            -- Merchant republic
                            "mr_aristocrats" -> "aristocrats influence"
                            "mr_guilds" -> "guilds influence"
                            "mr_traders" -> "traders influence"
                in hsep
                    [icon fac
                    -- Influence can be good or bad depending on the country's
                    -- situation, so leave it neutral.
                    ,bold (pp_signed pp_float . fromJust $ influence inf)
                    ,text ((\(Just (c,cs)) -> TL.fromStrict $ T.cons (toUpper c) cs) $ T.uncons fac)
                    ]
            else pre_statement stmt
        addField :: FactionInfluence -> GenericStatement -> FactionInfluence
        addField inf (Statement (GenericLhs "faction") (GenericRhs fac)) = inf { faction = Just fac }
        addField inf (Statement (GenericLhs "influence") (FloatRhs amt)) = inf { influence = Just amt }
        addField inf (Statement (GenericLhs "influence") (IntRhs amt)) = inf { influence = Just (fromIntegral amt) }
        addField inf _ = inf -- unknown statement

add_years_of_income :: GenericStatement -> PP Doc
add_years_of_income stmt
    | Statement _ (IntRhs n)   <- stmt = add_years_of_income' (fromIntegral n)
    | Statement _ (FloatRhs n) <- stmt = add_years_of_income' n
    where
        add_years_of_income' howmuch = return $ hsep
            [if howmuch < 0 then "Lose" else "Gain"
            ,icon "ducats"
            ,"ducats", "equal", "to"
            ,pp_float (abs howmuch)
            ,if abs howmuch == 1 then "year" else "years"
            ,"of", "income"
            ]

data NumType         -- Treat 1 as:
    = Plain          -- 1
    | Reduced        -- 100
    | Percent        -- 1%
    | ReducedPercent -- 100%
    deriving (Show, Eq, Ord)

-- "Gain" or "Lose" simple numbers, e.g. army tradition.
-- NumType determines how to treat the quantity (percentage or not, reduced or not).
-- First text argument is the icon key (or Nothing if none available).
-- Second text argument is text to show after it.
-- Second Bool is whether a gain is good.
gain :: NumType -> Maybe Text -> Bool -> Maybe Text -> Text -> GenericStatement -> PP Doc
gain numtype mwho good iconkey what stmt@(Statement _ rhs) = return $
    if isJust mhowmuch then hsep $
        (if know_who then [strictText who] else [])
        ++
        [gain_or_lose]
        ++ (if isJust iconkey then [icon (fromJust iconkey)] else [])
        ++
        [(if numtype `elem` [Percent, ReducedPercent] then pp_hl_pc else pp_hl_num)
            good pp_num_sep
            (if numtype `elem` [Reduced, ReducedPercent] then howmuch * 100 else howmuch)
        ,strictText what
        ]
    else pre_statement stmt
    where
        know_who = isJust mwho
        who = fromJust mwho
        mhowmuch = floatRhs rhs
        howmuch :: Double
        howmuch = fromJust mhowmuch
        gain_or_lose =
            if know_who
                then if howmuch < 0 then "loses" else "gains"
                else if howmuch < 0 then "Lose" else "Gain"

data Modifier = Modifier {
        mod_name :: Maybe Text
    ,   mod_key :: Maybe Text
    ,   mod_who :: Maybe Text
    ,   mod_duration :: Maybe Double
    ,   mod_power :: Maybe Double
    } deriving Show
newModifier = Modifier Nothing Nothing Nothing Nothing Nothing

addModifierLine :: Modifier -> GenericStatement -> Modifier 
addModifierLine apm (Statement (GenericLhs "name") (GenericRhs name)) = apm { mod_name = Just name }
addModifierLine apm (Statement (GenericLhs "name") (StringRhs name)) = apm { mod_name = Just name }
addModifierLine apm (Statement (GenericLhs "key") (GenericRhs key)) = apm { mod_key = Just key }
addModifierLine apm (Statement (GenericLhs "key") (StringRhs key)) = apm { mod_key = Just key }
addModifierLine apm (Statement (GenericLhs "who") (GenericRhs tag)) = apm { mod_who = Just tag }
addModifierLine apm (Statement (GenericLhs "who") (StringRhs tag)) = apm { mod_who = Just tag }
addModifierLine apm (Statement (GenericLhs "duration") (FloatRhs duration)) = apm { mod_duration = Just duration }
addModifierLine apm (Statement (GenericLhs "power") (FloatRhs power)) = apm { mod_power = Just power }
addModifierLine apm _ = apm -- e.g. hidden = yes

maybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
maybeM f x = maybe (return Nothing) (liftM Just . f) x

add_modifier :: Text -> GenericStatement -> PP Doc
add_modifier kind stmt@(Statement _ (CompoundRhs scr))
    = let mod = foldl' addModifierLine newModifier scr
      in if isJust (mod_name mod) || isJust (mod_key mod) then do
            let dur = fromJust (mod_duration mod)
                power = fromJust (mod_power mod)
            key_loc <- maybeM getGameL10n (mod_key mod)
            name_loc <- maybeM getGameL10n (mod_name mod)
            mwho <- maybe (return Nothing) (fmap Just . flag) (mod_who mod)
            return . hsep $
                [if isJust mwho
                    then hsep [fromJust mwho, "gains"]
                    else "Add"
                ,strictText kind, "modifier"
                ,dquotes (strictText
                    (if isJust name_loc then fromJust name_loc
                     else if isJust key_loc then fromJust key_loc
                     else "<unspecified>"))
                ]
                ++ (if isJust (mod_power mod)
                    then [parens (hsep [pp_hl_num True pp_float power, "Power"])]
                    else [])
                ++ if isJust (mod_duration mod) then
                    if dur < 0 then ["indefinitely"] else
                    ["for"
                    ,pp_float dur
                    ,"days"
                    ]
                else []
         else return $ pre_statement stmt
add_modifier _ stmt = return $ pre_statement stmt

has_modifier :: Text -> GenericStatement -> PP Doc
has_modifier kind (Statement _ (GenericRhs label))
    = (\label_loc -> hsep
        ["Has", strictText kind, "modifier"
        ,dquotes (strictText label_loc)
        ])
      <$> getGameL10n label
has_modifier kind stmt@(Statement _ (CompoundRhs scr))
    = let mod = foldl' addModifierLine newModifier scr
      in case mod_key mod of
            Just key -> do
                label_loc <- getGameL10n key
                whoflag <- maybe (return Nothing) (fmap Just . flag) (mod_who mod)
                return . hsep $
                    (if isJust whoflag
                        then [fromJust whoflag, "has"]
                        else ["Has"])
                    ++
                    [strictText kind, "modifier"
                    ,dquotes (strictText label_loc)
                    ]
            _ -> return $ pre_statement stmt
has_modifier _ stmt = return $ pre_statement stmt

remove_modifier :: Text -> GenericStatement -> PP Doc
remove_modifier kind (Statement _ (GenericRhs label))
    = (\label_loc -> hsep
        ["Remove", strictText kind, "modifier"
        ,dquotes (strictText label_loc)
        ])
      <$> getGameL10n label
remove_modifier _ stmt = return $ pre_statement stmt

-- "add_core = <n>" in country scope means "Gain core on <localize PROVn>"
-- "add_core = <tag>" in province scope means "<localize tag> gains core"
add_core :: GenericStatement -> PP Doc
add_core (Statement _ (GenericRhs tag)) = do -- tag
    tagflag <- flag tag
    return $ hsep [tagflag, "gains", "core"]
add_core (Statement _ (IntRhs num)) = do -- province
    prov <- getProvLoc num
    return $ hsep ["Gain", "core", "on", "province", strictText prov]
add_core (Statement _ (FloatRhs num)) = do -- province
    prov <- getProvLoc (round num)
    return $ hsep ["Gain", "core", "on", "province", strictText prov]
add_core stmt = return $ pre_statement stmt

-- Add an opinion modifier towards someone (for a number of years).
data AddOpinion = AddOpinion {
        who :: Maybe Text
    ,   modifier :: Maybe Text
    ,   op_years :: Maybe Double
    } deriving Show
newAddOpinion = AddOpinion Nothing Nothing Nothing

opinion :: Text -> GenericStatement -> PP Doc
opinion verb stmt@(Statement _ (CompoundRhs scr))
    = pp_add_opinion $ foldl' addLine newAddOpinion scr
    where
        addLine :: AddOpinion -> GenericStatement -> AddOpinion
        addLine op (Statement (GenericLhs "who") (GenericRhs tag))
            = op { who = Just tag }
        addLine op (Statement (GenericLhs "modifier") (GenericRhs label))
            = op { modifier = Just label }
        addLine op (Statement (GenericLhs "years") (FloatRhs n))
            = op { op_years = Just n }
        addLine op (Statement (GenericLhs "years") (IntRhs n))
            = op { op_years = Just (fromIntegral n) }
        addLine op _ = op
        pp_add_opinion op
            = if isJust (who op) && isJust (modifier op) then do
                let whom = fromJust (who op)
                    mod = fromJust (modifier op)
                whomflag <- flag whom
                mod <- getGameL10n (fromJust (modifier op))
                return . hsep $
                    [strictText verb
                    ,"opinion modifier"
                    ,dquotes $ strictText mod
                    ,"towards"
                    ,whomflag
                    ]
                    ++ if isNothing (op_years op) then [] else
                    ["for"
                    ,pp_float (fromJust (op_years op))
                    ,"years"
                    ]
              else return $ pre_statement stmt
add_opinion _ stmt = pre_statement stmt

-- Render a rebel type atom (e.g. anti_tax_rebels) as their name and icon key.
-- This is needed because all religious rebels localize as simply "Religious" -
-- we want to be more specific.
rebel_loc :: HashMap Text (Text,Text)
rebel_loc = HM.fromList
        [("polish_noble_rebels",    ("Magnates", "magnates"))
        ,("lollard_rebels",         ("Lollard zealots", "lollards"))
        ,("catholic_rebels",        ("Catholic zealots", "catholic zealots"))
        ,("protestant_rebels",      ("Protestant zealots", "protestant zealots"))
        ,("reformed_rebels",        ("Reformed zealots", "reformed zealots"))
        ,("orthodox_rebels",        ("Orthodox zealots", "orthodox zealots"))
        ,("sunni_rebels",           ("Sunni zealots", "sunni zealots"))
        ,("shiite_rebels",          ("Shiite zealots", "shiite zealots"))
        ,("buddhism_rebels",        ("Buddhist zealots", "buddhist zealots"))
        ,("mahayana_rebels",        ("Mahayana zealots", "mahayana zealots"))
        ,("vajrayana_rebels",       ("Vajrayana zealots", "vajrayana zealots"))
        ,("hinduism_rebels",        ("Hindu zealots", "hindu zealots"))
        ,("confucianism_rebels",    ("Confucian zealots", "confucian zealots"))
        ,("shinto_rebels",          ("Shinto zealots", "shinto zealots"))
        ,("animism_rebels",         ("Animist zealots", "animist zealots"))
        ,("shamanism_rebels",       ("Shamanist zealots", "shamanist zealots"))
        ,("totemism_rebels",        ("Totemist zealots", "totemist zealots"))
        ,("coptic_rebels",          ("Coptic zealots", "coptic zealots"))
        ,("ibadi_rebels",           ("Ibadi zealots", "ibadi zealots"))
        ,("sikhism_rebels",         ("Sikh zealots", "sikh zealots"))
        ,("jewish_rebels",          ("Jewish zealots", "jewish zealots"))
        ,("norse_pagan_reformed_rebels", ("Norse zealots", "norse zealots"))
        ,("inti_rebels",            ("Inti zealots", "inti zealots"))
        ,("maya_rebels",            ("Maya zealots", "maya zealots"))
        ,("nahuatl_rebels",         ("Nahuatl zealots", "nahuatl zealots"))
        ,("tengri_pagan_reformed_rebels", ("Tengri zealots", "tengri zealots"))
        ,("zoroastrian_rebels",     ("Zoroastrian zealots", "zoroastrian zealots"))
        ,("ikko_ikki_rebels",       ("Ikko-Ikkis", "ikko-ikkis"))
        ,("ronin_rebels",           ("Ronin", "ronin"))
        ,("reactionary_rebels",     ("Reactionaries", "reactionaries"))
        ,("anti_tax_rebels",        ("Peasant rabble", "peasants"))
        ,("revolutionary_rebels",   ("Revolutionaries", "revolutionaries"))
        ,("heretic_rebels",         ("Heretics", "heretics"))
        ,("religious_rebels",       ("Religious zealots", "religious zealots"))
        ,("nationalist_rebels",     ("Separatists", "separatists"))
        ,("noble_rebels",           ("Noble rebels", "noble rebels"))
        ,("colonial_rebels",        ("Colonial rebels", "colonial rebels")) -- ??
        ,("patriot_rebels",         ("Patriot", "patriot"))
        ,("pretender_rebels",       ("Pretender", "pretender"))
        ,("colonial_patriot_rebels", ("Colonial Patriot", "colonial patriot")) -- ??
        ,("particularist_rebels",   ("Particularist", "particularist"))
        ,("nationalist_rebels",   ("Nationalist", "separatists"))
        ]

-- Spawn a rebel stack.
data SpawnRebels = SpawnRebels {
        rebelType :: Maybe Text
    ,   rebelSize :: Maybe Double
    ,   friend :: Maybe Text
    ,   win :: Maybe Bool
    ,   unrest :: Maybe Double -- rebel faction progress
    ,   leader :: Maybe Text
    } deriving Show
newSpawnRebels = SpawnRebels Nothing Nothing Nothing Nothing Nothing Nothing

spawn_rebels :: Maybe Text -> GenericStatement  -> PP Doc
spawn_rebels mtype stmt = spawn_rebels' mtype stmt where
    spawn_rebels' Nothing stmt@(Statement _ (CompoundRhs scr))
        = pp_spawn_rebels $ foldl' addLine newSpawnRebels scr
    spawn_rebels' rtype stmt@(Statement _ (IntRhs size))
        = pp_spawn_rebels $ newSpawnRebels { rebelType = rtype, rebelSize = Just (fromIntegral size) }
    spawn_rebels' rtype stmt@(Statement _ (FloatRhs size))
        = pp_spawn_rebels $ newSpawnRebels { rebelType = rtype, rebelSize = Just size }

    addLine :: SpawnRebels -> GenericStatement -> SpawnRebels
    addLine op (Statement (GenericLhs "type") (GenericRhs tag))
        = op { rebelType = Just tag }
    addLine op (Statement (GenericLhs "size") (FloatRhs n))
        = op { rebelSize = Just n }
    addLine op (Statement (GenericLhs "friend") (GenericRhs tag))
        = op { friend = Just tag }
    addLine op (Statement (GenericLhs "win") (GenericRhs "yes"))
        = op { win = Just True }
    addLine op (Statement (GenericLhs "unrest") (FloatRhs n))
        = op { unrest = Just n }
    addLine op (Statement (GenericLhs "leader") (StringRhs name))
        = op { leader = Just name }
    addLine op _ = op

    pp_spawn_rebels :: SpawnRebels -> PP Doc
    pp_spawn_rebels reb
        = if isJust (rebelSize reb) then do
            let hasType = isJust (rebelType reb)
                rtype = fromJust (rebelType reb)
                rsize = fromJust (rebelSize reb)
                friendlyTo = fromJust (friend reb) -- not evaluated if Nothing
                reb_unrest = fromJust (unrest reb)
                (rtype_loc, rtype_icon) = fromJust $ HM.lookup rtype rebel_loc
            friendlyFlag <- flag friendlyTo
            return ((hsep $
                   (if hasType
                        then [icon rtype_icon, strictText rtype_loc, "rebels"]
                        else ["Rebels"])
                   ++
                   [PP.parens $ hsep ["size", pp_float (fromJust (rebelSize reb))]]
                   ++ (if isJust (friend reb) then
                           [PP.parens $ hsep ["friendly", "to", friendlyFlag]]
                       else [])
                   ++ (if isJust (leader reb) then
                           [hsep ["led", "by", strictText (fromJust (leader reb))]]
                       else [])
                   ++
                   ["rise in revolt"]
                   ++ (if isJust (win reb) && fromJust (win reb) then
                           [hsep ["and", "occupy", "the", "province"]]
                       else [])
                ) <> if isJust (unrest reb) then
                hsep
                   [","
                   ,"gaining"
                   ,pp_float reb_unrest
                   ,hsep ["progress","towards","the","next","uprising"]
                   ]
                else mempty)
        else return $ pre_statement stmt

has_spawned_rebels :: GenericStatement -> PP Doc
has_spawned_rebels (Statement _ (GenericRhs rtype))
    = let (rtype_loc, rtype_iconkey) = fromJust $ HM.lookup rtype rebel_loc
      in return $ hsep
            [icon rtype_iconkey
            ,strictText rtype_loc
            ,"have risen in revolt"
            ]

can_spawn_rebels :: GenericStatement -> PP Doc
can_spawn_rebels (Statement _ (GenericRhs rtype))
    = let (rtype_loc, rtype_iconkey) = fromJust $ HM.lookup rtype rebel_loc
      in return $ hsep
            ["Province has"
            ,icon rtype_iconkey
            ,strictText rtype_loc
            ,"rebels"
            ]

data TriggerEvent = TriggerEvent
        { e_id :: Maybe Text
        , e_title_loc :: Maybe Text
        , e_days :: Maybe Int
        }
newTriggerEvent = TriggerEvent Nothing Nothing Nothing
trigger_event :: Text -> GenericStatement -> PP Doc
trigger_event category stmt@(Statement _ (CompoundRhs scr))
    = pp_trigger_event =<< foldM addLine newTriggerEvent scr
    where
        addLine :: TriggerEvent -> GenericStatement -> PP TriggerEvent
        addLine evt (Statement (GenericLhs "id") (GenericRhs id))
            = (\t_loc -> evt { e_id = Just id, e_title_loc = t_loc })
              <$> getGameL10nIfPresent (id <> ".t")
        addLine evt (Statement (GenericLhs "days") rhs) = return $ case rhs of
            IntRhs n -> evt { e_days = Just n }
            FloatRhs n -> evt { e_days = Just (round n) }
        addLine evt _ = return evt
        pp_trigger_event :: TriggerEvent -> PP Doc
        pp_trigger_event evt
            = let have_loc = isJust (e_title_loc evt)
                  have_days = isJust (e_days evt)
                  mid = e_id evt
                  loc = e_title_loc evt
                  days = e_days evt
              in if isJust mid then return . hsep $
                    ["Trigger"
                    ,strictText category
                    ,"event"
                    ,dquotes (strictText (if have_loc then fromJust loc else fromJust mid))
                    ,"<!--", strictText (fromJust mid), "-->"
                    ] ++ if have_days then
                        ["in"
                        ,PP.int (fromJust days)
                        ,"day(s)"
                        ]
                    else []
                 else return $ pre_statement stmt

gain_manpower :: GenericStatement -> PP Doc
gain_manpower (Statement _ rhs) =
    let amt = case rhs of
            IntRhs n -> fromIntegral n
            FloatRhs n -> n
        gain_or_lose = if amt < 0 then "Lose" else "Gain"
    in if abs amt < 1
        --  if abs amt < 1, interpret amt as a fraction of max
        --  if abs amt >= 1, interpret amt as a multiple of 1,000
        then return $ hsep
                [gain_or_lose
                ,icon "manpower"
                ,"manpower equal to"
                ,pp_hl_num True pp_float amt <> "%"
                ,"of maximum"
                ]
        else return $ hsep
                [gain_or_lose
                ,pp_hl_num True pp_float amt
                ,"manpower"
                ]


data AddCB = AddCB
    {   acb_target :: Maybe Text
    ,   acb_target_loc :: Maybe Text
    ,   acb_type :: Maybe Text
    ,   acb_type_loc :: Maybe Text
    ,   acb_months :: Maybe Double
    }
newAddCB = AddCB Nothing Nothing Nothing Nothing Nothing
-- "direct" is False for reverse_add_casus_belli
add_casus_belli :: Bool -> GenericStatement -> PP Doc
add_casus_belli direct stmt@(Statement _ (CompoundRhs scr))
    = pp_add_cb =<< foldM addLine newAddCB scr where
        addLine :: AddCB -> GenericStatement -> PP AddCB
        addLine acb (Statement (GenericLhs "target") (GenericRhs target))
            = (\target_loc -> acb
                  { acb_target = Just target
                  , acb_target_loc = target_loc })
              <$> getGameL10nIfPresent target
        addLine acb (Statement (GenericLhs "type") (GenericRhs cbtype))
            = (\cbtype_loc -> acb
                  { acb_type = Just cbtype
                  , acb_type_loc = cbtype_loc })
              <$> getGameL10nIfPresent cbtype
        addLine acb (Statement (GenericLhs "months") rhs)
            = return $ acb { acb_months = Just months }
            where months = case rhs of
                    IntRhs n -> fromIntegral n
                    FloatRhs n -> n
        pp_add_cb :: AddCB -> PP Doc
        pp_add_cb acb
            = let has_target = isJust (acb_target acb)
                  has_type = isJust (acb_type acb)
                  has_months = isJust (acb_months acb)
                  target_loc = maybe (fromJust (acb_target acb)) id (acb_target_loc acb)
                  type_loc = maybe (fromJust (acb_type acb)) id (acb_type_loc acb)
                  months = fromJust (acb_months acb)
              in if has_target && has_type
                 then return . hsep $
                       (if not direct then
                            ["Gain"
                            ,dquotes (strictText type_loc)
                            ,"casus belli against"
                            ,strictText target_loc
                            ]
                        else
                            [strictText target_loc
                            ,"gains"
                            ,dquotes (strictText type_loc)
                            ,"casus belli"
                            ]
                        ) ++
                        if has_months then
                            ["for"
                            ,pp_float months
                            ,"months"
                            ]
                        else []
                 else return $ pre_statement stmt

random :: GenericStatement -> PP Doc
random stmt@(Statement _ (CompoundRhs scr))
    | (front, back) <- break
                        (\stmt -> case stmt of 
                            Statement (GenericLhs "chance") _ -> True
                            _ -> False)
                        scr
      , not (null back) =
        let chance = case head back of
                Statement _ (IntRhs n) -> fromIntegral n
                Statement _ (FloatRhs n) -> n
            defaultdoc = pre_statement stmt
            compound = generic_compound defaultdoc
        in generic_compound_doc
                (pre_statement stmt)
                (hsep [pp_float chance <> "%","chance of"])
                (Statement undefined (CompoundRhs (front ++ tail back)))
    | otherwise = do
        scr_pp'd <- indentUp (pp_script scr)
        return $ hcat ["At random:", line, scr_pp'd]
random stmt = return $ pre_statement stmt

data DefineAdvisor = DefineAdvisor
    {   da_type :: Maybe Text
    ,   da_type_loc :: Maybe Text
    ,   da_name :: Maybe Text
    ,   da_discount :: Maybe Bool
    ,   da_location :: Maybe Int
    ,   da_location_loc :: Maybe Text
    ,   da_skill :: Maybe Int
    ,   da_female :: Maybe Bool
    }
newDefineAdvisor = DefineAdvisor Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

define_advisor :: GenericStatement -> PP Doc
define_advisor stmt@(Statement _ (CompoundRhs scr))
    = pp_define_advisor =<< foldM addLine newDefineAdvisor scr where
        addLine :: DefineAdvisor -> GenericStatement -> PP DefineAdvisor
        addLine da stmt@(Statement (GenericLhs lhs) rhs) = case T.map toLower lhs of
            "type" ->
                let mthe_type = case rhs of
                        GenericRhs a_type -> Just a_type
                        StringRhs a_type -> Just a_type
                        _ -> Nothing
                in (\mtype_loc -> da
                        { da_type = mthe_type
                        , da_type_loc = mtype_loc })
                   <$> (maybe (return Nothing) getGameL10nIfPresent mthe_type)
            "name" -> return $
                let mthe_name = case rhs of
                        GenericRhs a_name -> Just a_name
                        StringRhs a_name -> Just a_name
                        _ -> Nothing
                in da { da_name = mthe_name }
            "discount" -> return $
                let yn = case rhs of
                        GenericRhs yn' -> Just yn'
                        StringRhs yn' -> Just yn'
                        _ -> Nothing
                in if yn == Just "yes" then da { da_discount = Just True }
                   else if yn == Just "no" then da { da_discount = Just False }
                   else da
            "location" -> do
                let location_code :: Maybe Int
                    location_code = case rhs of
                        IntRhs code -> Just code
                        -- Province ID isn't supposed to be float, but it's
                        -- parsed that way.
                        FloatRhs code -> Just $ round code
                        _ -> Nothing
                location_loc <- sequence (getProvLoc <$> location_code)
                return $ da { da_location = location_code
                            , da_location_loc = location_loc }
            "skill" -> return $ da { da_skill = round `fmap` (floatRhs rhs::Maybe Double) }
            "female" -> return $
                let yn = case rhs of
                        GenericRhs yn' -> Just yn'
                        StringRhs yn' -> Just yn'
                        _ -> Nothing
                in if yn == Just "yes" then da { da_female = Just True }
                   else if yn == Just "no" then da { da_female = Just False }
                   else da
        pp_define_advisor :: DefineAdvisor -> PP Doc
        pp_define_advisor da = return $
            let has_type = isJust (da_type da)
                thetype = fromJust (da_type da)
                has_type_loc = isJust (da_type_loc da)
                type_loc = fromJust (da_type_loc da)
                has_name = isJust (da_name da)
                name = fromJust (da_name da)
                has_discount = isJust (da_discount da)
                discount = fromJust (da_discount da)
                has_location = isJust (da_location da)
                location = fromJust (da_location da)
                has_location_loc = isJust (da_location_loc da)
                location_loc = fromJust (da_location_loc da)
                has_skill = isJust (da_skill da)
                skill = fromJust (da_skill da)
                has_female = isJust (da_female da)
                female = fromJust (da_female da)
            in if has_skill then hsep $
                ["Gain skill"
                ,PP.int skill
                ]
                ++ if has_female
                    then if female
                        then ["female"]
                        else ["male"]
                    else []
                ++ if has_type
                    then [strictText $
                        if has_type_loc
                            then type_loc
                            else thetype]
                    else []
                ++ ["advisor"]
                ++
                (if has_name
                    then ["named", strictText name]
                    else [])
                ++
                (if has_location
                    then ["in",
                          if has_location_loc
                            then strictText location_loc
                            else text . TL.pack . show $ location]
                    else [])
                ++
                (if has_discount
                    then [if discount
                            then "(50% cheaper than normal to employ)"
                            else "(at normal salary)"]
                    else [])
             else pre_statement stmt

data DefineRuler = DefineRuler
    {   dr_name :: Maybe Text
    ,   dr_dynasty :: Maybe Text -- can be a tag/pronoun
    ,   dr_age :: Maybe Double
    ,   dr_female :: Maybe Bool
    ,   dr_claim :: Maybe Double
    ,   dr_regency :: Maybe Bool
    ,   dr_adm :: Maybe Int
    ,   dr_dip :: Maybe Int
    ,   dr_mil :: Maybe Int
    ,   dr_fixed :: Maybe Bool
    }
newDefineRuler = DefineRuler Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

define_ruler :: GenericStatement -> PP Doc
define_ruler stmt@(Statement _ (CompoundRhs scr))
    = return . pp_define_ruler $ foldl' addLine newDefineRuler scr where
        addLine :: DefineRuler -> GenericStatement -> DefineRuler
        addLine dr stmt@(Statement (GenericLhs lhs) rhs) = case T.map toLower lhs of
            "name" ->
                let mthe_name = case rhs of
                        GenericRhs a_name -> Just a_name
                        StringRhs a_name -> Just a_name
                        _ -> Nothing
                in dr { dr_name = mthe_name }
            "dynasty" ->
                let mthe_dynasty = case rhs of
                        GenericRhs a_dynasty -> Just a_dynasty
                        StringRhs a_dynasty -> Just a_dynasty
                        _ -> Nothing
                in dr { dr_dynasty = mthe_dynasty }
            "age" ->
                let mage = floatRhs rhs
                in  dr { dr_age = mage }
            "claim" ->
                let mclaim = floatRhs rhs
                in  dr { dr_claim = mclaim }
            "adm" ->
                let madm = floatRhs rhs
                in  dr { dr_adm = madm }
            "dip" ->
                let mdip = floatRhs rhs
                in  dr { dr_dip = mdip }
            "mil" ->
                let mmil = floatRhs rhs
                in  dr { dr_mil = mmil }
            "regency" -> case rhs of
                GenericRhs "yes" -> dr { dr_regency = Just True }
                GenericRhs "no" -> dr { dr_regency = Just False }
                _ -> dr
            "fixed" -> case rhs of
                GenericRhs "yes" -> dr { dr_fixed = Just True }
                GenericRhs "no" -> dr { dr_fixed = Just False }
                _ -> dr
            "attach_leader" -> dr -- not sure what this means
            "female" ->
                let yn = case rhs of
                        GenericRhs yn' -> Just yn'
                        StringRhs yn' -> Just yn'
                        _ -> Nothing
                in if yn == Just "yes" then dr { dr_female = Just True }
                   else if yn == Just "no" then dr { dr_female = Just False }
                   else dr
        pp_define_ruler :: DefineRuler -> Doc
        pp_define_ruler dr =
            let has_name = isJust (dr_name dr)
                name = fromJust (dr_name dr)
                has_dynasty = isJust (dr_dynasty dr)
                dynasty = fromJust (dr_dynasty dr)
                has_age = isJust (dr_age dr)
                age = fromJust (dr_age dr)
                has_female = isJust (dr_female dr)
                female = fromJust (dr_female dr)
                has_claim = isJust (dr_claim dr)
                claim = fromJust (dr_claim dr)
                has_regency = isJust (dr_regency dr)
                regency = fromJust (dr_regency dr)
                has_adm = isJust (dr_adm dr)
                adm = fromJust (dr_adm dr)
                has_dip = isJust (dr_dip dr)
                dip = fromJust (dr_dip dr)
                has_mil = isJust (dr_mil dr)
                mil = fromJust (dr_mil dr)
                has_fixed = isJust (dr_fixed dr)
                fixed = fromJust (dr_fixed dr)
            in hsep $
                ["A new"]
                ++ (if has_age
                    then [pp_float age, "year old"]
                    else [])
                ++ (if has_female
                    then [if female then "female" else "male"]
                    else [])
                ++ ["ruler"]
                ++ (if has_dynasty
                    then ["of the"
                         ,strictText dynasty
                         ,"dynasty"]
                    else [])
                ++ (if has_name
                    then ["named"
                         ,strictText name]
                    else [])
                ++ ["comes to power"]
                ++ (if has_regency
                    then [if regency then "under" else "without", "a regency council"]
                    else [])
                ++ (if has_adm || has_dip || has_mil
                    then ["with"]
                        ++ if has_fixed
                            then if fixed
                                then ["fixed"]
                                else ["flexible"]
                            else []
                        ++ [hcat . intersperse (hcat [",", space]) . map hsep . filter (not . null) $
                            [if has_adm
                                then [icon "adm", PP.int adm]
                                else []
                            , if has_dip
                                then [icon "dip", PP.int dip]
                                else []
                            , if has_mil
                                then [icon "mil", PP.int mil]
                                else []
                            ]]
                    else [])
define_ruler stmt = return $ pre_statement stmt

data HadFlag = HadFlag
    {   hf_flag :: Maybe Text
    ,   hf_days :: Maybe Int
    }
newHadFlag = HadFlag Nothing Nothing

had_flag :: Text -> GenericStatement -> PP Doc
had_flag category stmt@(Statement _ (CompoundRhs scr))
    = return . pp_had_flag $ foldl' addLine newHadFlag scr where
        addLine :: HadFlag -> GenericStatement -> HadFlag
        addLine dr stmt@(Statement (GenericLhs lhs) rhs) = case T.map toLower lhs of
            "flag" -> case rhs of
                GenericRhs flagname -> dr { hf_flag = Just flagname }
                StringRhs flagname -> dr { hf_flag = Just flagname }
                _ -> dr
            "days" -> dr { hf_days = floatRhs rhs }
            _ -> dr
        pp_had_flag :: HadFlag -> Doc
        pp_had_flag dr
            = if isJust (hf_flag dr) && isJust (hf_days dr)
              then hsep
                    ["Has had"
                    ,strictText category
                    ,"flag"
                    ,enclose "<tt>" "</tt>" (strictText (fromJust (hf_flag dr)))
                    ,"for at least"
                    ,PP.int (fromJust (hf_days dr))
                    ,"days"]
              else pre_statement stmt

data BuildToForcelimit = BuildToForcelimit
    {   btf_infantry :: Maybe Double
    ,   btf_cavalry :: Maybe Double
    ,   btf_artillery :: Maybe Double
    ,   btf_heavy_ship :: Maybe Double
    ,   btf_light_ship :: Maybe Double
    ,   btf_galley :: Maybe Double
    ,   btf_transport :: Maybe Double
    }
newBuildToForcelimit = BuildToForcelimit Nothing Nothing Nothing Nothing Nothing Nothing Nothing

build_to_forcelimit :: GenericStatement -> PP Doc
build_to_forcelimit stmt@(Statement _ (CompoundRhs scr))
    = pp_build_to_forcelimit $ foldl' addLine newBuildToForcelimit scr where
        addLine :: BuildToForcelimit -> GenericStatement -> BuildToForcelimit
        addLine dr stmt@(Statement (GenericLhs lhs) rhs)
            = let mhowmuch = floatRhs rhs
                  howmuch = fromJust mhowmuch
              in if isNothing mhowmuch
                 then dr
                 else case T.map toLower lhs of
                    "infantry"   -> dr { btf_infantry   = Just howmuch }
                    "cavalry"    -> dr { btf_cavalry    = Just howmuch }
                    "artillery"  -> dr { btf_artillery  = Just howmuch }
                    "heavy_ship" -> dr { btf_heavy_ship = Just howmuch }
                    "light_ship" -> dr { btf_light_ship = Just howmuch }
                    "galley"     -> dr { btf_galley     = Just howmuch }
                    "transport"  -> dr { btf_transport  = Just howmuch }
                    _ -> dr
        pp_build_to_forcelimit :: BuildToForcelimit -> PP Doc
        pp_build_to_forcelimit dr = withCurrentIndent $ \indent -> do
            let has_infantry = isJust (btf_infantry dr)
                infantry = fromJust (btf_infantry dr)
                has_cavalry = isJust (btf_cavalry dr)
                cavalry = fromJust (btf_cavalry dr)
                has_artillery = isJust (btf_artillery dr)
                artillery = fromJust (btf_artillery dr)
                has_heavy_ship = isJust (btf_heavy_ship dr)
                heavy_ship = fromJust (btf_heavy_ship dr)
                has_light_ship = isJust (btf_light_ship dr)
                light_ship = fromJust (btf_light_ship dr)
                has_galley = isJust (btf_galley dr)
                galley = fromJust (btf_galley dr)
                has_transport = isJust (btf_transport dr)
                transport = fromJust (btf_transport dr)
                newindent = succ indent
                has_X :: (Bool, Double, Text, Text) -> [Doc]
                has_X (hasit, howmuch, iconkey, text)
                    = if hasit then
                          [line
                          ,hcat (replicate newindent "*"), space
                          ,pp_float (howmuch*100),"%", space
                          ,icon iconkey, space
                          ,strictText text]
                        else []
            return . hcat $
                ["Build units up to forcelimit:"]
                ++ concatMap has_X
                [(has_infantry, infantry, "infantry", "infantry")
                ,(has_cavalry, cavalry, "cavalry", "cavalry")
                ,(has_artillery, artillery, "artillery", "artillery")
                ,(has_heavy_ship, heavy_ship, "heavy ship", "heavy ships")
                ,(has_light_ship, light_ship, "light ship", "light ships")
                ,(has_galley, galley, "galley", "galleys")
                ,(has_transport, transport, "transport", "transports")
                ]

data DeclareWarWithCB = DeclareWarWithCB
    {   dwcb_who :: Maybe Text
    ,   dwcb_cb :: Maybe Text
    }
newDeclareWarWithCB = DeclareWarWithCB Nothing Nothing

declare_war_with_cb :: GenericStatement -> PP Doc
declare_war_with_cb stmt@(Statement _ (CompoundRhs scr))
    = pp_declare_war_with_cb $ foldl' addLine newDeclareWarWithCB scr where
        addLine :: DeclareWarWithCB -> GenericStatement -> DeclareWarWithCB
        addLine dwcb stmt@(Statement (GenericLhs lhs) (GenericRhs rhs))
            = case T.map toLower lhs of
                "who"         -> dwcb { dwcb_who = Just rhs }
                "casus_belli" -> dwcb { dwcb_cb  = Just rhs }
                _ -> dwcb
        pp_declare_war_with_cb :: DeclareWarWithCB -> PP Doc
        pp_declare_war_with_cb dwcb
            = let has_who = isJust (dwcb_who dwcb)
                  who = fromJust (dwcb_who dwcb)
                  has_cb = isJust (dwcb_cb dwcb)
                  cb = fromJust (dwcb_cb dwcb)
              in if has_who && has_cb
                 then do
                    whoflag <- flag who
                    cb_loc <- getGameL10n cb
                    return . hsep $
                      ["Declare war on"
                      ,whoflag
                      ,"using"
                      ,dquotes (strictText cb_loc)
                      ,"casus belli"
                      ]
                 else return $ pre_statement stmt

has_dlc :: GenericStatement -> PP Doc
has_dlc (Statement _ (StringRhs dlc))
    = return . hsep $
           ["DLC"]
           ++ dlc_icon
           ++
           [strictText dlc
           ,"is active"]
    where
        mdlc_key = HM.lookup dlc . HM.fromList $
            [("Conquest of Paradise", "cop")
            ,("Wealth of Nations", "won")
            ,("Res Publica", "rp")
            ,("Art of War", "aow")
            ,("El Dorado", "ed")
            ,("Common Sense", "cs")
            ,("The Cossacks", "cos")
            ]
        dlc_icon = if isNothing mdlc_key then [] else [icon (fromJust mdlc_key)]

data EstateInfluenceModifier = EstateInfluenceModifier {
        eim_estate :: Maybe Text
    ,   eim_modifier :: Maybe Text
    }
newEIM = EstateInfluenceModifier Nothing Nothing
has_estate_influence_modifier :: GenericStatement -> PP Doc
has_estate_influence_modifier stmt@(Statement _ (CompoundRhs scr))
    = pp_eim $ foldl' addField newEIM scr
    where
        pp_eim inf = case (eim_estate inf, eim_modifier inf) of
            (Just est, Just mod) -> do
                loc_est <- getGameL10n est
                loc_mod <- getGameL10n mod
                return $ hsep
                    [icon est
                    ,strictText loc_est
                    ,"estate has influence modifier"
                    ,dquotes (strictText loc_mod)
                    ]
            _ -> return (pre_statement stmt)
        addField :: EstateInfluenceModifier -> GenericStatement -> EstateInfluenceModifier
        addField inf (Statement (GenericLhs "estate") (GenericRhs est)) = inf { eim_estate = Just est }
        addField inf (Statement (GenericLhs "modifier") (GenericRhs mod)) = inf { eim_modifier = Just mod }
        addField inf _ = inf -- unknown statement
has_estate_influence_modifier stmt = return $ pre_statement stmt

trigger_switch :: GenericStatement -> PP Doc
-- A trigger switch must be of the form
-- trigger_switch = {
--  on_trigger = <statement lhs>
--  <statement rhs> = {
--      <actions>
--  }
-- }
-- where the <statement rhs> block may be repeated several times.
trigger_switch stmt@(Statement _ (CompoundRhs
                        (Statement (GenericLhs "on_trigger") (GenericRhs condlhs)
                        :clauses))) = do
            clauses_pp'd <- forM clauses $ \clause -> case clause of
                Statement (GenericLhs condrhs) (CompoundRhs action) -> do
                    let guard = Statement (GenericLhs condlhs) (GenericRhs condrhs)
                    guard_pp'd <- pp_statement' guard
                    statement_pp'd <- indentUp (pp_script action)
                    return $ hcat
                        ["If", space
                        ,guard_pp'd
                        ,":"
                        ,line
                        ,statement_pp'd
                        ]
                _ -> trace ("Unrecognized statement: " ++ show clause) (return mempty)
            line_prefix <- withCurrentIndent $ \i -> return $ strictText (T.replicate i "*") <> space
            return . hcat . PP.punctuate (line <> line_prefix) $ clauses_pp'd
trigger_switch stmt = return $ pre_statement stmt

data AddEstateInfluenceModifier = AddEstateInfluenceModifier {
        aeim_estate :: Maybe Text
    ,   aeim_desc :: Maybe Text
    ,   aeim_influence :: Maybe Double
    ,   aeim_duration :: Maybe Double
    } deriving Show
newAddEstateInfluenceModifier = AddEstateInfluenceModifier Nothing Nothing Nothing Nothing

estate_influence_modifier :: GenericStatement -> PP Doc
estate_influence_modifier stmt@(Statement _ (CompoundRhs scr))
    = pp_eim $ foldl' addLine newAddEstateInfluenceModifier scr
    where
        addLine :: AddEstateInfluenceModifier -> GenericStatement -> AddEstateInfluenceModifier 
        addLine aeim (Statement (GenericLhs "estate") (GenericRhs estate)) = aeim { aeim_estate = Just estate }
        addLine aeim (Statement (GenericLhs "desc") (GenericRhs desc)) = aeim { aeim_desc = Just desc }
        addLine aeim (Statement (GenericLhs "influence") (FloatRhs influence)) = aeim { aeim_influence = Just influence }
        addLine aeim (Statement (GenericLhs "duration") (FloatRhs duration)) = aeim { aeim_duration = Just duration }
        addLine aeim _ = aeim
        pp_eim :: AddEstateInfluenceModifier -> PP Doc
        pp_eim aeim
            = case (aeim_estate aeim, aeim_desc aeim, aeim_influence aeim, aeim_duration aeim) of
                (Just estate, Just desc, Just influence, Just duration) -> do
                    estate_loc <- getGameL10n estate
                    desc_loc <- getGameL10n desc
                    return . hsep $
                        [strictText estate_loc
                        ,"estate gains influence modifier"
                        ,dquotes (strictText desc_loc)
                        ,parens $ hsep [enclose "'''" "'''" (pp_signed pp_float influence), "influence"]
                        ]
                        ++ if duration < 0 then ["indefinitely"] else
                            ["for"
                            ,pp_float duration
                            ,"days"
                            ]
                _ -> return $ pre_statement stmt
estate_influence_modifier stmt = return $ pre_statement stmt

data EstateInfluence = EstateInfluence {
        ei_estate :: Maybe Text
    ,   ei_influence :: Maybe Double
    } deriving Show
newEstateInfluence = EstateInfluence Nothing Nothing

estate_influence :: GenericStatement -> PP Doc
estate_influence stmt@(Statement _ (CompoundRhs scr))
    = pp_eim $ foldl' addLine newEstateInfluence scr
    where
        addLine :: EstateInfluence -> GenericStatement -> EstateInfluence 
        addLine ei (Statement (GenericLhs "estate") (GenericRhs estate)) = ei { ei_estate = Just estate }
        addLine ei (Statement (GenericLhs "influence") (FloatRhs influence)) = ei { ei_influence = Just influence }
        addLine ei _ = ei
        pp_eim :: EstateInfluence -> PP Doc
        pp_eim ei
            = case (ei_estate ei, ei_influence ei) of
                (Just estate, Just influence) -> do
                    estate_loc <- getGameL10n estate
                    return . hsep $
                        [strictText estate_loc
                        ,"estate has at least"
                        ,enclose "'''" "'''" (pp_float influence)
                        ,"influence"]
                _ -> return $ pre_statement stmt
estate_influence stmt = return $ pre_statement stmt

data EstateLoyalty = EstateLoyalty {
        el_estate :: Maybe Text
    ,   el_loyalty :: Maybe Double
    } deriving Show
newEstateLoyalty = EstateLoyalty Nothing Nothing

-- True: adding; False: querying
estate_loyalty :: Bool -> GenericStatement -> PP Doc
estate_loyalty alter stmt@(Statement _ (CompoundRhs scr))
    = pp_elm $ foldl' addLine newEstateLoyalty scr
    where
        addLine :: EstateLoyalty -> GenericStatement -> EstateLoyalty 
        addLine el (Statement (GenericLhs "estate") (GenericRhs estate)) = el { el_estate = Just estate }
        addLine el (Statement (GenericLhs "loyalty") (FloatRhs loyalty)) = el { el_loyalty = Just loyalty }
        addLine el _ = el
        pp_elm :: EstateLoyalty -> PP Doc
        pp_elm el
            = case (el_estate el, el_loyalty el) of
                (Just estate, Just loyalty) -> do
                    estate_loc <- getGameL10n estate
                    return . hsep $
                        [strictText estate_loc
                        ,"estate"
                        ,if alter then
                            if loyalty < 0 then "loses" else "gains"
                         else "has at least"
                        ,(if alter then pp_hl_num True else (enclose "'''" "'''" .)) pp_float loyalty
                        ,"loyalty"]
                _ -> return $ pre_statement stmt
estate_loyalty _ stmt = return $ pre_statement stmt

data Heir = Heir
        {   heir_dynasty :: Maybe Text
        ,   heir_claim :: Maybe Double
        ,   heir_age :: Maybe Double
        }
newHeir = Heir Nothing Nothing Nothing
define_heir :: GenericStatement -> PP Doc
define_heir stmt@(Statement _ (CompoundRhs scr))
    = pp_heir $ foldl' addLine newHeir scr
    where
        addLine :: Heir -> GenericStatement -> Heir 
        addLine heir (Statement (GenericLhs "dynasty") (GenericRhs dynasty)) = heir { heir_dynasty = Just dynasty }
        addLine heir (Statement (GenericLhs "claim") (FloatRhs claim)) = heir { heir_claim = Just claim }
        addLine heir (Statement (GenericLhs "age") (FloatRhs age)) = heir { heir_age = Just age }
        addLine heir _ = heir
        pp_heir :: Heir -> PP Doc
        pp_heir heir = do
            dynasty_flag <- maybeM flag (heir_dynasty heir)
            return . hsep $
                ["Gain a new"]
                ++ (if isJust (heir_age heir)
                    then [pp_float (fromJust (heir_age heir))
                         ,"year old"]
                    else [])
                ++ ["heir"]
                ++ (if isJust dynasty_flag
                    then [parens (hsep ["with the same dynasty as", fromJust dynasty_flag])]
                    else [])
                ++ if isJust (heir_claim heir)
                    then ["with claim strength"
                         ,pp_float (fromJust (heir_claim heir))]
                    else []