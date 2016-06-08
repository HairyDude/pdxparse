{-# LANGUAGE OverloadedStrings, ViewPatterns, ScopedTypeVariables, QuasiQuotes #-}
module Stellaris.Common (
        pp_script
    ,   pp_mtth
    ,   ppOne
    ,   ppMany
    ,   iconKey
--  ,   AIWillDo (..), AIModifier (..)
    ,   StellarisScope (..), Stellaris (..)
    ,   scope, getCurrentStellarisScope
--  ,   ppAiWillDo, ppAiMod
    ,   module Stellaris.Types
    ) where

import Prelude hiding (sequence, mapM)

import Debug.Trace

import Control.Applicative (liftA2)
import Control.Arrow
import Control.Monad.Reader hiding (sequence, mapM, forM)
import Control.Monad.State hiding (sequence, mapM, forM)

import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Data.Traversable

import Data.ByteString (ByteString)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE

-- TODO: get rid of these, do icon key lookups from another module
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Trie (Trie)
import qualified Data.Trie as Tr

import qualified Data.Set as S

import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>), int, double)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract
import Localization
import Messages
import MessageTools (plural)
import QQ
import Stellaris.Types

scope :: Monad m => StellarisScope -> PPT m a -> PPT m a
scope s = local $ \st ->
    let oldststate = gStellaris st
    in  st { gStellaris = oldststate { scopeStack = s : scopeStack oldststate } }

-- Get current scope, if there is one.
getCurrentStellarisScope :: Monad m => PPT m (Maybe StellarisScope)
getCurrentStellarisScope = asks (listToMaybe . scopeStack . gStellaris)

-- no particular order from here... TODO: organize this!

msgToPP :: Monad m => ScriptMessage -> PPT m IndentedMessages
msgToPP msg = (:[]) <$> alsoIndent' msg

isPronoun :: Text -> Bool
isPronoun s = T.map toLower s `S.member` pronouns where
    pronouns = S.fromList
        ["root"
        ,"prev"
        ,"owner"
        ,"controller"
        ]

pp_script :: Monad m => GenericScript -> PPT m Doc
pp_script [] = return "(Nothing)"
pp_script script = imsg2doc_html =<< ppMany script

-- Emit icon template.
icon :: Text -> Doc
icon what = template "icon" [HM.lookupDefault what what scriptIconTable, "28px"]
iconText :: Text -> Text
iconText = doc2text . icon

plainMsg :: Monad m => Text -> PPT m IndentedMessages
plainMsg msg = (:[]) <$> (alsoIndent' . MsgUnprocessed $ msg)

-- Surround a doc in a <pre> element.
pre_statement :: GenericStatement -> Doc
pre_statement stmt = "<pre>" <> genericStatement2doc stmt <> "</pre>"

-- Don't use doc2text, because it uses renderCompact which is not what we want
-- here.
preMessage :: GenericStatement -> ScriptMessage
preMessage = MsgUnprocessed
            . TL.toStrict
            . PP.displayT
            . PP.renderPretty 0.8 80
            . pre_statement

preStatement :: Monad m => GenericStatement -> PPT m IndentedMessages
preStatement stmt = (:[]) <$> alsoIndent' (preMessage stmt)

-- Text version
pre_statement' :: GenericStatement -> Text
pre_statement' = doc2text . pre_statement

ppMany :: Monad m => GenericScript -> PPT m IndentedMessages
ppMany scr = indentUp (concat <$> mapM ppOne scr)

-- Table of handlers for statements.
-- Dispatch on strings is /much/ quicker using a lookup table than a
-- huge case statement, which uses (==) on each one in turn.
ppHandlers :: Monad m => Trie (GenericStatement -> PPT m IndentedMessages)
ppHandlers = Tr.fromList
        [
        -- Statements where RHS is irrelevant (usually "yes")
--       ("add_cardinal"           , const (msgToPP MsgAddCardinal))
        -- Numbers
--      ,("add_authority"                    , numeric MsgGainAuth) -- Inti
        -- ... with icons
--      ,("add_adm_power"            , numericIcon "adm" MsgGainADM)
--      ,("army_tradition"           , numericIconBonus "army tradition" MsgArmyTradition MsgYearlyArmyTradition)
        -- Modifiers
         ("add_modifier", addModifier)
        -- Simple compound statements
        -- Note that "any" can mean "all" or "one or more" depending on context.
        ,("and" , compoundMessage MsgAllOf)
        ,("root", compoundMessage MsgOurCountry)
        -- These two are ugly, but without further analysis we can't know
        -- what it means.
        ,("from", compoundMessage MsgFROM)
        ,("prev", compoundMessage MsgPREV)
        ,("not" , compoundMessage MsgNoneOf)
        ,("or"  , compoundMessage MsgAtLeastOneOf)
        -- There is a semantic distinction between "all" and "every",
        -- namely that the former means "this is true for all <type>" while
        -- the latter means "do this for every <type>."
        ,("any_country"         , scope StellarisCountry . compoundMessage MsgAnyCountry)
        ,("any_owned_planet"    , scope StellarisPlanet  . compoundMessage MsgAnyOwnedPlanet)
        ,("any_owned_ship"      , scope StellarisShip    . compoundMessage MsgAnyOwnedShip)
        ,("any_pop"             , scope StellarisPop     . compoundMessage MsgAnyPop)
        ,("every_country"       , scope StellarisCountry . compoundMessage MsgEveryCountry)
        ,("every_owned_planet"  , scope StellarisPlanet  . compoundMessage MsgEveryOwnedPlanet)
        ,("every_owned_pop"     , scope StellarisPop     . compoundMessage MsgEveryOwnedPop)
        ,("random_country"      , scope StellarisCountry . compoundMessage MsgRandomCountry)
        ,("random_owned_ship"   , scope StellarisShip    . compoundMessage MsgRandomOwnedShip)
        ,("random_pop"          , scope StellarisPop     . compoundMessage MsgRandomPop)
        ,("random_system"       , scope StellarisSystem  . compoundMessage MsgRandomSystem)
        ,("random_tile"         , scope StellarisTile    . compoundMessage MsgRandomTile)
        ,("if"                  ,                          compoundMessage MsgIf) -- always needs editing
        ,("hidden_effect"       ,                          compoundMessage MsgHiddenEffect)
        ,("limit"               ,                          compoundMessage MsgLimit) -- always needs editing
        ,("capital_scope"       , scope StellarisPlanet  . compoundMessage MsgCapital)
        ,("owner"               , scope StellarisCountry . compoundMessage MsgOwner)
        -- Random
        ,("random"      , random)
        ,("random_list" , randomList) -- Works differently than EU4
        -- Simple generic statements (RHS is a localizable atom)
        ,("text"            , withLocAtom MsgTextIs)
        -- RHS is a province ID
--      ,("capital"           , withProvince MsgCapitalIs)
        -- RHS is a flag OR a province ID
--      ,("add_permanent_claim", withFlagOrProvince MsgGainPermanentClaimCountry MsgGainPermanentClaimProvince)
        -- RHS is a flag or province id, but the statement's meaning depends on the scope
--      ,("has_discovered"     , withFlagOrProvinceStellarisScope MsgHasDiscovered MsgDiscoveredBy) -- scope sensitive
        -- Simple generic statements (typewriter face)
        ,("has_country_flag"    , withNonlocAtom2 MsgCountryFlag MsgHasFlag)
        ,("has_global_flag"     , withNonlocAtom2 MsgGlobalFlag  MsgHasFlag)
        ,("remove_country_flag" , withNonlocAtom2 MsgCountryFlag MsgClearFlag)
        ,("set_country_flag"    , withNonlocAtom2 MsgCountryFlag MsgSetFlag)
        -- Simple generic statements with icon
        ,("add_trait"       , withLocAtomIcon MsgGainTrait)
        ,("has_government"  , withLocAtomIcon MsgGovernmentIsIcon)
--      ,("advisor"                 , withLocAtomIcon MsgHasAdvisorType)
        -- Simple generic statements with flag
--      ,("alliance_with"           , withFlag MsgAlliedWith)
        -- Simple generic statements with flag or "yes"/"no"
--      ,("exists", withFlagOrBool MsgExists MsgCountryExists)
        -- Statements that may be an icon, a flag, or a pronoun (such as ROOT)
        -- Boolean argument is whether to emit an icon.
--      ,("religion"       , iconOrFlag MsgReligion MsgSameReligion)
        -- Statements that may be either a tag or a province
--      ,("is_core" , tagOrProvince MsgIsCoreOf MsgHasCoreOn)
        -- Boolean statements
        ,("is_ai"                       , withBool MsgIsAIControlled)
        ,("is_at_war"                   , withBool MsgAtWar)
        ,("is_capital"                  , withBool MsgIsCapital)
        ,("always"                      , withBool MsgAlways)
        -- Statements that may be numeric or a tag
--      ,("num_of_cities", numericOrTag MsgNumCities MsgNumCitiesThan)
        -- Signed numeric statements
--      ,("tolerance_to_this", numeric MsgToleranceToThis)
        -- Special cases
--      ,("legitimacy_or_horde_unity", numeric MsgLegitimacyOrHordeUnity)
        -- Number of provinces of some kind, mostly religions and trade goods
--      ,("wool"          , numProvinces "wool" MsgGoodsProvinces)
        -- Special
--      ,("add_core"            , addCore)
        -- Special complex statements
--      ,("add_casus_belli"              , addCB True)
        ,("add_opinion_modifier"         , opinion MsgAddOpinion MsgAddOpinionDur)
        ,("custom_trigger_tooltip"       , customTriggerTooltip)
        ,("check_variable"               , textValue "which" "value" MsgCheckVariable MsgCheckVariable tryLoc)
        ,("country_event"                , scope StellarisCountry . triggerEvent MsgCountryEvent)
--      ,("has_opinion"                  , hasOpinion)
--      ,("has_opinion_modifier"         , opinion MsgHasOpinionMod (\what who _years -> MsgHasOpinionMod what who))
--      ,("province_event"               , scope StellarisProvince . triggerEvent MsgProvinceEvent)
--      ,("remove_opinion"               , opinion MsgRemoveOpinionMod (\what who _years -> MsgRemoveOpinionMod what who))
        ,("trigger_switch"               , triggerSwitch)
        ,("switch"                       , triggerSwitch)
        -- Ignored
        ,("custom_tooltip", const (plainMsg "(custom tooltip - delete this line)"))
        ,("tooltip"       , const (plainMsg "(explanatory tooltip - delete this line)"))
        ]

ppOne :: Monad m => GenericStatement -> PPT m IndentedMessages
ppOne stmt@[pdx| %lhs = %rhs |] = case lhs of
    GenericLhs label -> case Tr.lookup (TE.encodeUtf8 (T.toLower label)) ppHandlers of
        Just handler -> handler stmt
        -- default
        Nothing -> do
            mloc <- getGameL10nIfPresent label
            case mloc of
                -- Check for localizable atoms, e.g. regions
                Just loc -> compound loc stmt
                Nothing -> preStatement stmt
    IntLhs n -> do -- Treat as a province tag
        let provN = T.pack (show n)
        prov_loc <- getGameL10nDefault ("Province " <> provN) ("PROV" <> provN)
        case rhs of
            CompoundRhs scr -> do
                header <- msgToPP (MsgProvince prov_loc)
                scriptMsgs <- ppMany scr
                return (header ++ scriptMsgs)
            _ -> preStatement stmt
    CustomLhs _ -> preStatement stmt
ppOne stmt = preStatement stmt

-----------------------------------------------------------------
-- Script handlers that should be used directly, not via ppOne --
-----------------------------------------------------------------

data MTTH = MTTH
        {   mtth_years :: Maybe Int
        ,   mtth_months :: Maybe Int
        ,   mtth_days :: Maybe Int
        ,   mtth_modifiers :: [MTTHModifier] -- TODO
        } deriving Show
data MTTHModifier = MTTHModifier
        {   mtthmod_factor :: Maybe Double
        ,   mtthmod_conditions :: GenericScript
        } deriving Show
newMTTH :: MTTH
newMTTH = MTTH Nothing Nothing Nothing []
newMTTHMod :: MTTHModifier
newMTTHMod = MTTHModifier Nothing []
pp_mtth :: Monad m => GenericScript -> PPT m Doc
pp_mtth = pp_mtth' . foldl' addField newMTTH
    where
        addField mtth [pdx| years    = !n   |] = mtth { mtth_years = Just n }
        addField mtth [pdx| months   = !n   |] = mtth { mtth_months = Just n }
        addField mtth [pdx| days     = !n   |] = mtth { mtth_days = Just n }
        addField mtth [pdx| modifier = @rhs |] = addMTTHMod mtth rhs
        addField mtth _ = mtth -- unrecognized
        addMTTHMod mtth scr = mtth {
                mtth_modifiers = mtth_modifiers mtth
                                 ++ [foldl' addMTTHModField newMTTHMod scr] } where
            addMTTHModField mtthmod [pdx| factor = !n |]
                = mtthmod { mtthmod_factor = Just n }
            addMTTHModField mtthmod stmt -- anything else is a condition
                = mtthmod { mtthmod_conditions = mtthmod_conditions mtthmod ++ [stmt] }
        pp_mtth' (MTTH myears mmonths mdays modifiers) = do
            modifiers_pp'd <- intersperse line <$> mapM pp_mtthmod modifiers
            let hasYears = isJust myears
                hasMonths = isJust mmonths
                hasDays = isJust mdays
                hasModifiers = not (null modifiers)
            return . mconcat $
                case myears of
                    Just years ->
                        [PP.int years, space, strictText $ plural years "year" "years"]
                        ++
                        if hasMonths && hasDays then [",", space]
                        else if hasMonths || hasDays then ["and", space]
                        else []
                    Nothing -> []
                ++
                case mmonths of
                    Just months ->
                        [PP.int months, space, strictText $ plural months "month" "months"]
                    _ -> []
                ++
                case mdays of
                    Just days ->
                        (if hasYears && hasMonths then ["and", space]
                         else []) -- if years but no months, already added "and"
                        ++
                        [PP.int days, space, strictText $ plural days "day" "days"]
                    _ -> []
                ++
                (if hasModifiers then
                    [line, "<br/>'''Modifiers'''", line]
                    ++ modifiers_pp'd
                 else [])
        pp_mtthmod (MTTHModifier (Just factor) conditions) =
            case conditions of
                [_] -> do
                    conditions_pp'd <- pp_script conditions
                    return . mconcat $
                        [conditions_pp'd
                        ,enclose ": '''×" "'''" (pp_float factor)
                        ]
                _ -> do
                    conditions_pp'd <- indentUp (pp_script conditions)
                    return . mconcat $
                        ["*"
                        ,enclose "'''×" "''':" (pp_float factor)
                        ,line
                        ,conditions_pp'd
                        ]
        pp_mtthmod (MTTHModifier Nothing _)
            = return "(invalid modifier! Bug in extractor?)"

--------------------------------
-- General statement handlers --
--------------------------------

compound :: Monad m =>
    Text -> GenericStatement -> PPT m IndentedMessages
compound header [pdx| %_ = @scr |]
    = withCurrentIndent $ \_ -> do -- force indent level at least 1
        headerMsg <- plainMsg (header <> ":")
        scriptMsgs <- ppMany scr
        return $ headerMsg ++ scriptMsgs
compound _ stmt = preStatement stmt

compoundMessage :: Monad m =>
    ScriptMessage -> GenericStatement -> PPT m IndentedMessages
compoundMessage header [pdx| %_ = @scr |]
    = withCurrentIndent $ \i -> do
        script_pp'd <- ppMany scr
        return ((i, header) : script_pp'd)
compoundMessage _ stmt = preStatement stmt

-- RHS is a localizable atom.
withLocAtom :: Monad m =>
    (Text -> ScriptMessage)
        -> GenericStatement
        -> PPT m IndentedMessages
withLocAtom msg [pdx| %_ = ?key |]
    = msgToPP =<< msg <$> getGameL10n key
withLocAtom _ stmt = preStatement stmt

-- RHS is a localizable atom and we need a second one (passed to message as
-- first arg).
withLocAtom2 :: Monad m =>
    ScriptMessage
        -> (Text -> Text -> Text -> ScriptMessage)
        -> GenericStatement
        -> PPT m IndentedMessages
withLocAtom2 inMsg msg [pdx| %_ = ?key |]
    = msgToPP =<< msg <$> pure key <*> messageText inMsg <*> getGameL10n key
withLocAtom2 _ _ stmt = preStatement stmt

withLocAtomAndIcon :: Monad m =>
    Text
        -> (Text -> Text -> ScriptMessage)
        -> GenericStatement
        -> PPT m IndentedMessages
withLocAtomAndIcon iconkey msg [pdx| %_ = $key |]
    = do what <- getGameL10n key
         msgToPP $ msg (iconText iconkey) what
withLocAtomAndIcon _ _ stmt = preStatement stmt

withLocAtomIcon :: Monad m =>
    (Text -> Text -> ScriptMessage)
        -> GenericStatement
        -> PPT m IndentedMessages
withLocAtomIcon msg stmt@[pdx| %_ = $key |]
    = withLocAtomAndIcon (fromMaybe key (iconKey key)) msg stmt
withLocAtomIcon _ stmt = preStatement stmt

{-
withLocAtomIconStellarisScope :: Monad m =>
    (Text -> Text -> ScriptMessage)
        -> (Text -> Text -> ScriptMessage)
        -> GenericStatement -> PPT m IndentedMessages
withLocAtomIconStellarisScope countrymsg provincemsg stmt = do
    thescope <- getCurrentStellarisScope
    case thescope of
        Just StellarisCountry -> withLocAtomIcon countrymsg stmt
        Just StellarisProvince -> withLocAtomIcon provincemsg stmt
        _ -> preStatement stmt -- others don't make sense
-}

-- As withLocAtom but no l10n.
-- Currently unused
--withNonlocAtom :: (Text -> ScriptMessage) -> GenericStatement -> PP extra IndentedMessages
--withNonlocAtom msg [pdx| %_ = ?text |] = msgToPP $ msg text
--withNonlocAtom _ stmt = preStatement stmt

-- As withNonlocAtom but with an additional bit of text.
withNonlocAtom2 :: Monad m =>
    ScriptMessage
        -> (Text -> Text -> ScriptMessage)
        -> GenericStatement
        -> PPT m IndentedMessages
withNonlocAtom2 submsg msg [pdx| %_ = ?txt |] = do
    extratext <- messageText submsg
    msgToPP $ msg extratext txt
withNonlocAtom2 _ _ stmt = preStatement stmt

-- Table of script atom -> icon key. Only ones that are different are listed.
scriptIconTable :: HashMap Text Text
scriptIconTable = HM.fromList
    [("ai_overlordship", "ai overlordship")
    ,("democratic_utopia", "democratic utopia")
    ,("despotic_empire", "despotic empire")
    ,("despotic_hegemony", "despotic hegemony")
    ,("direct_democracy", "direct democracy")
    ,("divine_mandate", "divine mandate")
    ,("enlightened_monarchy", "enlightened monarchy")
    ,("fragmented_nations", "fragmented nations")
    ,("illuminated_technocracy", "illuminated technocracy")
    ,("indirect_democracy", "indirect democracy")
    ,("irenic_democracy", "irenic democracy")
    ,("irenic_monarchy", "irenic monarchy")
    ,("irenic_protectorate", "irenic protectorate")
    ,("machine_consciousness", "machine consciousness")
    ,("martial_demarchy", "martial demarchy")
    ,("martial_empire", "martial empire")
    ,("mega_corporation", "mega corporation")
    ,("military_dictatorship", "military dictatorship")
    ,("military_junta", "military junta")
    ,("military_order", "military order")
    ,("military_republic", "military republic")
    ,("moral_democracy", "moral democracy")
    ,("neural_network_administration", "neural network administration")
    ,("ordered_stratocracy", "ordered stratocracy")
    ,("peaceful_bureaucracy", "peaceful bureaucracy")
    ,("pirate_codex", "pirate codex")
    ,("plutocratic_oligarchy", "plutocratic oligarchy")
    ,("primitive_feudalism", "primitive feudalism")
    ,("science_directorate", "science directorate")
    ,("stagnant_ascendancy", "stagnant ascendancy")
    ,("star_empire", "star empire")
    ,("subconscious_consensus", "subconscious consensus")
    ,("theocratic_oligarchy", "theocratic oligarchy")
    ,("theocratic_republic", "theocratic republic")
    ,("transcendent_empire", "transcendent empire")
    ,("transcendent_oligarchy", "transcendent oligarchy")
    ,("transcendent_republic", "transcendent republic")
    ,("leader_trait_adaptable", "adaptable")
    ,("leader_trait_aggressive", "aggressive")
    ,("leader_trait_archaeologist", "archaeologist")
    ,("leader_trait_architectural_interest", "architectural interest")
    ,("leader_trait_armchair_commander", "armchair commander")
    ,("leader_trait_army_logistician", "army logistician")
    ,("leader_trait_arrested_development", "arrested development")
    ,("leader_trait_attacker", "attacker")
    ,("leader_trait_butcher", "butcher")
    ,("leader_trait_carefree", "carefree")
    ,("leader_trait_careful", "careful")
    ,("leader_trait_cautious", "cautious")
    ,("leader_trait_charismatic", "charismatic")
    ,("leader_trait_custom_AI_assistant", "custom AI assistant")
    ,("leader_trait_defender", "defender")
    ,("leader_trait_engineer", "engineer")
    ,("leader_trait_environmental_engineer", "environmental engineer")
    ,("leader_trait_expertise_biology", "biology")
    ,("leader_trait_expertise_computing", "computing")
    ,("leader_trait_expertise_field_manipulation", "field manipulation")
    ,("leader_trait_expertise_industry", "industry")
    ,("leader_trait_expertise_materials", "materials")
    ,("leader_trait_expertise_military_theory", "military theory")
    ,("leader_trait_expertise_new_worlds", "new worlds")
    ,("leader_trait_expertise_particles", "particles")
    ,("leader_trait_expertise_psionics", "psionics")
    ,("leader_trait_expertise_rocketry", "rocketry")
    ,("leader_trait_expertise_statecraft", "statecraft")
    ,("leader_trait_expertise_voidcraft", "voidcraft")
    ,("leader_trait_fleet_logistician", "fleet logistician")
    ,("leader_trait_fleet_organizer", "fleet organizer")
    ,("leader_trait_gale_speed", "gale speed")
    ,("leader_trait_glory_seeker", "glory seeker")
    ,("leader_trait_intellectual", "intellectual")
    ,("leader_trait_iron_fist", "iron fist")
    ,("leader_trait_lethargic", "lethargic")
    ,("leader_trait_maniacal", "maniacal")
    ,("leader_trait_meticulous", "meticulous")
    ,("leader_trait_nervous", "nervous")
    ,("leader_trait_paranoid", "paranoid")
    ,("leader_trait_resilient", "resilient")
    ,("leader_trait_roamer", "roamer")
    ,("leader_trait_scout", "scout")
    ,("leader_trait_spark_of_genius", "spark of genius")
    ,("leader_trait_stubborn", "stubborn")
    ,("leader_trait_substance_abuser", "substance abuser")
    ,("leader_trait_trickster", "trickster")
    ,("leader_trait_unyielding", "unyielding")
    ,("trait_ruler_architectural_sense", "architectural sense")
    ,("trait_ruler_battleship_focus", "battleship focus")
    ,("trait_ruler_champion_of_the_people", "champion of the people")
    ,("trait_ruler_corvette_focus", "corvette focus")
    ,("trait_ruler_cruiser_focus", "cruiser focus")
    ,("trait_ruler_deep_connections", "deep connections")
    ,("trait_ruler_destroyer_focus", "destroyer focus")
    ,("trait_ruler_expansionist", "expansionist")
    ,("trait_ruler_explorer", "explorer")
    ,("trait_ruler_eye_for_talent", "eye for talent")
    ,("trait_ruler_fertility_preacher", "fertility preacher")
    ,("trait_ruler_fortifier", "fortifier")
    ,("trait_ruler_from_the_ranks", "from the ranks")
    ,("trait_ruler_frontier_spirit", "frontier spirit")
    ,("trait_ruler_home_in_the_sky", "home in the sky")
    ,("trait_ruler_industrialist", "industrialist")
    ,("trait_ruler_investor", "investor")
    ,("trait_ruler_logistic_understanding", "logistic understanding")
    ,("trait_ruler_military_pioneer", "military pioneer")
    ,("trait_ruler_recruiter", "recruiter")
    ,("trait_ruler_warlike", "warlike")
    ,("trait_ruler_world_shaper", "world shaper")
    ]

-- Given a script atom, return the corresponding icon key, if any.
iconKey :: Text -> Maybe Text
iconKey atom = HM.lookup atom scriptIconTable

-- Numeric statement.
-- TODO (if necessary): allow operators other than = and pass them to message
-- handler
numeric :: Monad m =>
    (Double -> ScriptMessage)
        -> GenericStatement
        -> PPT m IndentedMessages
numeric msg [pdx| %_ = !n |] = msgToPP $ msg n
numeric _ stmt = plainMsg $ pre_statement' stmt

withBool :: Monad m =>
    (Bool -> ScriptMessage)
        -> GenericStatement
        -> PPT m IndentedMessages
withBool msg stmt = do
    fullmsg <- withBool' msg stmt
    maybe (preStatement stmt)
          return
          fullmsg

withBool' :: Monad m =>
    (Bool -> ScriptMessage)
        -> GenericStatement
        -> PPT m (Maybe IndentedMessages)
withBool' msg [pdx| %_ = ?yn |] | T.map toLower yn `elem` ["yes","no","false"]
    = fmap Just . msgToPP $ case T.toCaseFold yn of
        "yes" -> msg True
        "no"  -> msg False
        "false" -> msg False
        _     -> error "impossible: withBool matched a string that wasn't yes, no or false"
withBool' _ _ = return Nothing

numericIcon :: Monad m =>
    Text
        -> (Text -> Double -> ScriptMessage)
        -> GenericStatement
        -> PPT m IndentedMessages
numericIcon the_icon msg [pdx| %_ = !amt |]
    = msgToPP $ msg (iconText the_icon) amt
numericIcon _ _ stmt = plainMsg $ pre_statement' stmt

numericIconBonus :: Monad m =>
    Text
        -> (Text -> Double -> ScriptMessage)
        -> (Text -> Double -> ScriptMessage)
        -> GenericStatement
        -> PPT m IndentedMessages
numericIconBonus the_icon plainmsg yearlymsg [pdx| %_ = !amt |]
    = do
        mscope <- getCurrentStellarisScope
        let icont = iconText the_icon
            yearly = msgToPP $ yearlymsg icont amt
        case mscope of
            Nothing -> yearly -- ideas / bonuses
            Just thescope -> case thescope of
                _ -> -- act as though it's country for all others
                    msgToPP $ plainmsg icont amt
numericIconBonus _ _ _ stmt = plainMsg $ pre_statement' stmt

----------------------
-- Text/value pairs --
----------------------

-- This is for statements of the form
--      head = {
--          what = some_atom
--          value = 3
--      }
-- e.g.
--      num_of_religion = {
--          religion = catholic
--          value = 0.5
--      }
-- There are several statements of this form, but with different "what" labels,
-- so the first parameter says what that label is. There's also one for value,
-- in case there are other labels for that.
--
-- There are two message parameters, one for value < 1 and one for value >= 1.
-- In the example num_of_religion, value is interpreted as a percentage of
-- provinces if less than 1, or a number of provinces otherwise. These require
-- rather different messages.
--
-- We additionally attempt to localize the RHS of "what". If it has no
-- localization string, it gets wrapped in a tt element instead.

-- convenience synonym
tryLoc :: Monad m => Text -> PPT m (Maybe Text)
tryLoc = getGameL10nIfPresent

data TextValue = TextValue
        {   tv_what :: Maybe Text
        ,   tv_value :: Maybe Double
        }
newTV :: TextValue
newTV = TextValue Nothing Nothing
textValue :: forall m. Monad m =>
    Text                                             -- ^ Label for "what"
        -> Text                                      -- ^ Label for "how much"
        -> (Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, if abs value < 1
        -> (Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, if abs value >= 1
        -> (Text -> PPT m (Maybe Text)) -- ^ Action to localize, get icon, etc. (applied to RHS of "what")
        -> GenericStatement -> PPT m IndentedMessages
textValue whatlabel vallabel smallmsg bigmsg loc stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_tv (foldl' addLine newTV scr)
    where
        addLine :: TextValue -> GenericStatement -> TextValue
        addLine tv [pdx| $label = ?what |] | label == whatlabel
            = tv { tv_what = Just what }
        addLine tv [pdx| $label = !val |] | label == vallabel
            = tv { tv_value = Just val }
        addLine nor _ = nor
        pp_tv :: TextValue -> PPT m ScriptMessage
        pp_tv tv = case (tv_what tv, tv_value tv) of
            (Just what, Just value) -> do
                mwhat_loc <- loc what
                let what_icon = iconText what
                    what_loc = fromMaybe ("<tt>" <> what <> "</tt>") mwhat_loc
                return $ (if abs value < 1 then smallmsg else bigmsg) what_icon what_loc value
            _ -> return $ preMessage stmt
textValue _ _ _ _ _ stmt = preStatement stmt

-- | Statements of the form
--      has_trade_modifier = {
--          who = ROOT
--          name = merchant_recalled
--      }
data TextAtom = TextAtom
        {   ta_what :: Maybe Text
        ,   ta_atom :: Maybe Text
        }
newTA :: TextAtom
newTA = TextAtom Nothing Nothing
textAtom :: forall m. Monad m =>
    Text -- ^ Label for "what"
        -> Text -- ^ Label for atom
        -> (Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor
        -> (Text -> PPT m (Maybe Text)) -- ^ Action to localize, get icon, etc. (applied to RHS of "what")
        -> GenericStatement -> PPT m IndentedMessages
textAtom whatlabel atomlabel msg loc stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_ta (foldl' addLine newTA scr)
    where
        addLine :: TextAtom -> GenericStatement -> TextAtom
        addLine ta [pdx| $label = ?what |]
            | label == whatlabel
            = ta { ta_what = Just what }
        addLine ta [pdx| $label = ?at |]
            | label == atomlabel
            = ta { ta_atom = Just at }
        addLine nor _ = nor
        pp_ta :: TextAtom -> PPT m ScriptMessage
        pp_ta ta = case (ta_what ta, ta_atom ta) of
            (Just what, Just atom) -> do
                mwhat_loc <- loc what
                atom_loc <- getGameL10n atom
                let what_icon = iconText what
                    what_loc = fromMaybe ("<tt>" <> what <> "</tt>") mwhat_loc
                return $ msg what_icon what_loc atom_loc
            _ -> return $ preMessage stmt
textAtom _ _ _ _ stmt = preStatement stmt

{-
-- AI decision factors
-- Most of the code for this is in Stellaris.SuperCommon and re-exported here,
-- because Stellaris.IdeaGroups needs them. But only Stellaris.Common needs output
-- functions.
ppAiWillDo :: Monad m => AIWillDo -> PPT m IndentedMessages
ppAiWillDo (AIWillDo mbase mods) = do
    mods_pp'd <- fold <$> traverse ppAiMod mods
    let baseWtMsg = case mbase of
            Nothing -> MsgNoBaseWeight
            Just base -> MsgAIBaseWeight base
    iBaseWtMsg <- msgToPP baseWtMsg
    return $ iBaseWtMsg ++ mods_pp'd

ppAiMod :: Monad m => AIModifier -> PPT m IndentedMessages
ppAiMod (AIModifier (Just multiplier) triggers) = do
    triggers_pp'd <- ppMany triggers
    case triggers_pp'd of
        [(i, triggerMsg)] -> do
            triggerText <- messageText triggerMsg
            return [(i, MsgAIFactorOneline triggerText multiplier)]
        _ -> withCurrentIndentZero $ \i -> return $
            (i, MsgAIFactorHeader multiplier)
            : map (first succ) triggers_pp'd -- indent up
ppAiMod (AIModifier Nothing _) =
    plainMsg "(missing multiplier for this factor)"
-}

---------------------------------
-- Specific statement handlers --
---------------------------------

-- Modifiers

addModifier :: Monad m => GenericStatement -> PPT m IndentedMessages
addModifier stmt@(Statement _ OpEq (CompoundRhs
        [[pdx| modifier = ?mod_name |]
        ,[pdx| days     = !mod_days |]]))
    = msgToPP =<< do
        name_loc <- getGameL10n mod_name
        return $ MsgAddMod mod_name name_loc mod_days
addModifier stmt = preStatement stmt

-- Opinions

-- Add an opinion modifier towards someone (for a number of years).
data AddOpinion = AddOpinion {
        op_who :: Maybe Text
    ,   op_modifier :: Maybe Text
    ,   op_years :: Maybe Double
    } deriving Show
newAddOpinion :: AddOpinion
newAddOpinion = AddOpinion Nothing Nothing Nothing

opinion :: Monad m =>
    (Text -> Text -> ScriptMessage)
        -> (Text -> Text -> Double -> ScriptMessage)
        -> GenericStatement -> PPT m IndentedMessages
opinion msgIndef msgDur stmt@(Statement _ OpEq (CompoundRhs scr))
    = msgToPP =<< pp_add_opinion (foldl' addLine newAddOpinion scr)
    where
        addLine :: AddOpinion -> GenericStatement -> AddOpinion
        addLine op [pdx| who      = $tag   |] = op { op_who = Just tag }
        addLine op [pdx| modifier = ?label |] = op { op_modifier = Just label }
        addLine op [pdx| years    = !n     |] = op { op_years = Just n }
        addLine op _ = op
        pp_add_opinion op = case (op_who op, op_modifier op) of
            (Just whom, Just modifier) -> do
                mod_loc <- getGameL10n modifier
                case op_years op of
                    Nothing -> return $ msgIndef mod_loc whom
                    Just years -> return $ msgDur mod_loc whom years
            _ -> trace ("failed! modifier op is " ++ show (op_modifier op)) $ return (preMessage stmt)
opinion _ _ stmt = preStatement stmt

data HasOpinion = HasOpinion
        {   hop_who :: Maybe Text
        ,   hop_value :: Maybe Double
        }
newHasOpinion :: HasOpinion
newHasOpinion = HasOpinion Nothing Nothing
hasOpinion :: forall m. Monad m =>
    GenericStatement -> PPT m IndentedMessages
hasOpinion stmt@(Statement _ OpEq (CompoundRhs scr))
    = msgToPP =<< pp_hasOpinion (foldl' addLine newHasOpinion scr)
    where
        addLine :: HasOpinion -> GenericStatement -> HasOpinion
        addLine hop [pdx| who   = ?who |] = hop { hop_who = Just who }
        addLine hop [pdx| value = !val |] = hop { hop_value = Just val }
        addLine hop _ = trace "warning: unrecognized has_opinion clause" hop
        pp_hasOpinion :: HasOpinion -> PPT m ScriptMessage
        pp_hasOpinion hop = case (hop_who hop, hop_value hop) of
            (Just who, Just value) ->
                return (MsgHasOpinion value who)
            _ -> return (preMessage stmt)
hasOpinion stmt = preStatement stmt

-- Events

data TriggerEvent = TriggerEvent
        { e_id :: Maybe Text
        , e_title_loc :: Maybe Text
        , e_days :: Maybe Double
        }
newTriggerEvent :: TriggerEvent
newTriggerEvent = TriggerEvent Nothing Nothing Nothing
triggerEvent :: forall m. Monad m =>
    ScriptMessage
        -> GenericStatement
        -> PPT m IndentedMessages
triggerEvent evtType stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_trigger_event =<< foldM addLine newTriggerEvent scr
    where
        addLine :: TriggerEvent -> GenericStatement -> PPT m TriggerEvent
        addLine evt [pdx| id = $eid |] = do
            mevt_t <- gets ((stevt_title =<<)
                             . HM.lookup eid
                             . stevents . stdata . game)
            t_loc <- fmap join (sequence (getGameL10nIfPresent <$> mevt_t))
            return evt { e_id = Just eid, e_title_loc = t_loc }
        addLine evt [pdx| days = %rhs |]
            = return evt { e_days = floatRhs rhs }
        addLine evt _ = return evt
        pp_trigger_event :: TriggerEvent -> PPT m ScriptMessage
        pp_trigger_event evt = do
            evtType_t <- messageText evtType
            case e_id evt of
                Just msgid ->
                    let loc = fromMaybe msgid (e_title_loc evt)
                    in case e_days evt of
                        Just days -> return $ MsgTriggerEventDays evtType_t msgid loc days
                        Nothing -> return $ MsgTriggerEvent evtType_t msgid loc
                _ -> return $ preMessage stmt
triggerEvent _ stmt = preStatement stmt

-- Random

random :: Monad m => GenericStatement -> PPT m IndentedMessages
random stmt@[pdx| %_ = @scr |]
    | (front, back) <- break
                        (\substmt -> case substmt of 
                            [pdx| chance = %_ |] -> True
                            _ -> False)
                        scr
      , not (null back)
      , [pdx| %_ = %rhs |] <- head back
      , Just chance <- floatRhs rhs
      = compoundMessage
          (MsgRandomChance chance)
          [pdx| %undefined = @(front ++ tail back) |]
    | otherwise = compoundMessage MsgRandom stmt
random stmt = preStatement stmt

randomList :: Monad m => GenericStatement -> PPT m IndentedMessages
randomList stmt@[pdx| %_ = @scr |] = fmtRandomList $ map entry scr
    where
        entry [pdx| !weight = @scr |] = (fromIntegral weight, scr)
        entry _ = error "Bad clause in random_list"
        fmtRandomList entries = withCurrentIndent $ \i ->
            let total = sum (map fst entries)
            in (:) <$> pure (i, MsgRandom)
                   <*> (concat <$> indentUp (mapM (fmtRandomList' total) entries))
        fmtRandomList' total (wt, what) = withCurrentIndent $ \i ->
            (:) <$> pure (i, MsgRandomChance ((wt / total) * 100))
                <*> ppMany what -- has integral indentUp
-- DLC

hasDlc :: Monad m => GenericStatement -> PPT m IndentedMessages
hasDlc [pdx| %_ = ?dlc |]
    = msgToPP $ MsgHasDLC dlc_icon dlc
    where
        mdlc_key = HM.lookup dlc . HM.fromList $
            [ -- Stellaris has no major DLC yet.
            ]
        dlc_icon = maybe "" iconText mdlc_key
hasDlc stmt = preStatement stmt

-- Switch / Trigger switch

triggerSwitch :: Monad m => GenericStatement -> PPT m IndentedMessages
-- A trigger switch must be of the form
-- trigger_switch = {
--  on_trigger = <statement lhs>
--  <statement rhs> = {
--      <actions>
--  }
-- }
-- where the <statement rhs> block may be repeated several times.
--
-- Switches are the same, but the head is `switch` and the trigger clause is
-- `trigger` instead of `on_trigger`. Semantically `trigger_switch` is used in
-- triggers; `switch` is used in actions.
triggerSwitch stmt@(Statement _ OpEq (CompoundRhs
                    ([pdx| $triggerClause = $condlhs |] -- assume this is first statement
                    :clauses)))
                    | triggerClause `elem` ["trigger", "on_trigger"] = do
    statementsMsgs <- indentUp $ forM clauses $ \clause -> case clause of
        -- using next indent level, for each block <condrhs> = { ... }:
        [pdx| $condrhs = @action |] -> do
            -- construct a fake condition to pp
            let cond = [pdx| $condlhs = $condrhs |]
            ((_, guardMsg):_) <- ppOne cond -- XXX: match may fail (but shouldn't)
            guardText <- messageText guardMsg
            -- pp the rest of the block, at the next level
            statementMsgs <- indentUp (ppMany action)
            withCurrentIndent $ \i -> return $ (i, MsgTriggerSwitchClause guardText) : statementMsgs
        _ -> preStatement stmt
    withCurrentIndent $ \i -> return $ (i, MsgTriggerSwitch) : concat statementsMsgs
triggerSwitch stmt = preStatement stmt

isMonth :: Monad m => GenericStatement -> PPT m IndentedMessages
isMonth [pdx| %_ = !(num :: Int) |] | num >= 1, num <= 12
    = do
        month_loc <- getGameL10n $ case num of
            1 -> "January"
            2 -> "February"
            3 -> "March"
            4 -> "April"
            5 -> "May"
            6 -> "June"
            7 -> "July"
            8 -> "August"
            9 -> "September"
            10 -> "October"
            11 -> "November"
            12 -> "December"
            _ -> error "impossible: tried to localize bad month number"
        msgToPP $ MsgIsMonth month_loc
isMonth stmt = preStatement stmt

customTriggerTooltip :: Monad m => GenericStatement -> PPT m IndentedMessages
customTriggerTooltip [pdx| %_ = @scr |]
    -- ignore the custom tooltip
    = let rest = flip filter scr $ \stmt -> case stmt of
            [pdx| tooltip = %_ |] -> False
            _ -> True
      in indentDown $ ppMany rest
customTriggerTooltip stmt = preStatement stmt
