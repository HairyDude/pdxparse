{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Common where

import Debug.Trace

import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>), int, double)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract
import Localization

strictText :: Text -> Doc
strictText = text . TL.fromStrict

pp_script :: Int -> L10n -> GenericScript -> Doc
pp_script indent l10n script
    = hcat . punctuate line
        . map ((mconcat (replicate indent "*" ++ [" "]) <>) . pp_statement' indent l10n) $ script

-- Pretty-print a number, putting a + sign in front if it's not negative.
-- Assumes the passed-in formatting function does add a minus sign.
pp_signed :: (Ord n, Num n) => (n -> Doc) -> n -> Doc
pp_signed pp_num n = (if signum n >= 0 then "+" else "") <> pp_num n

-- Pretty-print a number, adding wiki formatting:
-- * {{green}} if good
-- * {{red}} if bad
-- * '''boldface''' if neutral
-- What is good or bad is determined by the first argument:
-- * if True, positive is good and negative is bad (e.g. stability)
-- * if False, negative is good and positive is bad (e.g. unrest)
pp_hl_num :: (Ord n, Num n) => Bool -> (n -> Doc) -> n -> Doc
pp_hl_num pos pp_num n =
    let sign = signum n
        positivity = if pos then sign else negate sign
        n_pp'd = pp_signed pp_num n
    in case positivity of
        -1 -> template "red" n_pp'd
        0 ->  bold n_pp'd
        1 ->  template "green" n_pp'd

-- Pretty-print a Double. If it's a whole number, display it without a decimal.
pp_float :: Double -> Doc
pp_float n =
    let trunc = floor n
    in if fromIntegral trunc == n
        then PP.int (fromIntegral trunc)
        else PP.double n

-- Simple template (one arg).
-- NB: This does not perform escaping of pipes, because I don't know how to do
-- it with Docs.
template :: Text -> Doc -> Doc
template name content = hcat ["{{", strictText name, "|", content, "}}"]

-- Emit flag template, unless the argument is "ROOT".
flag :: Text -> Doc
flag name = let name' = strictText name
    in if name == "ROOT" then name' else template "flag" name'

icon :: Text -> Doc
icon what = template "icon" (strictText what)

italic :: Doc -> Doc
italic content = enclose "''" "''" content

bold :: Doc -> Doc
bold content = enclose "'''" "'''" content

labelIcon :: Doc -> Doc -> Doc
labelIcon label content = hsep [template "icon" label, content]

pre_statement :: GenericStatement -> Doc
pre_statement stmt = "<pre>" <> genericStatement2doc stmt <> "</pre>"

-- Pretty-print a statement.
-- Most statements are expected to be of a particular form. If they're not, we
-- just echo the statement instead of failing. This is also what we do with
-- unrecognized statements.
pp_statement :: L10n -> GenericStatement -> Doc
pp_statement = pp_statement' 1

pp_statement' :: Int -> L10n -> GenericStatement -> Doc
pp_statement' indent l10n stmt@(Statement lhs rhs) =
    let defaultdoc = pre_statement stmt
        -- not computed if not needed, thanks to laziness
    in case lhs of
        GenericLhs label -> case label of
            -- Add or subtract something
            "add_adm_power" -> add_power ADM stmt
            "add_dip_power" -> add_power DIP stmt
            "add_mil_power" -> add_power MIL stmt
            "add_faction_influence" -> faction_influence stmt
            "add_years_of_income" -> add_years_of_income stmt
            "add_army_tradition" -> gain True "army tradition" "army tradition" stmt
            "add_prestige" -> gain True "prestige" "prestige" stmt
            "add_stability" -> gain True "stability" "stability" stmt
            "add_inflation" -> gain False "inflation" "inflation" stmt
            "add_base_tax" -> gain False "base tax" "base tax" stmt
            "add_legitimacy" -> gain False "legitimacy" "legitimacy" stmt
            "add_province_modifier" -> add_modifier "province" l10n stmt
            "add_permanent_province_modifier" -> add_modifier "permanent province" l10n stmt
            "add_country_modifier" -> add_modifier "country" l10n stmt
            "remove_country_modifier" -> remove_modifier "country" l10n stmt
            "add_opinion" -> add_opinion l10n stmt
            "spawn_rebels" -> spawn_rebels l10n stmt
            -- Compound statements
            "every_owned_province" -> case rhs of
                CompoundRhs scr -> "Every owned province:" <> line
                                    <> pp_script (succ indent) l10n scr
                _ -> defaultdoc
            "random_owned_province" -> case rhs of
                CompoundRhs scr -> "One random owned province:" <> line
                                    <> pp_script (succ indent) l10n scr
                _ -> defaultdoc
            "every_known_country" -> case rhs of
                CompoundRhs scr -> "Every known country:" <> line
                                    <> pp_script (succ indent) l10n scr
                _ -> defaultdoc
            "random_neighbor_province" -> case rhs of
                CompoundRhs scr -> "One random neighbor province:" <> line
                                    <> pp_script (succ indent) l10n scr
                _ -> defaultdoc
            "limit" -> case rhs of
                CompoundRhs scr -> "Limited to:" <> line
                                    <> pp_script (succ indent) l10n scr
                _ -> defaultdoc
            "hidden_effect" -> case rhs of
                CompoundRhs scr -> "Hidden effect:" <> line
                                    <> pp_script (succ indent) l10n scr
                _ -> defaultdoc
            "NOT" -> case rhs of
                CompoundRhs scr -> "None of:" <> line
                                    <> pp_script (succ indent) l10n scr
                _ -> defaultdoc
            "AND" -> case rhs of
                CompoundRhs scr -> "All of:" <> line
                                    <> pp_script (succ indent) l10n scr
                _ -> defaultdoc
            "OR" -> case rhs of
                CompoundRhs scr -> "At least one of:" <> line
                                    <> pp_script (succ indent) l10n scr
                _ -> defaultdoc
            "if" -> case rhs of
                CompoundRhs scr -> "If:" <> line
                                    <> pp_script (succ indent) l10n scr
                _ -> defaultdoc
            -- Simple generic statements
            "continent" -> simple_generic l10n "Continent is" stmt
            "culture" -> simple_generic l10n "Culture is" stmt
            "government" -> simple_generic l10n "Government is" stmt
            "change_government" -> simple_generic l10n "Change government to" stmt
            "region" -> simple_generic l10n "Is in region" stmt
            -- Simple generic statements (typewriter face)
            "set_country_flag" -> simple_generic_tt "Set country flag" stmt
            "set_province_flag" -> simple_generic_tt "Set province flag" stmt
            "has_province_flag" -> simple_generic_tt "Has province flag" stmt
            "clr_province_flag" -> simple_generic_tt "Clear province flag" stmt
            -- Simple generic statements with icon
            "trade_goods" -> simple_generic_icon l10n "Produces" stmt
            "religion" -> simple_generic_icon l10n "Religion is" stmt
            "change_religion" -> simple_generic_icon l10n "Change religion to" stmt
            -- Simple generic statements with flag
            "has_discovered" -> simple_generic_tag l10n "Has discovered" stmt
            "is_core" -> simple_generic_tag l10n "Is core of" stmt
            "owned_by" -> simple_generic_tag l10n "Is owned by" stmt
            -- Boolean statements
            "has_port" -> has "a port" stmt
            "is_reformation_center" -> is "a center of reformation" stmt
            "is_capital" -> is "capital" stmt
            -- Numeric statements
            "create_revolt" -> simple_numeric "Create size " stmt " revolt"
            "base_tax" -> simple_numeric "Base tax is at least " stmt ""
            "stability" -> simple_numeric "Stability is at least " stmt ""
            -- Ignored
            "custom_tooltip" -> "(Custom tooltip - delete this line)"
            -- default
            _ -> if T.length label == 3
                    && T.all isAlpha label
                    && T.all isUpper label
                 then case rhs of -- Treat as a country tag
                    CompoundRhs scr ->
                        flag (HM.lookupDefault label label l10n)
                        <> ":"
                        <> line <> pp_script (succ indent) l10n scr
                    _ -> defaultdoc
                 else defaultdoc
        IntLhs n -> case rhs of -- Treat as a province tag
            CompoundRhs scr ->
                let provN = T.pack (show n)
                in "Province" <> space
                    <> strictText (HM.lookupDefault ("Province " <> provN) ("PROV" <> provN) l10n)
                    <> ":"
                    <> line <> pp_script (succ indent) l10n scr
            _ -> defaultdoc


------------------------------------------------------------------------
-- Script handlers that should be used directly, not via pp_statement --
------------------------------------------------------------------------

data MTTH = MTTH
        {   years :: Maybe Int
        ,   months :: Maybe Int
        ,   days :: Maybe Int
--        ,   factors :: [GenericStatement]
        } deriving Show
newMTTH = MTTH Nothing Nothing Nothing --[]
addField mtth _ = mtth -- unrecognized
pp_mtth :: L10n -> GenericScript -> Doc
pp_mtth l10n scr
    = pp_mtth $ foldl' addField newMTTH scr
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
            = mtth -- TODO
        pp_mtth mtth@(MTTH years months days) =
            let hasYears = isJust years
                hasMonths = isJust months
                hasDays = isJust days
            in mconcat $
                ((if hasYears then
                    [PP.int (fromJust years), space, "years"]
                    ++
                    if hasMonths && hasDays then [",", space]
                    else if hasMonths || hasDays then ["and", space]
                    else []
                 else [])
                ++
                (if hasMonths then
                    [PP.int (fromJust months), space, "months"]
                 else [])
                ++
                (if hasDays then
                    (if hasYears && hasMonths then ["and", space]
                     else []) -- if years but no months, already added "and"
                    ++
                    [PP.int (fromJust days), space, "days"]
                 else []))

---------------------------------
-- Specific statement handlers --
---------------------------------

data Power = ADM | DIP | MIL

add_power :: Power -> GenericStatement -> Doc
add_power pwr stmt@(Statement _ rhs) =
    let pwr_label = case pwr of
            ADM -> "adm"
            DIP -> "dip"
            MIL -> "mil"
    in case rhs of
        IntRhs amt -> labelIcon pwr_label (pp_hl_num True PP.int amt)
        FloatRhs amt -> labelIcon pwr_label (pp_hl_num True pp_float amt)
        _ -> pre_statement stmt


data FactionInfluence = FactionInfluence {
        faction :: Maybe Text
    ,   influence :: Maybe Double
    }
newInfluence = FactionInfluence Nothing Nothing
faction_influence :: GenericStatement -> Doc
faction_influence stmt@(Statement _ (CompoundRhs scr))
    = pp_influence $ foldl' addField newInfluence scr
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

add_years_of_income :: GenericStatement -> Doc
add_years_of_income stmt
    | Statement _ (IntRhs n)   <- stmt = add_years_of_income' (fromIntegral n)
    | Statement _ (FloatRhs n) <- stmt = add_years_of_income' n
    where
        add_years_of_income' howmuch = hsep
            [if howmuch < 0 then "Lose" else "Gain"
            ,icon "ducats"
            ,"ducats equal to"
            ,pp_float (abs howmuch)
            ,if abs howmuch == 1 then "year" else "years"
            ,"of income"
            ]

-- "Gain" or "Lose" simple numbers, e.g. army tradition.
-- First text argument is the icon key, second is text.
-- Bool is whether a gain is good.
gain :: Bool -> Text -> Text -> GenericStatement -> Doc
gain good iconkey what stmt
    | Statement _ (IntRhs n)   <- stmt = gain' (fromIntegral n)
    | Statement _ (FloatRhs n) <- stmt = gain' n
    where
        gain' howmuch = hsep
            [if howmuch < 0 then "Lose" else "Gain"
            ,icon iconkey
            ,pp_hl_num good pp_float howmuch
            ,strictText what
            ]

data AddModifier = AddModifier {
        name :: Maybe Text
    ,   duration :: Maybe Double
    } deriving Show
newAddModifier = AddModifier Nothing Nothing

add_modifier :: Text -> L10n -> GenericStatement -> Doc
add_modifier kind l10n stmt@(Statement _ (CompoundRhs scr))
    = pp_add_modifier $ foldl' addLine newAddModifier scr
    where
        addLine :: AddModifier -> GenericStatement -> AddModifier 
        addLine apm (Statement (GenericLhs "name") (GenericRhs name)) = apm { name = Just name }
        addLine apm (Statement (GenericLhs "duration") (FloatRhs duration)) = apm { duration = Just duration }
        addLine apm _ = apm -- e.g. hidden = yes
        pp_add_modifier :: AddModifier -> Doc
        pp_add_modifier apm
            = if isJust (name apm) then
                let dur = fromJust (duration apm)
                in hsep $
                    ["Add", strictText kind, "modifier"
                    ,dquotes (strictText $
                        let key = fromJust . name $ apm
                        in  HM.lookupDefault key key l10n)
                    ]
                    ++ if isJust (duration apm) then
                        if dur < 0 then ["indefinitely"] else
                        ["for"
                        ,pp_float dur
                        ,"days"
                        ]
                    else []
              else pre_statement stmt
add_modifier _ _ stmt = pre_statement stmt

remove_modifier :: Text -> L10n -> GenericStatement -> Doc
remove_modifier kind l10n (Statement _ (GenericRhs label))
    = hsep
        ["Remove", strictText kind, "modifier"
        ,dquotes (strictText $ HM.lookupDefault label label l10n)
        ]
remove_modifier _ _ stmt = pre_statement stmt

-- Statement with generic on both sides translating to the form
--  <string> <l10n value>
simple_generic :: L10n -> Text -> GenericStatement -> Doc
simple_generic l10n premsg (Statement _ (GenericRhs name))
    = hsep [strictText $ premsg, strictText $ HM.lookupDefault name name l10n]
simple_generic _ _ stmt = pre_statement stmt

-- As simple_generic but definitely no l10n. Set the RHS in typewriter face
simple_generic_tt :: Text -> GenericStatement -> Doc
simple_generic_tt premsg (Statement _ (GenericRhs name))
    = mconcat [strictText $ premsg, space, "<tt>", strictText name, "</tt>"]
simple_generic_tt _ stmt = pre_statement stmt

-- As simple_generic but also add an appropriate icon before the value.
simple_generic_icon :: L10n -> Text -> GenericStatement -> Doc
simple_generic_icon l10n premsg (Statement _ (GenericRhs name))
    = hsep
        [strictText $ premsg
        ,icon name
        ,strictText $ HM.lookupDefault name name l10n]
simple_generic_icon _ _ stmt = pre_statement stmt

simple_numeric :: Text -> GenericStatement -> Text -> Doc
simple_numeric premsg (Statement _ (IntRhs n)) postmsg
    = mconcat
        [strictText premsg
        ,PP.int n
        ,strictText postmsg
        ]
simple_numeric premsg (Statement _ (FloatRhs n)) postmsg
    = mconcat
        [strictText premsg
        ,pp_float n
        ,strictText postmsg
        ]
simple_numeric _ stmt _ = pre_statement stmt

has :: Text -> GenericStatement -> Doc
has what (Statement _ (GenericRhs "yes"))
    = hsep
        ["Has"
        ,strictText what
        ]
has what (Statement _ (GenericRhs "no"))
    = hsep
        ["Does NOT have"
        ,strictText what
        ]
has _ stmt = pre_statement stmt

is :: Text -> GenericStatement -> Doc
is what (Statement _ (GenericRhs "yes"))
    = hsep
        ["Is"
        ,strictText what
        ]
is what (Statement _ (GenericRhs "no"))
    = hsep
        ["Is NOT"
        ,strictText what
        ]
is _ stmt = pre_statement stmt

simple_generic_tag :: L10n -> Text -> GenericStatement -> Doc
simple_generic_tag l10n prefix (Statement _ (GenericRhs who))
    = hsep
        [strictText prefix
        ,flag $ HM.lookupDefault who who l10n
        ]
simple_generic_tag _ _ stmt = pre_statement stmt

data AddOpinion = AddOpinion {
        who :: Maybe Text
    ,   modifier :: Maybe Text
    ,   op_years :: Maybe Double
    } deriving Show
newAddOpinion = AddOpinion Nothing Nothing Nothing

add_opinion :: L10n -> GenericStatement -> Doc
add_opinion l10n stmt@(Statement _ (CompoundRhs scr))
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
            = if isJust (who op) && isJust (modifier op) then
                let whom = fromJust (who op)
                    mod = fromJust (modifier op)
                in hsep $
                    ["Add opinion modifier"
                    ,dquotes $ strictText (HM.lookupDefault mod mod l10n)
                    ,"towards"
                    ,flag $ HM.lookupDefault whom whom l10n
                    ]
                    ++ if isNothing (op_years op) then [] else
                    ["for"
                    ,pp_float (fromJust (op_years op))
                    ,"years"
                    ]
              else pre_statement stmt
add_opinion _ stmt = pre_statement stmt

data SpawnRebels = SpawnRebels {
        rebelType :: Maybe Text
    ,   rebelSize :: Maybe Double
    ,   friend :: Maybe Text
    ,   win :: Maybe Bool
    ,   unrest :: Maybe Double -- rebel faction progress
    } deriving Show
newSpawnRebels = SpawnRebels Nothing Nothing Nothing Nothing Nothing

spawn_rebels :: L10n -> GenericStatement -> Doc
spawn_rebels l10n stmt@(Statement _ (CompoundRhs scr))
    = pp_spawn_rebels $ foldl' addLine newSpawnRebels scr
    where
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
        addLine op _ = op
        pp_spawn_rebels :: SpawnRebels -> Doc
        pp_spawn_rebels reb
            = if isJust (rebelType reb) && isJust (rebelSize reb) then
                let rtype = fromJust (rebelType reb)
                    rsize = fromJust (rebelSize reb)
                    friendlyTo = fromJust (friend reb) -- not evaluated if Nothing
                    reb_unrest = fromJust (unrest reb)
                in (hsep $
                       [strictText (HM.lookupDefault rtype (rtype <> "_title") l10n)
                       ,"rebels"
                       ,PP.parens $ hsep ["size", pp_float (fromJust (rebelSize reb))]
                       ] ++ if isJust (friend reb) then
                       [PP.parens $ hsep ["friendly", "to",
                                            strictText (HM.lookupDefault friendlyTo friendlyTo l10n)]
                       ] else []
                       ++
                       ["rise in revolt"
                       ] ++ if isJust (win reb) && fromJust (win reb) then
                       [hsep ["and", "occupy", "the", "province"]
                       ] else []
                    ) <> if isJust (unrest reb) then
                    hsep
                       [","
                       ,"gaining"
                       ,pp_float reb_unrest
                       ,hsep ["progress","towards","the","next","uprising"]
                       ]
                    else mempty
            else pre_statement stmt
