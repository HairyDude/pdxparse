{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Shared.Common (
        StatementHandler
    ,   msgToPP
    ,   isTag
    ,   isPronoun
    ,   flag
    ,   flagText
    ,   icon
    ,   iconText
    ,   plainMsg
    ,   pre_statement
    ,   pre_statement'
    ,   preMessage
    ,   preStatement
    ,   getProvLoc
    ,   MTTH (..)
    ,   newMTTH
    ,   MTTHModifier (..)
    ,   newMTTHMod
    ,   withLocAtom
    ,   withLocAtom2
    ,   withLocAtomIcon
    ,   withLocAtomAndIcon
--    ,   withNonlocAtom
    ,   withNonlocAtom2
    ,   withProvince
    ,   iconKeyFromTable
    ,   iconFileFromTable
    ,   iconFileBFromTable
    ,   iconOrFlag
    ,   tagOrProvince
    ,   numeric
    ,   numericOrTag
    ,   withFlag
    ,   withBool
    ,   withBool'
    ,   withFlagOrBool
    ,   numericIcon
    ,   tryLoc -- TODO: move to Localization or wherever
    ,   textValue
    ,   textAtom
    ) where

import Data.Char (isUpper, toUpper, toLower)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid -- everything

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE

import qualified Data.Set as S

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Abstract -- everything
import Doc -- everything
import Messages (ScriptMessage (MsgUnprocessed), IndentedMessages, messageText, template)
import QQ (pdx)
import SettingsTypes (
        IsGameState (..), GameState (..), IsGameData (..), GameData (..), PPT
    ,   alsoIndent, alsoIndent'
    ,   getGameL10n, getGameL10nDefault, getGameL10nIfPresent)

-- | Convenience synonym.
type StatementHandler g m = GenericStatement -> PPT g m IndentedMessages

msgToPP :: (IsGameState (GameState g), Monad m) => ScriptMessage -> PPT g m IndentedMessages
msgToPP msg = (:[]) <$> alsoIndent' msg

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

-- | Emit flag template if the argument is a tag.
flag :: (IsGameData (GameData g), Monad m) => Text -> PPT g m Doc
flag name =
    if isTag name
        then template "flag" . (:[]) <$> getGameL10n name
        else return $ case T.map toUpper name of
                "ROOT" -> "(Our country)" -- will need editing for capitalization in some cases
                "PREV" -> "(Previously mentioned country)"
                -- Suggestions of a description for FROM are welcome.
                _ -> Doc.strictText name

-- | 'Text' version of 'flag'.
flagText :: (IsGameData (GameData g),
             IsGameState (GameState g),
             Monad m) =>
    Text -> PPT g m Text
flagText = fmap Doc.doc2text . flag

-- Emit icon template.
icon :: HashMap Text Text -> Text -> Doc
icon table what = template "icon" [HM.lookupDefault what what table, "28px"]
iconText :: HashMap Text Text -> Text -> Text
iconText table = Doc.doc2text . icon table

-- | Create a generic message from a piece of text. The rendering function will
-- pass this through unaltered.
plainMsg :: (IsGameState (GameState g), Monad m) => Text -> PPT g m IndentedMessages
plainMsg msg = (:[]) <$> (alsoIndent' . MsgUnprocessed $ msg)

-- | Pretty-print a statement and wrap it in a @<pre>@ element.
pre_statement :: GenericStatement -> Doc
pre_statement stmt = "<pre>" <> genericStatement2doc stmt <> "</pre>"

-- | 'Text' version of 'pre_statement'.
pre_statement' :: GenericStatement -> Text
pre_statement' = Doc.doc2text . pre_statement

-- | Pretty-print a script statement, wrap it in a @<pre>@ element, and emit a
-- generic message for it.
preMessage :: GenericStatement -> ScriptMessage
preMessage = MsgUnprocessed
            . TL.toStrict
            . PP.displayT
            . PP.renderPretty 0.8 80 -- Don't use 'Doc.doc2text', because it uses
                                     -- 'Doc.renderCompact' which is not what
                                     -- we want here.
            . pre_statement

-- | Pretty-print a script statement, wrap it in a @<pre>@ element, and emit a
-- generic message for it at the current indentation level. This is the
-- fallback in case we haven't implemented that particular statement or we
-- failed to understand it.
preStatement :: (IsGameState (GameState g), Monad m) =>
    GenericStatement -> PPT g m IndentedMessages
preStatement stmt = (:[]) <$> alsoIndent' (preMessage stmt)

-- | Get the localization for a province ID, if available. Assumes the province
-- key is "PROV<number>". E.g. in EU4, PROV1 is Stockholm, PROV1234 is Qasr
-- Ibrim.
getProvLoc :: (IsGameData (GameData g), Monad m) =>
    Int -> PPT g m Text
getProvLoc n =
    let provid_t = T.pack (show n)
    in getGameL10nDefault provid_t ("PROV" <> provid_t)

-------------------------
-- mean_time_to_happen --
-------------------------

-- | Data for @mean_time_to_happen@ clauses
data MTTH = MTTH
        {   mtth_years :: Maybe Int
        ,   mtth_months :: Maybe Int
        ,   mtth_days :: Maybe Int
        ,   mtth_modifiers :: [MTTHModifier] -- TODO
        } deriving Show
-- | Data for @modifier@ clauses within @mean_time_to_happen@ clauses
data MTTHModifier = MTTHModifier
        {   mtthmod_factor :: Maybe Double
        ,   mtthmod_conditions :: GenericScript
        } deriving Show
-- | Empty MTTH
newMTTH :: MTTH
newMTTH = MTTH Nothing Nothing Nothing []
-- | Empty MTTH modifier
newMTTHMod :: MTTHModifier
newMTTHMod = MTTHModifier Nothing []

--------------------------------------
-- Game-agnostic statement handlers --
--------------------------------------

-- | Generic handler for a statement whose RHS is a localizable atom.
withLocAtom :: (IsGameData (GameData g),
                IsGameState (GameState g),
                Monad m) =>
    (Text -> ScriptMessage) -> StatementHandler g m
withLocAtom msg [pdx| %_ = ?key |]
    = msgToPP =<< msg <$> getGameL10n key
withLocAtom _ stmt = preStatement stmt

-- | Generic handler for a statement whose RHS is a localizable atom and we
-- need a second one (passed to message as first arg).
withLocAtom2 :: (IsGameData (GameData g),
                 IsGameState (GameState g),
                 Monad m) =>
    ScriptMessage
        -> (Text -> Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withLocAtom2 inMsg msg [pdx| %_ = ?key |]
    = msgToPP =<< msg <$> pure key <*> messageText inMsg <*> getGameL10n key
withLocAtom2 _ _ stmt = preStatement stmt

-- | Generic handler for a statement whose RHS is a localizable atom, where we
-- also need an icon.
withLocAtomAndIcon :: (IsGameData (GameData g),
                       IsGameState (GameState g),
                       Monad m) =>
    HashMap Text Text
        -> Text -- ^ icon name - see
                -- <https://www.eu4wiki.com/Template:Icon Template:Icon> on the wiki
        -> (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withLocAtomAndIcon table iconkey msg [pdx| %_ = $key |]
    = do what <- getGameL10n key
         msgToPP $ msg (iconText table iconkey) what
withLocAtomAndIcon _ _ _ stmt = preStatement stmt

-- | Generic handler for a statement whose RHS is a localizable atom that
-- corresponds to an icon.
withLocAtomIcon :: (IsGameData (GameData g),
                    IsGameState (GameState g),
                    Monad m) =>
    HashMap Text Text
        -> (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withLocAtomIcon table msg stmt@[pdx| %_ = $key |]
    = withLocAtomAndIcon table key msg stmt
withLocAtomIcon table _ stmt = preStatement stmt

-- As withLocAtom but no l10n.
-- Currently unused
--withNonlocAtom :: (Text -> ScriptMessage) -> StatementHandler g m
--withNonlocAtom msg [pdx| %_ = ?text |] = msgToPP $ msg text
--withNonlocAtom _ stmt = preStatement stmt

-- | As withlocAtom but wth no l10n and an additional bit of text.
withNonlocAtom2 :: (IsGameData (GameData g),
                    IsGameState (GameState g),
                    Monad m) =>
    ScriptMessage
        -> (Text -> Text -> ScriptMessage)
        -> StatementHandler g m
withNonlocAtom2 submsg msg [pdx| %_ = ?txt |] = do
    extratext <- messageText submsg
    msgToPP $ msg extratext txt
withNonlocAtom2 _ _ stmt = preStatement stmt

-- | Higher order statement handler for a statement referring to a province.
withProvince :: (IsGameData (GameData g),
                 IsGameState (GameState g),
                 Monad m) =>
    (Text -> ScriptMessage)
        -> StatementHandler g m
withProvince msg [pdx| %lhs = !provid |]
    = withLocAtom msg [pdx| %lhs = $(T.pack ("PROV" <> show (provid::Int))) |]
withProvince _ stmt = preStatement stmt

-- | Given a script atom, return the corresponding icon key, if any.
--
-- Feed this a table of script atom -> icon key.
iconKeyFromTable :: HashMap Text Text -> Text -> Maybe Text
iconKeyFromTable = flip HM.lookup

-- | Given an {{icon}} key, give the corresponding icon file name. Feed this a
-- key -> file name table.
iconFileFromTable :: HashMap Text Text -> Text -> Text
iconFileFromTable table s = HM.lookupDefault s s table
-- | ByteString version of 'iconFile'.
iconFileBFromTable :: HashMap Text Text -> ByteString -> ByteString
iconFileBFromTable table = TE.encodeUtf8 . iconFileFromTable table . TE.decodeUtf8

-- | As 'withLocAtomAndIcon' except
--
-- * say "same as <foo>" if foo refers to a country (in which case, add a flag)
-- * may not actually have an icon (localization file will know if it doesn't)
iconOrFlag :: (IsGameData (GameData g),
               IsGameState (GameState g),
               Monad m) =>
    HashMap Text Text
    -> (Text -> Text -> ScriptMessage)
        -> (Text -> ScriptMessage)
        -> StatementHandler g m
iconOrFlag table iconmsg flagmsg [pdx| %_ = $name |] = msgToPP =<< do
    nflag <- flag name -- laziness means this might not get evaluated
    if isTag name || isPronoun name
        then return . flagmsg . Doc.doc2text $ nflag
        else iconmsg <$> return (iconText table . HM.lookupDefault name name $ table)
                     <*> getGameL10n name
iconOrFlag _ _ _ stmt = plainMsg $ pre_statement' stmt

-- | Handler for statements where RHS is a tag or province id.
tagOrProvince :: (IsGameData (GameData g),
                  IsGameState (GameState g),
                  Monad m) =>
    (Text -> ScriptMessage)
        -> (Text -> ScriptMessage)
        -> StatementHandler g m
tagOrProvince tagmsg provmsg stmt@[pdx| %_ = ?!eobject |]
    = msgToPP =<< case eobject of
            Just (Right tag) -> do -- is a tag
                tagflag <- flag tag
                return . tagmsg . Doc.doc2text $ tagflag
            Just (Left provid) -> do -- is a province id
                prov_loc <- getProvLoc provid
                return . provmsg $ prov_loc
            Nothing -> return (preMessage stmt)
tagOrProvince _ _ stmt = preStatement stmt

-- TODO (if necessary): allow operators other than = and pass them to message
-- handler
-- | Handler for numeric statements.
numeric :: (IsGameState (GameState g), Monad m) =>
    (Double -> ScriptMessage)
        -> StatementHandler g m
numeric msg [pdx| %_ = !n |] = msgToPP $ msg n
numeric _ stmt = plainMsg $ pre_statement' stmt

-- | Handler for statements where the RHS is either a number or a tag.
numericOrTag :: (IsGameData (GameData g),
                 IsGameState (GameState g),
                 Monad m) =>
    (Double -> ScriptMessage)
        -> (Text -> ScriptMessage)
        -> StatementHandler g m
numericOrTag numMsg tagMsg stmt@[pdx| %_ = %rhs |] = msgToPP =<<
    case floatRhs rhs of
        Just n -> return $ numMsg n
        Nothing -> case textRhs rhs of
            Just t -> do -- assume it's a country
                tflag <- flag t
                return $ tagMsg (Doc.doc2text tflag)
            Nothing -> return (preMessage stmt)
numericOrTag _ _ stmt = preStatement stmt

-- | Handler for a statement referring to a country. Use a flag.
withFlag :: (IsGameData (GameData g), IsGameState (GameState g), Monad m) =>
    (Text -> ScriptMessage)
        -> StatementHandler g m
withFlag msg [pdx| %_ = $who |] = msgToPP =<< do
    whoflag <- flag who
    return . msg . Doc.doc2text $ whoflag
withFlag _ stmt = plainMsg $ pre_statement' stmt

-- | Handler for yes-or-no statements.
withBool :: (IsGameState (GameState g), Monad m) =>
    (Bool -> ScriptMessage)
        -> StatementHandler g m
withBool msg stmt = do
    fullmsg <- withBool' msg stmt
    maybe (preStatement stmt)
          return
          fullmsg

-- | Helper for 'withBool'.
withBool' :: (IsGameState (GameState g), Monad m) =>
    (Bool -> ScriptMessage)
        -> GenericStatement
        -> PPT g m (Maybe IndentedMessages)
withBool' msg [pdx| %_ = ?yn |] | T.map toLower yn `elem` ["yes","no","false"]
    = fmap Just . msgToPP $ case T.toCaseFold yn of
        "yes" -> msg True
        "no"  -> msg False
        "false" -> msg False
        _     -> error "impossible: withBool matched a string that wasn't yes, no or false"
withBool' _ _ = return Nothing

-- | Handler for statements whose RHS may be "yes"/"no" or a tag.
withFlagOrBool :: (IsGameData (GameData g),
                   IsGameState (GameState g),
                   Monad m) =>
    (Bool -> ScriptMessage)
        -> (Text -> ScriptMessage)
        -> StatementHandler g m
withFlagOrBool bmsg _ [pdx| %_ = yes |] = msgToPP (bmsg True)
withFlagOrBool bmsg _ [pdx| %_ = no  |]  = msgToPP (bmsg False)
withFlagOrBool _ tmsg stmt = withFlag tmsg stmt

-- | Handler for statements that have a number and an icon.
numericIcon :: (IsGameState (GameState g), Monad m) =>
    HashMap Text Text
        -> Text
        -> (Text -> Double -> ScriptMessage)
        -> StatementHandler g m
numericIcon table the_icon msg [pdx| %_ = !amt |]
    = msgToPP $ msg (iconText table the_icon) amt
numericIcon _ _ _ stmt = plainMsg $ pre_statement' stmt

----------------------
-- Text/value pairs --
----------------------

-- $textvalue
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
-- localization string, it gets wrapped in a @<tt>@ element instead.

-- convenience synonym
tryLoc :: (IsGameData (GameData g), Monad m) => Text -> PPT g m (Maybe Text)
tryLoc = getGameL10nIfPresent

data TextValue = TextValue
        {   tv_what :: Maybe Text
        ,   tv_value :: Maybe Double
        }
newTV :: TextValue
newTV = TextValue Nothing Nothing
textValue :: forall g m. (IsGameState (GameState g), Monad m) =>
    HashMap Text Text                                -- ^ Table of script atom to icon key
        -> Text                                      -- ^ Label for "what"
        -> Text                                      -- ^ Label for "how much"
        -> (Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, if abs value < 1
        -> (Text -> Text -> Double -> ScriptMessage) -- ^ Message constructor, if abs value >= 1
        -> (Text -> PPT g m (Maybe Text)) -- ^ Action to localize, get icon, etc. (applied to RHS of "what")
        -> StatementHandler g m
textValue table whatlabel vallabel smallmsg bigmsg loc stmt@[pdx| %_ = @scr |]
    = msgToPP =<< pp_tv (foldl' addLine newTV scr)
    where
        addLine :: TextValue -> GenericStatement -> TextValue
        addLine tv [pdx| $label = ?what |] | label == whatlabel
            = tv { tv_what = Just what }
        addLine tv [pdx| $label = !val |] | label == vallabel
            = tv { tv_value = Just val }
        addLine nor _ = nor
        pp_tv :: TextValue -> PPT g m ScriptMessage
        pp_tv tv = case (tv_what tv, tv_value tv) of
            (Just what, Just value) -> do
                mwhat_loc <- loc what
                let what_icon = iconText table what
                    what_loc = fromMaybe ("<tt>" <> what <> "</tt>") mwhat_loc
                return $ (if abs value < 1 then smallmsg else bigmsg) what_icon what_loc value
            _ -> return $ preMessage stmt
textValue _ _ _ _ _ _ stmt = preStatement stmt

-- | Statements of the form
-- @
--      has_trade_modifier = {
--          who = ROOT
--          name = merchant_recalled
--      }
-- @
data TextAtom = TextAtom
        {   ta_what :: Maybe Text
        ,   ta_atom :: Maybe Text
        }
newTA :: TextAtom
newTA = TextAtom Nothing Nothing
textAtom :: forall g m. (IsGameData (GameData g),
                         IsGameState (GameState g),
                         Monad m) =>
    HashMap Text Text -- ^ Table of script atom to icon key
        -> Text -- ^ Label for "what" (e.g. "who")
        -> Text -- ^ Label for atom (e.g. "name")
        -> (Text -> Text -> Text -> ScriptMessage) -- ^ Message constructor
        -> (Text -> PPT g m (Maybe Text)) -- ^ Action to localize, get icon, etc. (applied to RHS of "what")
        -> StatementHandler g m
textAtom table whatlabel atomlabel msg loc stmt@[pdx| %_ = @scr |]
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
        pp_ta :: TextAtom -> PPT g m ScriptMessage
        pp_ta ta = case (ta_what ta, ta_atom ta) of
            (Just what, Just atom) -> do
                mwhat_loc <- loc what
                atom_loc <- getGameL10n atom
                let what_icon = iconText table what
                    what_loc = fromMaybe ("<tt>" <> what <> "</tt>") mwhat_loc
                return $ msg what_icon what_loc atom_loc
            _ -> return $ preMessage stmt
textAtom _ _ _ _ _ stmt = preStatement stmt
