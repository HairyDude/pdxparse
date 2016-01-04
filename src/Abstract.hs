{-# LANGUAGE OverloadedStrings #-}
module Abstract (
    -- Types
        Statement (..)
    ,   GenericStatement
    ,   GenericScript
    ,   Lhs (..)
    ,   GenericLhs
    ,   Rhs (..)
    ,   GenericRhs
    ,   Date (..)
    -- Constructors
    ,   s_yes
    -- Views
    ,   textRhs, floatRhs, floatOrTextRhs
    -- Presentation
    ,   genericStatement2doc
    ,   genericScript2doc
    ,   displayGenericScript
    -- Parsing
    ,   readScript
    ,   genericStatement
    ) where

{-
    A scripting language as used by Paradox Interactive in games such
    as Crusader Kings II and Europa Universalis IV.

    The language is syntactically very simple. The complete BNF is as follows
    (where ident is an identifier, string is a string literal, and each
    terminal is separated by enough whitespace to distinguish it from adjacent
    ones):

        statement ::= ident | ident "=" rhs
        rhs ::= ident | string | compound_rhs
        compound_rhs ::= "{" statements "}"
        statements ::= statement | statement statements

    Semantics of each statement are defined by the application. Typically the
    RHS of a compound statement will be a "scope", where certain identifiers
    have defined meanings. Specifically:

        THIS = whatever this scope pertains to (its "subject")
        FROM = some other object relevant to this scope (its "object")
        ROOT = THIS of the topmost scope of this particular script
                (the subject of the script as a whole)
        PREV = THIS of the next scope up
        PREVPREV = PREV of the next scope up, etc.
-}

import Control.Applicative hiding ((<$>))
import qualified Data.Foldable as F
import Data.Monoid

import Data.Char
import Data.List

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text as Ap

import Doc
import FileIO
import SettingsTypes

-- statement ::= lhs | lhs '=' rhs
-- Type of statements, parametrized by two custom types, one for left-hand
-- sides and one for right-hand sides.
data Statement lhs rhs
        = StatementBare (Lhs lhs)
        | Statement (Lhs lhs) (Rhs lhs rhs)
    deriving (Eq, Ord, Show, Read)
type GenericStatement = Statement () ()

-- lhs ::= some_custom_lhs | ident
-- Type of statement left-hand sides (the part before the '=').
data Lhs lhs
    = CustomLhs lhs
    | GenericLhs Text
    | IntLhs Int -- used frequently in EU4
    deriving (Eq, Ord, Show, Read)
type GenericLhs = Lhs ()

-- rhs ::= some_custom_rhs | ident | string | "{" statements "}"
-- statements ::= statement | statement statements
-- Type of statement right-hand sides (the part after the '=').
-- Since this is mutually recursive with 'Statement', it also requires the
-- custom LHS type as a parameter.
data Rhs lhs rhs
    = CustomRhs rhs
    | GenericRhs Text
    | StringRhs Text
    | IntRhs Int
    | FloatRhs Double
    | CompoundRhs [Statement lhs rhs]
    | DateRhs Date
    deriving (Eq, Ord, Show, Read)
type GenericRhs = Rhs () ()

type Script lhs rhs = [Statement lhs rhs]
type GenericScript = [GenericStatement]

data Date = Date { year :: Int, month :: Int, day :: Int }
    deriving (Show, Eq, Ord, Read) -- Ord works with fields in this order only

-- A very common type of statement
s_yes :: Text -> Statement lhs rhs
s_yes tok = Statement (GenericLhs tok) (GenericRhs "yes")

-- | Class for painlessly getting the type of number we want out of a value
-- that might have parsed as something else.
class CoerceNum a where
    fromInt :: Int -> a
    -- | If there is an instance @Real a@, the implementation should be
    -- 'round', because 'floor' might cause an off-by-one error.
    fromFloat :: Double -> a
instance CoerceNum Int where
    fromInt = id
    fromFloat = round
instance CoerceNum Double where
    fromInt = fromIntegral
    fromFloat = id

-- Get a number of the desired type from a RHS.
-- If it's a float and we want an int, round it.
floatRhs :: CoerceNum a => GenericRhs -> Maybe a
floatRhs (IntRhs n) = Just (fromInt n)
floatRhs (FloatRhs n) = Just (fromFloat n)
floatRhs _ = Nothing

-- Get a Text from a RHS.
textRhs :: GenericRhs -> Maybe Text
textRhs (GenericRhs s) = Just s
textRhs (StringRhs s) = Just s
textRhs _ = Nothing

floatOrTextRhs :: CoerceNum a => GenericRhs -> Maybe (Either a Text)
floatOrTextRhs rhs = case floatRhs rhs of
    Just n -> Just (Left n)
    Nothing -> Right <$> textRhs rhs

------------
-- Parser --
------------

skipSpace :: Parser ()
skipSpace = Ap.skipMany
            (   (Ap.space >> return ())
            <|> comment)

comment :: Parser ()
comment = Ap.char '#' >> restOfLine >> return ()

restOfLine :: Parser Text
restOfLine = (Ap.many1' Ap.endOfLine >> return T.empty)
         <|> (T.cons <$> Ap.anyChar <*> restOfLine)

ident :: Parser Text
ident = (<>) <$> (T.singleton <$> (Ap.satisfy (\c -> c  == '_' || isAlpha c)))
             <*> Ap.takeWhile (\c -> c `elem` ['_','.'] || isAlphaNum c)
    <?> "identifier"

-- A string literal.
stringLit :: Parser Text
stringLit = "\""
         *> (T.pack <$> Ap.many' stringChar)
         <* "\""
    <?> "string literal"

-- An integer literal.
intLit :: Parser Int
intLit = Ap.signed Ap.decimal

-- A floating-point literal.
floatLit :: Parser Double
floatLit = Ap.signed Ap.double

-- A date literal.
dateLit :: Parser Date
dateLit = Date <$> Ap.decimal
               <*> (Ap.char '.' *> Ap.decimal)
               <*> (Ap.char '.' *> Ap.decimal)

-- A character within a string, possibly escaped.
stringChar :: Parser Char
stringChar = ("\\" *> escapedChar)
         <|> Ap.notChar '"'
escapedChar :: Parser Char
escapedChar = ("0" *> return '\0')
          <|> ("a" *> return '\a')
          <|> ("b" *> return '\b')
          <|> ("e" *> return '\ESC')
          <|> ("f" *> return '\f')
          <|> ("n" *> return '\n')
          <|> ("r" *> return '\r')
          <|> ("t" *> return '\t')
          <|> ("v" *> return '\v')
          <|> ("\"" *> return '"')
          <|> ("\\" *> return '\\')
    <?> "character escape sequence"

statement :: Parser lhs -> Parser rhs -> Parser (Statement lhs rhs)
statement customLhs customRhs
    = Statement <$> lhs customLhs
                -- TODO: permit bare statements (no = and RHS)
                <*  (skipSpace >> Ap.char '=' >> skipSpace)
                <*> rhs customLhs customRhs
    <?> "statement"

script :: Parser lhs -> Parser rhs -> Parser (Script lhs rhs)
script customLhs customRhs = statement customLhs customRhs `Ap.sepBy` skipSpace
    <?> "script"

lhs :: Parser lhs -> Parser (Lhs lhs)
lhs custom = CustomLhs <$> custom
         <|> GenericLhs <$> ident
         <|> IntLhs <$> Ap.decimal
    <?> "statement LHS"

rhs :: Parser lhs -> Parser rhs -> Parser (Rhs lhs rhs)
rhs customLhs customRhs
          = (CustomRhs  <$> customRhs
        <|> GenericRhs  <$> ident
        <|> StringRhs   <$> stringLit
        <|> DateRhs     <$> dateLit
        <|> FloatRhs    <$> floatLit
        <|> IntRhs      <$> intLit
        <|> CompoundRhs <$> compoundRhs customLhs customRhs)
    <?> "statement RHS"

compoundRhs :: Parser lhs -> Parser rhs -> Parser (Script lhs rhs)
compoundRhs customLhs customRhs
    = ("{" >> skipSpace)
      *> script customLhs customRhs
      <* (skipSpace >> "}")
    <?> "compound RHS"

-- | Statement with no custom elements.
-- Use this as a starting point for scripts that use standard syntax.
genericStatement :: Parser GenericStatement
genericStatement = statement parse_generic parse_generic
    where parse_generic = fail "generic"

genericScript :: Parser GenericScript
genericScript = script parse_generic parse_generic
    where parse_generic = fail "generic"

--------------------
-- Pretty-printer --
--------------------

-- Pretty-printer for a script with no custom elements.
genericScript2doc :: GenericScript -> Doc
genericScript2doc = F.fold . intersperse line . map genericStatement2doc

genericStatement2doc :: GenericStatement -> Doc
genericStatement2doc = statement2doc (const "") (const "")

script2doc :: (lhs -> Doc) -> (rhs -> Doc) -> [Statement lhs rhs] -> Doc
script2doc customLhs customRhs
    = foldr (\x y -> mconcat [x, line, y]) mempty
            . intersperse line
            . map (statement2doc customLhs customRhs)

statement2doc :: (lhs -> Doc) -> (rhs -> Doc) -> Statement lhs rhs -> Doc
statement2doc customLhs _ (StatementBare lhs)
    = lhs2doc customLhs lhs
statement2doc customLhs customRhs (Statement lhs rhs)
    = lhs2doc customLhs lhs <++> text "=" <++> rhs2doc customLhs customRhs rhs

lhs2doc :: (lhs -> Doc) -> Lhs lhs -> Doc
lhs2doc customLhs (CustomLhs lhs) = customLhs lhs
lhs2doc _         (GenericLhs lhs) = text (TL.fromStrict lhs)
lhs2doc _         (IntLhs lhs) = text (TL.pack (show lhs))

rhs2doc :: (lhs -> Doc) -> (rhs -> Doc) -> Rhs lhs rhs -> Doc
rhs2doc _ customRhs (CustomRhs rhs) = customRhs rhs
rhs2doc _ _ (GenericRhs rhs) = strictText rhs
rhs2doc _ _ (StringRhs rhs) = text (TL.pack (show rhs))
rhs2doc _ _ (IntRhs rhs) = text (TL.pack (show rhs))
rhs2doc _ _ (FloatRhs rhs) = pp_float rhs
rhs2doc customLhs customRhs (CompoundRhs rhs)
    = text "{" <$$> indent 4 (script2doc customLhs customRhs rhs) <$$> text "}"
rhs2doc _ _ (DateRhs (Date year month day)) =
    mconcat . map (text . TL.pack) $ [show year, ".", show month, ".", show day]

displayGenericScript :: GenericScript -> Text
displayGenericScript script = TL.toStrict . displayT . renderPretty 0.8 80 $ genericScript2doc script

-------------------------------
-- Reading scripts from file --
-------------------------------

readScript :: Settings a -> FilePath -> IO GenericScript
readScript settings file = do
    let filepath = buildPath settings file
    contents <- readFileRetry filepath
    case Ap.parseOnly (skipSpace >> genericScript) contents of
        Right result -> return result
        Left error -> do
            putStrLn $ "Couldn't parse " ++ file ++ ": " ++ error
            return []
