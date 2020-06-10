{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-|
Module      : Abstract
Description : Generic types and parsers for the Clausewitz engine scripting language

A scripting language as used by Paradox Interactive in its games using the
Clasewitz engine, such as Crusader Kings II and Europa Universalis IV.

The language is syntactically very simple. The BNF is as follows (where ident
is an identifier, string is a string literal, and each terminal is separated by
enough whitespace to distinguish it from adjacent ones):

@
    script ::= statements
    statement ::= lhs | ident operator rhs
    lhs ::= \"@\" ident | ident | integer
    operator ::= \"=\" | \"<\" | \">\"
    rhs ::= ident | string | date | number | compound_rhs
    compound_rhs ::= \"{\" statements \"}\"
    statements ::= statement | statement statements
@

Semantics of each statement are defined by the application. Typically the RHS
of a compound statement will be a "scope", where certain identifiers have
defined meanings. Specifically:

    * THIS = whatever this scope pertains to (its "subject")
    * FROM = some other object relevant to this scope (its "object")
    * ROOT = THIS of the topmost scope of this particular script
              (the subject of the script as a whole)
    * PREV = THIS of the next scope up
    * PREVPREV = PREV of the next scope up, etc.

-}
module Abstract (
    -- Types
        Statement (..)
    ,   GenericStatement
    ,   GenericScript
    ,   Lhs (..)
--    ,   GenericLhs
    ,   Operator (..)
    ,   Rhs (..)
    ,   GenericRhs
    ,   Date (..)
    -- Views
    ,   textRhs, floatRhs, floatOrTextRhs
    -- Presentation
    ,   genericStatement2doc
--    ,   genericScript2doc
--    ,   displayGenericScript
    -- Parsing
    ,   skipSpace
--    ,   genericStatement
    ,   genericScript
    ,   statement
    ,   ident
    ) where


import Control.Applicative (Applicative (..), Alternative (..))
import Control.Monad (void)
import qualified Data.Foldable as F
import Data.Monoid (Monoid (..), (<>))

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (intersperse)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text as Ap

import Text.PrettyPrint.Leijen.Text (Doc, (<++>))
import qualified Text.PrettyPrint.Leijen.Text as PP

import qualified Doc

-- | Type of statements, parametrized by two custom types, one for left-hand
-- sides and one for right-hand sides.
--
-- @
--  statement ::= lhs | lhs '=' rhs
-- @
data Statement lhs rhs
        = StatementBare (Lhs lhs)
        | Statement (Lhs lhs) Operator (Rhs lhs rhs)
    deriving (Eq, Ord, Show, Read)
-- | Statement with no custom components.
type GenericStatement = Statement () ()

-- | The operator between the two sides of a statement. Usually an equals sign,
-- but can be less than or greater than.
--
-- @
--  operator ::= \"=\" | \"<\" | \">\"
-- @
data Operator
    = OpEq -- | \"=\", the most common
    | OpLT -- | \"<\"
    | OpGT -- | \">\"
    deriving (Eq, Ord, Show, Read)

-- | Produce text representation of an Operator.
showOp :: Operator -> Text
showOp OpEq = "="
showOp OpLT = "<"
showOp OpGT = ">"

-- | Produce Doc representation of an Operator.
showOpD :: Operator -> Doc
showOpD = Doc.strictText . showOp

-- | Type of statement left-hand sides (the part before the operator).
--
-- @
--  lhs ::= some_custom_lhs | ident
-- @
data Lhs lhs
    = CustomLhs lhs
    | GenericLhs Text [Text]  -- ^ foo = ..., foo:bar = ... etc.
    | AtLhs Text                    -- ^ @foo = ...
    | IntLhs Int                    -- ^ 1234 = ...
    deriving (Eq, Ord, Show, Read)
-- | LHS with no custom elements.
type GenericLhs = Lhs ()

-- | Type of statement right-hand sides (the part after the '='). Since this is
-- mutually recursive with 'Statement', it also requires the custom LHS type as
-- a parameter.
--
-- @
--  rhs ::= some_custom_rhs | ident | string | "{" statements "}"
--  statements ::= statement | statement statements
-- @
data Rhs lhs rhs
    = CustomRhs rhs
    | GenericRhs Text [Text]        -- ^ ... = foo or ... = foo:bar or ... = foo:bar:baz etc.
    | StringRhs Text                -- ^ ... = "foo"
    | IntRhs Int                    -- ^ ... = 1234
    | FloatRhs Double               -- ^ ... = 1.234
    | CompoundRhs [Statement lhs rhs] -- ^ ... = { ... }
    | DateRhs Date                  -- ^ ... = 11.10.1234
    deriving (Eq, Ord, Show, Read)
-- | RHS with no custom elements.
type GenericRhs = Rhs () ()

-- | Script, i.e. list of statements.
type Script lhs rhs = [Statement lhs rhs]
-- | Script, i.e. list of statements, with no custom elements.
type GenericScript = [GenericStatement]

-- | Being an engine for historical games, Clausewitz-script has a literal
-- syntax for dates.  For example, the start date of Europa Universalis IV is
-- 11 November 1444, represented as @1444.11.11@. It is big-endian
-- (YYYY.MM.DD).
data Date = Date { year :: Int, month :: Int, day :: Int }
    deriving (Show, Eq, Ord, Read) -- Ord works with fields in this order only

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

-- | Get a number of the desired type from a RHS. If it's a float and we want
-- an integer, round it.
floatRhs :: CoerceNum a => GenericRhs -> Maybe a
floatRhs (IntRhs n) = Just (fromInt n)
floatRhs (FloatRhs n) = Just (fromFloat n)
floatRhs _ = Nothing

-- | Get a Text from a RHS.
textRhs :: GenericRhs -> Maybe Text
textRhs (GenericRhs s mt) = Just (s <> mconcat (map (":" <>) mt))
textRhs (StringRhs s) = Just s
textRhs _ = Nothing

-- | Get either a number or a Text from a RHS.
floatOrTextRhs :: CoerceNum a => GenericRhs -> Maybe (Either a Text)
floatOrTextRhs rhs = case floatRhs rhs of
    Just n -> Just (Left n)
    Nothing -> Right <$> textRhs rhs

------------
-- Parser --
------------

-- | Skip whitespace, including comments. A comment is a \"#\" followed by the
-- rest of the line.
skipSpace :: Parser ()
skipSpace = Ap.skipMany
            (   (void Ap.space)
            <|> comment)

-- | A comment is a # followed by the rest of the line. This parser also
-- consumes any number of following blank lines.
comment :: Parser ()
comment = "#" >> restOfLine >> return ()

-- | Parse the entire rest of the line, and also consume any number of
-- following blank lines.
restOfLine :: Parser Text
restOfLine = (Ap.many1' Ap.endOfLine >> return "")
         <|> (T.cons <$> Ap.anyChar <*> restOfLine)

-- | An identifier, or atom. An atom can start with a letter, an underscore or
-- a number and continue with letters, numbers, underscores, and full stops.
ident :: Parser Text
ident = do
        res <- (<>) <$> (T.singleton <$> (Ap.satisfy (\c -> c  == '_' || isAlphaNum c)))
                    <*> Ap.takeWhile (\c -> c `elem` ['_','.'] || isAlphaNum c)
        if T.all isDigit res
            then fail "ident: numeric identifier"
            else return res
    <?> "identifier"

-- | A string literal: any number of characters other than a double quotation
-- mark (possibly escaped), between two double quotation marks. Literal
-- newlines are allowed.
stringLit :: Parser Text
stringLit = "\""
         *> (T.pack <$> Ap.many' stringChar)
         <* "\""
    <?> "string literal"

-- | An integer literal, possibly prefixed with a sign. Must be followed by
-- whitespace or eof to prevent confusion with identifiers that begin with
-- digits.
intLit :: Parser Int
intLit = Ap.signed Ap.decimal
    <* (do
        mnext <- Ap.peekChar
        case mnext of
            Nothing -> return () -- EOF, all good
            Just c -> if (c == '}' || c == '#' || isSpace c)
                        then return () -- space or comment follows
                        else fail "intLit")
    <?> "integer literal"

-- | A floating-point literal. Must be followed by whitespace or eof to prevent
-- confusion with identifiers that begin with digits.
floatLit :: Parser Double
floatLit = Ap.signed Ap.double
    <* (do
        mnext <- Ap.peekChar
        case mnext of
            Nothing -> return () -- EOF, all good
            Just c -> if (c == '}' || c == '#' || isSpace c)
                        then return () -- space or comment follows
                        else fail "intLit")

-- | A date literal.
--
-- @
--  datelit ::= integer \".\" integer \".\" integer
-- @
dateLit :: Parser Date
dateLit = Date <$> Ap.decimal
               <*> (Ap.char '.' *> Ap.decimal)
               <*> (Ap.char '.' *> Ap.decimal)
    <?> "date literal"

-- | A character within a string, possibly escaped. C escape sequences are
-- supported, other than character references.
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

-- | An entire statement. This usually contains an operator, but some
-- statements are lists of data items, e.g. strings or numbers.
--
-- Currently bare statements (those with no operator or RHS) are not supported.
--
-- @
--  statement ::= ident | ident operator rhs
-- @
statement :: Parser lhs -> Parser rhs -> Parser (Statement lhs rhs)
statement customLhs customRhs
    = Statement <$> lhs customLhs
                -- TODO: permit bare statements (no operator and RHS)
                <*> (skipSpace *> operator <* skipSpace)
                <*> rhs customLhs customRhs
    <?> "statement"

-- | A script (i.e. list of statements separated by whitespace), possibly with
-- custom elements.
--
-- @
--  script ::= statements
--  statements ::= statement | statement statements
-- @
script :: Parser lhs -> Parser rhs -> Parser (Script lhs rhs)
script customLhs customRhs = statement customLhs customRhs `Ap.sepBy` skipSpace
    <?> "script"

-- | Statement LHS, possibly with custom elements.
--
-- @
--  lhs ::= \"@\" ident | ident | integer
-- @
lhs :: Parser lhs -> Parser (Lhs lhs)
lhs custom = CustomLhs <$> custom
         <|> AtLhs <$> ("@" *> ident) -- guessing at the syntax here...
         <|> GenericLhs <$> ident <*> Ap.option [] (":" *> ident `Ap.sepBy'` ":")
         <|> IntLhs <$> Ap.decimal
    <?> "statement LHS"

-- | An operator.
--
-- @
--  operator ::= \"<\" | \"=\" | \">\"
-- @
operator :: Parser Operator
operator = "=" *> pure OpEq
       <|> "<" *> pure OpLT
       <|> ">" *> pure OpGT
   <?> "operator"

-- | Statement RHS, possibly with custom elements. Since this may be a
-- compound, it is mutually recursive with 'statement' and so needs to know the
-- custom LHS as well.
--
-- @
--  rhs ::= ident | string | date | number | compound_rhs
-- @
rhs :: Parser lhs -> Parser rhs -> Parser (Rhs lhs rhs)
rhs customLhs customRhs
          = (CustomRhs  <$> customRhs
        <|> StringRhs   <$> stringLit
        <|> DateRhs     <$> dateLit
        <|> IntRhs      <$> intLit
        <|> FloatRhs    <$> floatLit
        <|> GenericRhs  <$> ident <*> Ap.many' (":" *> ident)
        <|> CompoundRhs <$> compoundRhs customLhs customRhs)
    <?> "statement RHS"

-- | A RHS that consists of multiple statements grouped by braces. Frequently
-- this will introduce a new scope, but not always.
--
-- @
--  compound_rhs ::= \"{\" statements \"}\"
-- @
compoundRhs :: Parser lhs -> Parser rhs -> Parser (Script lhs rhs)
compoundRhs customLhs customRhs
    = ("{" *> skipSpace)
      *> script customLhs customRhs
      <* (skipSpace *> "}")
    <?> "compound RHS"

-- | A statement with no custom elements. Use this as a starting point for
-- scripts that use standard syntax.
genericStatement :: Parser GenericStatement
genericStatement = statement parse_generic parse_generic
    where parse_generic = fail "generic"

-- | A script, i.e. a list of statements separated by space. No custom
-- elements. This is not guaranteed to parse the entire file; it will stop at a
-- parse failure.
genericScript :: Parser GenericScript
genericScript = script parse_generic parse_generic
    where parse_generic = fail "generic"

--------------------
-- Pretty-printer --
--------------------

-- | Pretty-printer for a script with no custom elements.
genericScript2doc :: GenericScript -> Doc
genericScript2doc = F.fold . intersperse PP.line . map genericStatement2doc

-- | Pretty-printer for a statement with no custom elements.
genericStatement2doc :: GenericStatement -> Doc
genericStatement2doc = statement2doc (const "") (const "")

-- | Pretty-printer for a script, possibly with custom elements.
script2doc :: (lhs -> Doc) -> (rhs -> Doc) -> [Statement lhs rhs] -> Doc
script2doc customLhs customRhs
    = PP.vsep . map (statement2doc customLhs customRhs)

-- | Pretty-printer for a statement, possibly with custom elements.
statement2doc :: (lhs -> Doc) -> (rhs -> Doc) -> Statement lhs rhs -> Doc
statement2doc customLhs _ (StatementBare lhs)
    = lhs2doc customLhs lhs
statement2doc customLhs customRhs (Statement lhs op rhs)
    = lhs2doc customLhs lhs <++> showOpD op <++> rhs2doc customLhs customRhs rhs

-- | Pretty-printer for a LHS, possibly with custom elements.
lhs2doc :: (lhs -> Doc) -> Lhs lhs -> Doc
lhs2doc customLhs (CustomLhs lhs) = customLhs lhs
lhs2doc _         (AtLhs lhs) = Doc.strictText ("@" <> lhs)
lhs2doc _         (GenericLhs s ts) = Doc.strictText s <> mconcat (intersperse ":" (map Doc.strictText ts))
lhs2doc _         (IntLhs lhs) = PP.text (TL.pack (show lhs))

-- | Pretty-printer for a RHS, possibly with custom elements.
rhs2doc :: (lhs -> Doc) -> (rhs -> Doc) -> Rhs lhs rhs -> Doc
rhs2doc _ customRhs (CustomRhs rhs) = customRhs rhs
rhs2doc _ _ (GenericRhs s ts) = Doc.strictText s <> mconcat (map ((":" <>) . Doc.strictText) ts)
rhs2doc _ _ (StringRhs rhs) = PP.text (TL.pack (show rhs))
rhs2doc _ _ (IntRhs rhs) = PP.text (TL.pack (show rhs))
rhs2doc _ _ (FloatRhs rhs) = Doc.pp_float rhs
rhs2doc customLhs customRhs (CompoundRhs rhs)
    = PP.vsep ["{", PP.indent 4 (script2doc customLhs customRhs rhs), PP.text "}"]
rhs2doc _ _ (DateRhs (Date year month day)) =
    mconcat . map (PP.text . TL.pack) $ [show year, ".", show month, ".", show day]

-- | Display a script with no custom elements in an 80-column format.
displayGenericScript :: GenericScript -> Text
displayGenericScript script = TL.toStrict . PP.displayT . PP.renderPretty 0.8 80 $ genericScript2doc script
