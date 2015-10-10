{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}
module Abstract where

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

import Control.Applicative
import qualified Data.Foldable as F
import Data.Monoid

import Data.Char

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text as Ap

import Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>))
import qualified Text.PrettyPrint.Leijen.Text as PP

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
    | CompoundRhs [Statement lhs rhs]
    deriving (Eq, Ord, Show, Read)
type GenericRhs = Rhs () ()

type Script lhs rhs = [Statement lhs rhs]
type GenericScript = [GenericStatement]

-- A very common type of statement
s_yes :: Text -> Statement lhs rhs
s_yes tok = Statement (GenericLhs tok) (GenericRhs "yes")

------------
-- Parser --
------------

ident :: Parser Text
ident = (<>) <$> (T.singleton <$> (Ap.satisfy (\c -> c == '_' || isAlpha c)))
             <*> Ap.takeWhile (\c -> c == '_' || isAlphaNum c)
    <?> "identifier"

-- A string literal.
stringLit :: Parser Text
stringLit = "\""
         *> (T.pack <$> Ap.many' stringChar)
         <* "\""
    <?> "string literal"

-- An integer literal.
intLit :: Parser Int
intLit = Ap.decimal

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
                <*  (Ap.skipSpace >> Ap.char '=' >> Ap.skipSpace)
                <*> rhs customLhs customRhs
    <?> "statement"

script :: Parser lhs -> Parser rhs -> Parser (Script lhs rhs)
script customLhs customRhs = Ap.sepBy (statement customLhs customRhs) Ap.skipSpace

-- | Statement with no custom elements.
-- Use this as a starting point for scripts that use standard syntax.
genericStatement :: Parser GenericStatement
genericStatement = statement parse_generic parse_generic
    where parse_generic = fail "generic"

genericScript :: Parser GenericScript
genericScript = script parse_generic parse_generic
    where parse_generic = fail "generic"

lhs :: Parser lhs -> Parser (Lhs lhs)
lhs custom = CustomLhs <$> custom
         <|> GenericLhs <$> ident
    <?> "statement LHS"

rhs :: Parser lhs -> Parser rhs -> Parser (Rhs lhs rhs)
rhs customLhs customRhs
          = CustomRhs   <$> customRhs
        <|> GenericRhs  <$> ident
        <|> StringRhs   <$> stringLit
        <|> IntRhs      <$> intLit
        <|> CompoundRhs <$> ("{"
                         *> Ap.many1 (Ap.skipSpace *> statement customLhs customRhs) <* Ap.skipSpace
                         <* "}")
    <?> "statement RHS"

--------------------
-- Pretty-printer --
--------------------

-- Pretty-printer for a script with no custom elements.
genericScript2doc :: GenericScript -> Doc
genericScript2doc = F.foldMap genericStatement2doc

genericStatement2doc :: GenericStatement -> Doc
genericStatement2doc = statement2doc (const "") (const "")

script2doc :: (lhs -> Doc) -> (rhs -> Doc) -> [Statement lhs rhs] -> Doc
script2doc customLhs customRhs = foldr (PP.<$>) PP.empty . map (statement2doc customLhs customRhs)

statement2doc :: (lhs -> Doc) -> (rhs -> Doc) -> Statement lhs rhs -> Doc
statement2doc customLhs customRhs (StatementBare lhs)
    = lhs2doc customLhs lhs
statement2doc customLhs customRhs (Statement lhs rhs)
    = lhs2doc customLhs lhs <++> text "=" <++> rhs2doc customLhs customRhs rhs

lhs2doc :: (lhs -> Doc) -> Lhs lhs -> Doc
lhs2doc customLhs (CustomLhs lhs) = customLhs lhs
lhs2doc _         (GenericLhs lhs) = text (LT.fromStrict lhs)

rhs2doc :: (lhs -> Doc) -> (rhs -> Doc) -> Rhs lhs rhs -> Doc
rhs2doc _ customRhs (CustomRhs rhs) = customRhs rhs
rhs2doc _ _ (GenericRhs rhs) = text (LT.fromStrict rhs)
rhs2doc _ _ (StringRhs rhs) = text (LT.pack (show rhs)) -- lazy rendering...
rhs2doc _ _ (IntRhs rhs) = text (LT.pack (show rhs)) -- lazy rendering...
rhs2doc customLhs customRhs (CompoundRhs rhs)
    = text "{" PP.<$$> indent 4 (script2doc customLhs customRhs rhs) PP.<$$> text "}"
