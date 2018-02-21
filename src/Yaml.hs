{-|
Module      : Yaml
Description : Quasi-YAML parser and localization types

Europa Universalis 1.17 and later, plus Hearts of Iron IV and Stellaris, use
a format for localization files that is based on YAML, but (unlike versions
before it) is not true YAML, so cannot be parsed with off-the-shelf YAML
libraries. This module provides a parser for this format. Since I don't know
the true grammar of the format, expect bugs.

$locsyn
-}
module Yaml (
        LocEntry (..)
    ,   L10nLang, L10n
    ,   parseLocFile
    ,   mergeLangs, mergeLangList
--  ,   message
--  ,   locFile
    ) where

import Control.Applicative (Applicative (..), Alternative (..), liftA2)

import Data.List (foldl')
import Data.Monoid (Monoid (..), (<>))

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Text as T

import Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text as Ap

-- | Localization table for a single language.
type L10nLang = HashMap Text LocEntry
-- | Localization tables for all languages.
type L10n = HashMap Text L10nLang

-- | Content of an individual localization entry. Note that the 'Ord' instance
-- orders by version number before content.
data LocEntry = LocEntry {
        version :: Int
    ,   content :: Text
    } deriving (Show, Eq, Ord)

-- | Pick from two localizations based on their version number: highest is
-- best. If they're equal, use the first argument.
latest :: LocEntry -> LocEntry -> LocEntry
latest a b = if version a >= version b then a else b

-- | Merge localizations for two languages (using 'latest' to resolve
-- duplicates).
mergeLoc :: L10nLang -> L10nLang -> L10nLang
mergeLoc = HM.unionWith latest

-- | Merge two localization tables. If two entries exist for the same language
-- and key, prefer the one with more recent version number, or (if the same)
-- the one in the first argument.
mergeLangs :: L10n -> L10n -> L10n
mergeLangs = HM.unionWith mergeLoc

-- TODO: make L10nLang and L10n newtypes, so we can define Monoid instances for
-- them with the above as mappend. Then this will be mconcat:

-- | Merge a list of localization tables, from left to right, using
-- 'mergeLangs'.
mergeLangList :: [L10n] -> L10n
mergeLangList = foldl' mergeLangs HM.empty

------------------------
-- Parser combinators --
------------------------

-- | Succeed while consuming nothing.
nothing :: Parser ()
nothing = pure ()

-- | Consume horizontal space. This may include a comment, in which case
-- parsing will end just before the newline, but the newline will not be
-- consumed.
--
-- This parser cannot fail, so don't use it with repeating combinators such as
-- 'many'.
hspace :: Parser ()
hspace = Ap.takeWhile Ap.isHorizontalSpace
      *> Ap.option mempty comment
      *> nothing
  <?> "horizontal space and/or comment"

-- | Consume a comment. That means a # followed by the rest of the line.
comment :: Parser ()
comment =  Ap.char '#'
        *> Ap.takeWhile (not . Ap.isEndOfLine)
        *> nothing
    <?> "comment"

-- | Consume a line break, possibly preceded by whitespace and/or a comment.
newline :: Parser ()
newline = hspace *> Ap.endOfLine *> nothing
    <?> "linebreak, possibly preceded by whitespace and/or a comment"

-- | Consume space / comments at the start of the file, if any. When used
-- there, this is guaranteed to finish either at the start of the file or just
-- after a newline.
--
-- This parser cannot fail, so don't use it with repeating combinators such as
-- 'many'.
startspace :: Parser ()
startspace = Ap.option mempty (Ap.string "\xFEFF" <|> Ap.string "\xFFFE") -- BOM
          *> Ap.option mempty (many newline)
          *> nothing
    <?> "space at start of file"

-- | Consume space / comments at the start of the file, if any. This is the
-- same as 'startspace' except it need not end after a newline.
endspace :: Parser ()
endspace = Ap.option mempty startspace *> Ap.option mempty hspace *> nothing
    <?> "space at end of file"

-- $locsyn
-- #strangesyn#
-- The syntax of the strings in the localization files is quite strange:
--
-- * They take up the entire rest of the line
-- * They start with a quotation mark and end with a quotation mark
-- * Unescaped quotation marks are allowed
--
-- The last means that using 'many' to collect characters in the string is
-- insufficient - it will greedily take the final quotation mark as part of the
-- string. Instead, we have to explicitly drop it from the result /after/
-- consuming it.
--
-- Bug report: https://forum.paradoxplaza.com/forum/index.php?threads/1-17-0-f236-localisation-files-containing-unescaped-quotation-marks.934107/

-- | Content of a localization entry. This has its origin in YAML strings, but
-- its syntax is actually rather different; see "Yaml#strangesyn" for details.
stringLit :: Parser Text
stringLit = T.init . T.pack <$> (Ap.char '"' *> many stringChar)
    <?> "string literal"

-- | Characters within a string. Process backslash escapes (apostrophes,
-- newlines and tabs).
stringChar :: Parser Char
stringChar = Ap.satisfy (not . \c -> Ap.inClass "\\" c || Ap.isEndOfLine c)
         <|> Ap.char '\\' 
            *> (    Ap.char '\'' 
                <|> (Ap.char 'n' *> pure '\n')
                <|> (Ap.char 't' *> pure '\t')
                <|> Ap.anyChar
               )
    <?> "string character"

-----------------------
-- High level parsers -
-----------------------

-- | Parse an entire localization file.
--
-- We expect each file will contain only one localization, of the form
--
--  l_<lang>:
--   TAG:<version>  "text"
--   ....
--
-- where the l_<lang> tag appears at the start of the line.
--
-- Note that the text between the quotation marks can be anything other than a
-- newline, even unescaped quotation marks. (It can contain newlines, but they
-- are escaped as \n.) We do expect it to end at the end of the line.
--
-- Blank lines and comments (beginning with @#@) are permitted.
locFile :: Parser L10n
locFile = startspace *> lang <* endspace
    <?> "localization file"

-- | Parse a localization file. If the parser fails, returns
-- @Left <the parse error>@.
parseLocFile :: Text -> Either String L10n
parseLocFile contents = Ap.parseOnly locFile contents

-- | Parser for one language's localisations.
lang :: Parser L10n
lang = HM.singleton <$> (liftA2 (<>) (Ap.string "l_")
                                     (Ap.takeWhile1 (Ap.inClass "a-z_"))
                            <?> "language tag")
                                         -- Stellaris has l_braz_por
                    <*> (Ap.char ':'
                     *> Ap.many1 newline
                     *> messages)
    <?> "localization for one language"

-- | Parse localization messages, separated by line breaks, blank lines, and/or
-- comments.
messages :: Parser L10nLang
messages = HM.fromList <$> message `Ap.sepBy` Ap.many1 newline
    <?> "messages"

-- | Parse a single message. We assume the localisation files have strictly a
-- single space at the start.
message :: Parser (Text, LocEntry)
message = (,) <$> (Ap.string " "
               *> liftA2 T.cons (Ap.satisfy (Ap.inClass "a-zA-Z-"))
                                (Ap.takeWhile (Ap.inClass "a-zA-Z._0-9-")))
              <*> (LocEntry <$> (Ap.char ':' *> Ap.decimal) -- version
                            <*> (hspace *> stringLit))
    <?> "message"
