{-# LANGUAGE OverloadedStrings #-}
module Yaml (
        LocEntry (..)
    ,   L10nLang, L10n
    ,   parseLocFile
    ,   mergeLangs
    ) where

import Control.Applicative

import Data.Monoid

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Text (Text)
import qualified Data.Text as T

import Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text as Ap

type L10nLang = HashMap Text LocEntry
type L10n = HashMap Text L10nLang

data LocEntry = LocEntry {
        version :: Int
    ,   content :: Text
    } deriving (Show, Eq, Ord)

-- Order localizations by version number (left-biased).
latest :: LocEntry -> LocEntry -> LocEntry
latest a b = if version a >= version b then a else b

mergeLoc :: L10nLang -> L10nLang -> L10nLang
mergeLoc = HM.unionWith latest

mergeLangs :: L10n -> L10n -> L10n
mergeLangs = HM.unionWith mergeLoc

------------------------
-- Parser combinators --
------------------------

-- Succeed while consuming nothing.
nothing :: Parser ()
nothing = pure ()

-- Consume horizontal space. This may include a comment, in which case parsing
-- will end just before the newline, but the newline will not be consumed.
--
-- This parser cannot fail, so don't use it with repeating combinators such as
-- 'many'.
hspace :: Parser ()
hspace = Ap.takeWhile Ap.isHorizontalSpace
      *> Ap.option mempty comment
      *> nothing
  <?> "horizontal space and/or comment"

-- Consume a comment.
comment :: Parser ()
comment =  Ap.char '#'
        *> Ap.takeWhile (not . Ap.isEndOfLine)
        *> nothing
    <?> "comment"

-- Consume a line break, possibly preceded by whitespace and/or a comment.
newline :: Parser ()
newline = hspace *> Ap.endOfLine *> nothing
    <?> "linebreak, possibly preceded by whitespace and/or a comment"

-- Consume space / comments at the start of the file, if any. When used there,
-- this is guaranteed to finish either at the start of the file or just after a
-- newline.
--
-- This parser cannot fail, so don't use it with repeating combinators such as
-- 'many'.
startspace :: Parser ()
startspace = Ap.option mempty (Ap.string "\xFEFF" <|> Ap.string "\xFFFE") -- BOM
          *> Ap.option mempty (many newline)
          *> nothing
    <?> "space at start of file"

-- Consume space / comments at the start of the file, if any. This is the same
-- as 'startspace' except it need not end after a newline.
endspace :: Parser ()
endspace = Ap.option mempty startspace *> Ap.option mempty hspace *> nothing
    <?> "space at end of file"

-- The syntax of the strings in the localization files is quite strange:
-- - They take up the entire rest of the line
-- - They start with a quotation mark and end with a quotation mark
-- - Unescaped quotation marks are allowed
-- The last means that using 'many' to collect characters in the string is
-- insufficient - it will greedily take the final quotation mark as part of the
-- string. Instead, we have to explicitly drop it from the result /after/
-- consuming it.
--
-- Bug report: https://forum.paradoxplaza.com/forum/index.php?threads/1-17-0-f236-localisation-files-containing-unescaped-quotation-marks.934107/
stringLit :: Parser Text
stringLit = T.init . T.pack <$> (Ap.char '"' *> many stringChar)
    <?> "string literal"

-- Characters within a string. Process backslash escapes.
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

-- Parse an entire localization file.
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

locFile :: Parser L10n
locFile = startspace *> lang <* endspace
    <?> "localization file"

-- Front end.
parseLocFile :: Text -> Either String L10n
parseLocFile contents = Ap.parseOnly locFile contents

-- Parser for one language's localisations.
lang :: Parser L10n
lang = HM.singleton <$> (liftA2 (<>) (Ap.string "l_")
                                     (Ap.takeWhile1 (Ap.inClass "a-z_"))
                            <?> "language tag")
                                         -- Stellaris has l_braz_por
                    <*> (Ap.char ':'
                     *> newline
                     *> messages)
    <?> "localization for one language"

messages :: Parser L10nLang
messages = HM.fromList <$> message `Ap.sepBy` Ap.many1 newline
    <?> "messages"

-- We assume the localisation files have strictly a single space at the start.
message :: Parser (Text, LocEntry)
message = (,) <$> (Ap.string " "
               *> liftA2 T.cons (Ap.satisfy (Ap.inClass "a-zA-Z"))
                                (Ap.takeWhile (Ap.inClass "a-zA-Z._0-9")))
              <*> (LocEntry <$> (Ap.char ':' *> Ap.decimal) -- version
                            <*> (hspace *> stringLit))
    <?> "message"
