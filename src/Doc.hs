{-# LANGUAGE OverloadedStrings #-}
module Doc (
        strictText
    ,   pp_string
    ,   doc2text
    ,   pp_signed
    ,   pp_float
    ,   pp_float_t
    ,   nl2br
    ,   module PP
    ) where

import Data.List
import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Text.PrettyPrint.Leijen.Text as PP hiding ((<>), (<$>), (</>))

import Numeric (showFFloat)

strictText :: Text -> Doc
strictText = PP.text . TL.fromStrict

pp_string :: String -> Doc
pp_string = PP.text . TL.pack

doc2text :: Doc -> Text
doc2text = TL.toStrict . PP.displayT . PP.renderCompact

-- Pretty-print a number, putting a + sign in front if it's positive.
-- Assumes the passed-in formatting function does add a minus sign.
pp_signed :: (Ord n, Num n) => (n -> Doc) -> n -> Doc
pp_signed pp_num n = (if signum n > 0 then "+" else mempty) <> pp_num n

-- Pretty-print a Double. If it's a whole number, display it without a decimal.
pp_float :: Double -> Doc
pp_float n =
    let trunc = floor n
    in if fromIntegral trunc == n
        then PP.int (fromIntegral trunc)
        else PP.text . TL.pack $ showFFloat Nothing n ""

-- Pretty-print a Double, as Text.
pp_float_t :: Double -> Text
pp_float_t = TL.toStrict . PP.displayT . PP.renderCompact . pp_float

-- Convert newlines to <br/> tags.
nl2br :: Text -> Text
nl2br = mconcat . unfoldr replaceNextBreak . Just where
    replaceNextBreak :: Maybe Text -> Maybe (Text, Maybe Text)
    replaceNextBreak Nothing = Nothing
    replaceNextBreak (Just t)
        = let (left, right) = T.breakOn "\n" t
              right' = T.drop 1 right
          in if T.null right -- no newlines found
                then Just (left, Nothing)
                else Just (left <> "<br/>", Just right')
