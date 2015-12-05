{-# LANGUAGE OverloadedStrings #-}
module Doc (
        strictText
    ,   doc2text
    ,   pp_signed
    ,   pp_float
    ,   pp_float_t
    ) where

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Text.PrettyPrint.Leijen.Text (Doc)
import qualified Text.PrettyPrint.Leijen.Text as PP

import Numeric (showFFloat)

strictText :: Text -> Doc
strictText = PP.text . TL.fromStrict

doc2text :: Doc -> Text
doc2text = TL.toStrict . PP.displayT . PP.renderCompact

-- Pretty-print a number, putting a + sign in front if it's not negative.
-- Assumes the passed-in formatting function does add a minus sign.
pp_signed :: (Ord n, Num n) => (n -> Doc) -> n -> Doc
pp_signed pp_num n = (if signum n >= 0 then "+" else mempty) <> pp_num n

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
