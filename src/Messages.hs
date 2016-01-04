{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}
module Messages (
        ScriptMessage (..)
    ,   template, templateDoc
    ,   message, messageText
    ,   imsg2doc
    ,   module Doc
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Doc
import MessageTools
import SettingsTypes

----------------
---- Messages --
----------------

-- dummy type required by the Shakespeare machinery
data Script = Script

-- The following splice generates the ScriptMessage type and a RenderMessage
-- instance for it.
mkMessage "Script" "l10n" "en"

messageText :: ScriptMessage -> PP extra Text
messageText msg = do
    langs <- getLangs
    return $ renderMessage Script langs msg

message :: ScriptMessage -> PP extra Doc
message msg = strictText <$> messageText msg

imsg2doc :: [(Int, ScriptMessage)] -> PP extra Doc
imsg2doc msgs = vsep <$>
                mapM (\(i,rm) -> do
                        m <- message rm
                        return (hsep [strictText (T.replicate i "*"),  m]))
                     msgs
