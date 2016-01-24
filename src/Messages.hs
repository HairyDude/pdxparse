{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -O0 #-} -- Compiling with optimizations on takes far too long
module Messages (
        ScriptMessage (..)
    ,   template, templateDoc
    ,   message, messageText
    ,   imsg2doc
    ,   IndentedMessage, IndentedMessages
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

type IndentedMessage = (Int, ScriptMessage)
type IndentedMessages = [IndentedMessage]

messageText :: ScriptMessage -> PP extra Text
messageText msg = do
    langs <- getLangs
    return $ renderMessage Script langs msg

message :: ScriptMessage -> PP extra Doc
message msg = strictText <$> messageText msg

imsg2doc :: IndentedMessages -> PP extra Doc
imsg2doc msgs = vsep <$>
                mapM (\(i,rm) -> do
                        m <- message rm
                        return (hsep [strictText (T.replicate i "*"),  m]))
                     msgs
