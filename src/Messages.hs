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

messageText :: Monad m => ScriptMessage -> PPT extra m Text
messageText msg = do
    langs <- getLangs
    return $ renderMessage Script langs msg

message :: Monad m => ScriptMessage -> PPT extra m Doc
message msg = strictText <$> messageText msg

imsg2doc :: Monad m => IndentedMessages -> PPT extra m Doc
imsg2doc msgs = vsep <$>
                mapM (\(i,rm) -> do
                        m <- message rm
                        return (hsep [strictText (T.replicate i "*"),  m]))
                     msgs
