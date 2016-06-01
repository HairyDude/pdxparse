{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS_GHC -O0 -fno-warn-type-defaults #-} -- Compiling with optimizations on takes far too long
module Messages (
        ScriptMessage (..)
    ,   template, templateDoc
    ,   message, messageText
    ,   imsg2doc, imsg2doc_html
    ,   IndentedMessage, IndentedMessages
    ,   module Doc
    ) where

import Data.Monoid
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

messageText :: Monad m => ScriptMessage -> PPT m Text
messageText msg = do
    mlangs <- getLangs
    return $ renderMessage Script mlangs msg

message :: Monad m => ScriptMessage -> PPT m Doc
message msg = strictText <$> messageText msg

imsg2doc :: Monad m => IndentedMessages -> PPT m Doc
imsg2doc msgs = vsep <$>
                mapM (\(i,rm) -> do
                        m <- message rm
                        return (hsep [strictText (T.replicate i "*"),  m]))
                     msgs

-- Use HTML to format the messages instead of wiki markup. This behaves better
-- with <pre> blocks but doesn't play well with idea groups.
imsg2doc_html :: forall m. Monad m => IndentedMessages -> PPT m Doc
imsg2doc_html [] = return mempty
imsg2doc_html msgs@((i,_):_)
    | i > 0     = enclose "<ul>" "</ul>" . fst <$> imsg2doc' msgs
    | otherwise = fst <$> imsg2doc' msgs
    where
        -- Format all (remaining) messages at the current indent level.
        imsg2doc' :: IndentedMessages -> PPT m (Doc, IndentedMessages)
        imsg2doc' [] = return (mempty, [])
        imsg2doc' [(_, rm)] = do -- Last message.
            m <- message rm
            return (enclose "<li>" "</li>" m, [])
        imsg2doc' ((i, rm):msgs@((i',_):_))
            | i < i' = do
                -- New indent.
                m <- message rm
                -- Format the indented stuff.
                (indented, moremsgs) <- imsg2doc' msgs
                -- Format stuff after the indent.
                (postdoc, restmsgs) <- imsg2doc' moremsgs
                -- Put it all together.
                return (vsep
                            [enclose "<li>" "</li>"
                                (vsep
                                    [m
                                    ,enclose "<ul>" "</ul>" indented])
                            ,postdoc]
                       , restmsgs)
            | i > i' = do
                -- Last message at this level.
                m <- enclose "<li>" "</li>" <$> message rm
                return (m, msgs)
            | otherwise = do
                -- Carry on with this indent level.
                m <- enclose "<li>" "</li>" <$> message rm
                (postdoc, restmsgs) <- imsg2doc' msgs
                return (m <> line <> postdoc, restmsgs)

