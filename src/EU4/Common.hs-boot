module EU4.Common (
        pp_script
    ,   ppMany
    ,   ppOne
    ) where

import Abstract (GenericScript)
import Doc (Doc)
import Messages (IndentedMessages, StatementHandler)
import SettingsTypes (PPT)
import EU4.Types (EU4Info)

pp_script :: (EU4Info g, Monad m) => GenericScript -> PPT g m Doc
ppMany :: (EU4Info g, Monad m) => GenericScript -> PPT g m IndentedMessages
ppOne :: (EU4Info g, Monad m) => StatementHandler g m
