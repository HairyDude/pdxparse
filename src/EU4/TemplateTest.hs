module EU4.TemplateTest where

import Data.List (foldl')
import Data.Text (Text)

import Abstract
import Messages
import SettingsTypes
import Templates

-- stuff that will be available in situ
msgToPP :: (IsGameState (GameState g), Monad m) => ScriptMessage -> PPT g m IndentedMessages
msgToPP = undefined
preStatement :: (IsGameState (GameState g), Monad m) => GenericStatement -> PPT g m IndentedMessages
preStatement = undefined
iconText :: Text -> Text
iconText = undefined
preMessage :: GenericStatement -> ScriptMessage
preMessage = undefined

data UnitType
    = UnitInfantry
    | UnitCavalry
    | UnitArtillery
    | UnitHeavyShip
    | UnitLightShip
    | UnitGalley
    | UnitTransport
    deriving (Show)

instance Param UnitType where
    toParam (textRhs -> Just "heavy_ship") = Just UnitHeavyShip
    toParam (textRhs -> Just "light_ship") = Just UnitLightShip
    toParam (textRhs -> Just "galley")     = Just UnitGalley
    toParam (textRhs -> Just "transport")  = Just UnitTransport
    toParam _ = Nothing

--foldCompound :: String -> String -> String -> [(String, Q Type)] -> [CompField] -> Q Exp -> Q [Dec]
$(foldCompound "addUnitConstruction" "UnitConstruction" "uc"
    [("extraArg", [t|Text|])]
    [CompField "amount" [t|Double|] Nothing True
    ,CompField "type" [t|UnitType|] Nothing True
    ,CompField "speed" [t|Double|] (Just [|1|]) False
    ,CompField "cost" [t|Double|] (Just [|1|]) False
    ,CompField "optionalArg" [t|Double|] Nothing False]
    [| (case _type of
            UnitHeavyShip -> MsgBuildHeavyShips (iconText "heavy ship")
            UnitLightShip -> MsgBuildLightShips (iconText "light ship")
            UnitGalley    -> MsgBuildGalleys    (iconText "galley")
            UnitTransport -> MsgBuildTransports (iconText "transport")
       ) _amount _speed _cost
    |])
