{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}
module Templates (
        StatementHandler
    ,   Param (..)
    ,   CompField (..)
    ,   foldCompound
    ) where

import Language.Haskell.TH -- everything

import Data.Text (Text)

import Data.List (unzip5)
import Data.Maybe (isJust)

import Abstract
import Messages
import QQ
import SettingsTypes

import Debug.Trace

data CompField = CompField {
        fieldName :: String
    ,   fieldType :: Q Type
    ,   fieldDefault :: Maybe (Q Exp)
    ,   fieldCompulsory :: Bool
    }


class Param t where
    toParam :: GenericRhs -> Maybe t
instance Param Double where
    toParam = floatRhs
instance Param Text where
    toParam = textRhs

-- | Convenience synonym.
type StatementHandler g m = GenericStatement -> PPT g m IndentedMessages

foldCompound :: String -> String -> String -> [(String, Q Type)] -> [CompField] -> Q Exp -> Q [Dec]
foldCompound funname s_tyname prefix extraArgs fieldspecs eval = do
    let -- Missing TH library function
        funT :: TypeQ -> TypeQ -> TypeQ
        funT t1 t2 = [t| $t1 -> $t2 |] 
        -- Variable names
        name_acc = mkName "acc"
        name_addLine = mkName "addLine"
        name_fun = mkName funname
        name_pp = mkName "pp"
        name_scr = mkName "scr"
        name_stmt = mkName "stmt"
        name_x = mkName "x"
        tyname = mkName s_tyname
        defaultsName = mkName ("new" ++ s_tyname)
    -- Variables. These are evaluated and immediately put back in Q to make
    -- sure every instance is the same.
    [tvar_g, tvar_m] <- mapM (\n -> return . varT =<< newName n) ["g","m"]
    [var_acc, var_addLine, var_defaults, var_pp, var_scr, var_stmt, var_x]
        <- mapM (return . varE) [name_acc, name_addLine, defaultsName
                                ,name_pp, name_scr, name_stmt, name_x]

    -- Iterate through fields generating relevant code for each
    let (recFields, defs, lineclauses, caseheads, casebodies) =
            unzip5 . flip map fieldspecs $ \fieldspec ->
                let ftype = fieldType fieldspec
                    fname = fieldName fieldspec
                    varname = '_' : fieldName fieldspec
                    rfname = mkName (prefix ++ varname)
                    def = fieldDefault fieldspec
                    hasDefault = isJust def
                    patvar = varP (mkName varname)
                in ( -- Record field declaration
                    do  thetype <- ftype
                        thedefault <- case fieldDefault fieldspec of
                            Nothing -> return Nothing
                            Just def -> return . Just =<< def
                        varBangType rfname
                            (bangType (bang noSourceUnpackedness noSourceStrictness)
                                (if hasDefault
                                    then ftype -- guaranteed to exist
                                    else appT (conT ''Maybe) ftype
                                )
                            )
                    -- Initial value for accumulator
                    ,maybe [| Nothing |] id def
                    -- Clause for addLine
                    ,clause [varP name_acc
                            ,[p| Statement (GenericLhs $(litP (stringL (fieldName fieldspec))) Nothing)
                                          OpEq
                                          $(viewP (varE 'toParam) (conP 'Just [varP name_x])) |]]
                            (normalB $ recUpdE (varE name_acc) [fieldExp rfname
                                (if hasDefault
                                    then varE name_x
                                    else appE (conE 'Just) (varE name_x))])
                            []
                    -- Case head for this field
                    ,appE (varE rfname) var_acc
                    -- Case body, exposing a variable for this field
                    ,if fieldCompulsory fieldspec
                        then [p| Just $patvar |] -- :: <fieldtype>
                        else [p| $patvar |]) -- :: Maybe <fieldtype>
    sequence $ [
        -- Accumulator type
        -- data <AccType> = AccType
        --  { <prefix>_<field1Name> :: {Maybe} <field1Type>
        --  , ...
        --  }
            dataD (cxt []) tyname [] Nothing
                  [recC tyname recFields
                  ]
                  (cxt [])
        -- Initial accumulator
        -- new<AccType> :: <AccType>
        -- new<AccType> = AccType <default1> <default2> ...
        ,   sigD defaultsName (conT tyname)
        ,   flip (valD (varP defaultsName)) [] . normalB $
                appsE (conE tyname : defs)
        -- Unpacking code
        -- <funName> :: (IsGameState (GameState g), Monad m) => <ExtraArg1Type> -> ... -> StatementHandler g m
        -- <funName> <extraArg1> ... stmt@[pdx| %_ = @scr |]
        --      = msgToPP . pp $ foldl' addLine new<AccType> scr where
        --          addLine :: <AccType> -> GenericStatement -> <AccType>
        --          addLine [pdx| <tag1> = $(toParam -> Just _<paramName> |] = acc { prefix_<paramName> = {Just} _<paramName> }
        --          ...
        --          pp :: <AccType> -> ScriptMessage
        --          pp acc = case (requiredPat1, ..., optionalPat1, ...) of
        --              (Just _<requiredParamName1>, ..., _<optionalParamName1>, ...) -> <eval>
        --              _ -> preMessage stmt
        -- <funName> _1 ... stmt = preStatement stmt
        ,   sigD name_fun
                (forallT [] (sequence [[t|IsGameState (GameState $tvar_g)|], [t|Monad $tvar_m |]]) $
                    foldr funT [t| StatementHandler $tvar_g $tvar_m |] (map snd extraArgs))
        ,   funD name_fun [
                    clause (map (varP . mkName . fst) extraArgs
                                ++ [asP name_stmt [p| [pdx| %_ = @scr |] |]])
                           (normalB [| msgToPP . $var_pp $ foldl' $var_addLine $var_defaults $var_scr |])
                           -- where
                           [sigD name_addLine [t| $(conT tyname) -> GenericStatement -> $(conT tyname) |]
                           ,funD name_addLine (lineclauses ++
                                [clause [varP name_acc, wildP]
                                        (normalB $ [| $var_acc |])
                                        []
                                ])
                           ,funD name_pp
                                [flip (clause [varP name_acc]) [] . normalB $
                                    caseE (tupE caseheads)
                                        [match (tupP casebodies) (normalB eval) []
                                        ,match wildP (normalB
                                            [| trace (funname ++ ": one or more required fields not present") $
                                                preMessage stmt |]) []
                                        ]
                                ]
                           ]
                ,   clause (map (const wildP) extraArgs ++ [varP name_stmt])
                           (normalB [| preStatement stmt |])
                           []
            ]
        ]
