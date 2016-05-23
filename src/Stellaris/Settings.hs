module Stellaris.Settings {-(
    )-} where

-- Placeholder - Stellaris scopes are different from EU4
data StScope
    = StCountry
    | StProvince
    | StTradeNode
    | StGeographic -- area, etc.
    | StBonus
    deriving (Show, Eq, Ord, Enum, Bounded)

-- State
data Stellaris = Stellaris
    { scopeStack :: [StScope]
    } deriving (Show)
