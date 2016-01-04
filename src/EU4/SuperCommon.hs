-- Things that should be in Common.hs but need to be used by IdeaGroups.hs-boot.
module EU4.SuperCommon where

data MonarchPower = Administrative
                  | Diplomatic
                  | Military
    deriving (Show, Eq, Ord)
