module Steam.Core.Database.Types where

data CaseMode
    = CaseInsensitive
    | CaseSensitive
    deriving Show

data MatchMode
    = Exact
    | PartialLeft
    | PartialRight
    | PartialBoth
    deriving Show

data MatchPrefs
    = MatchPrefs
    { mpCaseMode   :: CaseMode
    , mpMatchMode  :: MatchMode
    } deriving Show

