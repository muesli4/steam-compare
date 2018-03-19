module Steam.Types where

data OutputPrefs
    = OutputPrefs
    { opUnmatchedAction :: UnmatchedAction
    , opOutputMode      :: OutputMode
    } deriving Show

-- | Determines what happens with unmatched input names.
data UnmatchedAction
    = Display
    | DisplaySeperate
    | Hide
    deriving Show

-- | Determines how a list of games is displayed.
data OutputMode
    = Plain
    | Markdown
    | Tabular
    deriving Show
