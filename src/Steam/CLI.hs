module Steam.CLI
    ( ProgramArgs(..)
    , ProgramAction(..)
    , MatchAction(..)
    , OutputAction(..)
    --, module Steam.Core.Database.Types
    , parseProgArgs
    ) where

import Data.Monoid
import Options.Applicative

import Steam.Core.Database.Types
import Steam.Types

data ProgramArgs
    = ProgramArgs
    { paConfigOverride :: Maybe FilePath
    , paDbOverride     :: Maybe FilePath
    , paProgramAction  :: ProgramAction
    } deriving Show

data MatchAction
    = Blacklist
    | OutputAction
    { oaOutputAction :: OutputAction
    , oaOutputPrefs  :: OutputPrefs
    }
    deriving Show

-- | Actions that output a list of games.
data OutputAction
    = Query
    | Match
    deriving Show

data ProgramAction
    = Update
    | DumpBlacklist
    | MatchAction
    { maPrefs      :: MatchPrefs
    , maTrimSpaces :: Bool
    , maCutSuffix  :: Maybe String
    , maAct        :: MatchAction
    } deriving Show

parseProgArgs :: IO ProgramArgs
parseProgArgs = customExecParser (prefs $ showHelpOnEmpty <> showHelpOnError) progArgsPI

progArgsPI :: ParserInfo ProgramArgs
progArgsPI = info (progArgsP <**> helper) (progDesc desc)
  where
    desc = "Manage a local steam database and filter a list of games you don't \
           \own with your criteria."

progArgsP :: Parser ProgramArgs
progArgsP = ProgramArgs <$> optional cfgP <*> optional dbP <*> progActP
  where
    cfgP = strOption $ long "config-file"
                       <> short 'c'
                       <> metavar "CONFIG_PATH"
                       <> help "Override default configuration file"
    dbP  = strOption $ long "database-file"
                       <> short 'd'
                       <> metavar "DATABASE_PATH"
                       <> help "Override default database file"

progActP :: Parser ProgramAction
progActP = hsubparser $
    foldr (\(n, p, d) r -> command n (info p (progDesc d)) <> r) (metavar "COMMAND")
    [ ("update"            , pure Update                       , "Update applications and owned games")
    , ("dump-blacklist"    , pure DumpBlacklist                , "Dump the blacklist")
    , ("blacklist"         , matchActionP $ pure Blacklist     , blacklistDesc)
    , ("match"             , matchActionP $ outputActionP Match, "Find matches in a list of games")
    , ("query"             , matchActionP $ outputActionP Query, "Query for appid of a game")
    ]
  where
    blacklistDesc = "Hide games that should not appear in future matching"

matchActionP :: Parser MatchAction -> Parser ProgramAction
matchActionP p =
    flip <$> (MatchAction <$> matchPrefsP <*> trimSpacesP) <*> p <*> optional cutSuffixP
  where
    cutSuffixP  = strArgument (help "The character sequence to cut after" <> metavar "CUT_SUFFIX")
    trimSpacesP = not <$> switch (short 'k' <> long "keep-spaces" <> help "Do not trim spaces of input lines")

outputActionP :: OutputAction -> Parser MatchAction
outputActionP oa = OutputAction oa <$> (OutputPrefs <$> unmatchedActionP <*> outputModeP)
  where
    unmatchedActionP = flag' Display (long "display-unmatched" <> help "Display unmatched input in the result")
                       <|> flag' DisplaySeperate (long "seperate-unmatched" <> help "Display unmatched input seperate in the result")
                       <|> flag' Hide (long "hide-unmatched" <> help "Hide unmatched input in the result")
                       -- TODO take default as argument
                       <|> pure Hide
    outputModeP = flag' Plain (long "output-plain" <> help "Output result with plain game names")
                  <|> flag' Markdown (long "output-markdown" <> help "Output result with markdown links to the shop page")
                  <|> flag' Tabular (long "output-tabular" <> help "Output result with a table")
                  <|> pure Tabular

matchPrefsP :: Parser MatchPrefs
matchPrefsP = MatchPrefs <$> singleMatchP <*> caseP <*> matchModeP
  where
    singleMatchP = not <$> switch (short 'm' <> long "multiple" <> help "Allow matching multiple games")
    caseP = flag CaseInsensitive CaseSensitive (short 'S' <> long "case-sensitive")
    matchModeP = flag' Exact (short 'e' <> long "exact-match" <> help "Allow only exact matches")
                 <|> flag' PartialLeft (short 'l' <> long "partial-left-match" <> help "Match input games that are contained in game names")
                 <|> flag' PartialRight (short 'r' <> long "partial-right-match" <> help "Match game names that are contained in input games")
                 <|> flag' PartialBoth (short 'b' <> long "partial-both" <> help "Allow partial matches on both sides")
                 <|> flag' Wildcard (short 'w' <> long "wildcard-match" <> help "Treat '%' and '?' as wildcards")
                 <|> pure Exact

