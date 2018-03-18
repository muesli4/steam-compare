module Steam.CLI
    ( ProgramArgs(..)
    , ProgramAction(..)
    , MatchAction(..)
    --, module Steam.Core.Database.Types
    , parseProgArgs
    ) where

import Data.Monoid
import Options.Applicative

import Steam.Core.Database.Types

data ProgramArgs
    = ProgramArgs
    { paConfigOverride :: Maybe FilePath
    , paDbOverride     :: Maybe FilePath
    , paProgramAction  :: ProgramAction
    } deriving Show

data MatchAction
    = QueryAppID String
    | Blacklist
    | Match 
    | ReplaceWithLinks
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
    [ ("update"            , pure Update                  , "Update applications and owned games")
    , ("dump-blacklist"    , pure DumpBlacklist           , "Dump the blacklist")
    , ("appid"             , matchActionP queryAppIDP     , "Query for appid of a game")
    , ("blacklist"         , matchActionP $ pure Blacklist, blacklistDesc)
    , ("match"             , matchActionP $ pure Match    , "Find matches in a list of games")
    , ("replace-with-links", matchActionP $ pure ReplaceWithLinks , "Replace game names with corresponding Steam store links if found")
    ]
  where
    blacklistDesc = "Hide games that should not appear in future matching"

matchActionP :: Parser MatchAction -> Parser ProgramAction
matchActionP p =
    flip <$> (MatchAction <$> matchPrefsP <*> trimSpacesP) <*> p <*> optional cutSuffixP
  where
    cutSuffixP  = strArgument (help "The character sequence to cut after" <> metavar "CUT_SUFFIX")
    trimSpacesP = not <$> switch (short 'k' <> long "keep-spaces" <> help "Do not trim spaces of input lines")

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

queryAppIDP :: Parser MatchAction
queryAppIDP = QueryAppID <$> strArgument (help "The name of a game" <> metavar "GAME")

