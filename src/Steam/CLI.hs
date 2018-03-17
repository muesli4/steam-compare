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
    { paOptConfigPath :: Maybe String
    , paProgramAction :: ProgramAction
    } deriving Show

data MatchAction
    = QueryAppID String
    | Blacklist
    | Match 
    deriving Show

data ProgramAction
    = Update
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
progArgsP = ProgramArgs <$> optional cfgP <*> progActP
  where
    cfgP    = strOption $ long "config"
                          <> short 'c'
                          <> metavar "CONFIG_PATH"
                          <> help cfgDesc
    cfgDesc = "Override default configuration file"

progActP :: Parser ProgramAction
progActP = hsubparser $
    foldr (\(n, p, d) r -> command n (info p (progDesc d)) <> r) (metavar "COMMAND")
    [ ("update"   , pure Update                  , "Update applications and owned games")
    , ("appid"    , matchActionP queryAppIDP     , "Query for appid of a game")
    , ("blacklist", matchActionP $ pure Blacklist, blacklistDesc)
    , ("match"    , matchActionP $ pure Match    , "Find matches in a list of games")
    ]
  where
    blacklistDesc = "Hide games that should not appear in future matching."

matchActionP :: Parser MatchAction -> Parser ProgramAction
matchActionP p =
    flip <$> (MatchAction <$> matchPrefsP <*> trimSpacesP) <*> p <*> optional cutSuffixP
  where
    cutSuffixP  = strArgument (help "The character sequence to cut after" <> metavar "CUT_SUFFIX")
    trimSpacesP = switch (short 't' <> long "trim-spaces" <> help "Trim spaces of input lines")

matchPrefsP :: Parser MatchPrefs
matchPrefsP = MatchPrefs <$> caseP <*> matchModeP
  where
    caseP = flag CaseInsensitive CaseSensitive (short 'S' <> long "case-sensitive")
    matchModeP = flag' Exact (short 'e' <> long "exact-match" <> help "Allow only exact matches")
                 <|> flag' PartialLeft (short 'l' <> long "partial-left-match" <> help "Match input games that are contained in game names")
                 <|> flag' PartialRight (short 'r' <> long "partial-right-match" <> help "Match game names that are contained in input games")
                 <|> flag' PartialBoth (short 'b' <> long "partial-right-match" <> help "Allow partial matches on both sides")
                 <|> flag' Regex (short 'x' <> long "regex-match" <> help "Treat input as regular expressions using '%' and '?'")
                 <|> pure Exact

queryAppIDP :: Parser MatchAction
queryAppIDP = QueryAppID <$> strArgument (help "The name of a game" <> metavar "GAME")

