module Steam.CLI
    ( ProgramArgs(..)
    , ProgramAction(..)
    , parseProgArgs
    ) where

import Data.Monoid
import Options.Applicative

data ProgramArgs
    = ProgramArgs
    { paOptConfigPath :: Maybe String
    , paProgramAction :: ProgramAction
    } deriving Show

data ProgramAction
    = Update
    | QueryAppID String
    | Blacklist
    | Match (Maybe String)
    deriving Show

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
    cfgP = strOption (long "config" <> short 'c' <> metavar "CONFIG_PATH")

progActP :: Parser ProgramAction
progActP = hsubparser $
    metavar "COMMAND"
    <> command "update" (info (pure Update) (progDesc "Update databases."))
    <> command "appid" (info queryAppIDP (progDesc "Query for the appid of a game."))
    <> command "blacklist" (info (pure Blacklist) (progDesc blacklistDesc))
    <> command "match" (info matchP (progDesc "Find matches in a list of games."))
  where
    blacklistDesc = "Hide games that should not appear in future matching."

queryAppIDP :: Parser ProgramAction
queryAppIDP = QueryAppID <$> strArgument (help "The name of a game" <> metavar "GAME")

matchP :: Parser ProgramAction
matchP = Match <$> optional delimP
  where
    delimP = strArgument (help "The character sequence to cut after." <> metavar "DELIM")
