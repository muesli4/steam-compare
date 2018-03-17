{-# LANGUAGE RecordWildCards #-}
import           Control.Applicative
import           Data.List.Split
import           Data.Maybe
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           System.Environment
import           Text.Config.Ini
import           System.Exit

import           Steam.Core.Types
import           Steam.Action
import           Steam.CLI
import           Steam.Path

-- TODO progress bar while fetching ?

main :: IO ()
main = do
    ProgramArgs {..} <- parseProgArgs

    optConfigPath <- pickConfigFile paConfigOverride progName configName

    case optConfigPath of
        Left configPath  -> die $ "missing configuration file: " ++ configPath
        Right configPath -> do
            optConfig <- readConfigFile configPath
            case optConfig of
                Left e               -> die $ "invalid configuration file format: " ++ e
                Right (Config kvs _) ->
                    case lookupSteamID kvs of
                        Nothing  -> die "no steam id given: specify at least 'steamid' or 'steamid64'"
                        Just sid -> do
                            optDbPath <- pickDataFile paDbOverride progName dbName
                            prog (ProgramInfo sid optDbPath) paProgramAction
  where
    lookupSteamID kvs = SteamID64 <$> lookup "steamid64" kvs <|> SteamID <$> lookup "steamid" kvs

    configName = "default-user.conf"
    dbName     = "default-user.db"
    progName   = "steam-compare"

prog :: ProgramInfo -> ProgramAction -> IO ()
prog pi@(ProgramInfo sid _) pa = do
    c <- connectSqlite3 dbPath
    runRaw c "PRAGMA case_sensitive_like=ON;"

    dbInitAction c

    case pa of
        Update           -> progUpdate c sid
        DumpBlacklist    -> progDumpBlacklist c
        MatchAction {..} -> case maAct of
            QueryAppID game  -> progQueryAppID c maPrefs (inputMod game)
            Blacklist        -> progBlacklist c maPrefs inputMod
            Match            -> progMatch c maPrefs inputMod
            ReplaceWithLinks -> progReplaceWithLinks c maPrefs inputMod
          where
            inputMod = maybe id (\d -> head . splitOn d) maCutSuffix
    disconnect c
  where
    (dbInitAction, dbPath) = progDBInfo pi
