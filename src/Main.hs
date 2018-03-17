{-# LANGUAGE RecordWildCards #-}
import           Control.Applicative
import           Control.Monad.Loops                   (firstM)
import           Data.List.Split
import           Data.Maybe
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           System.Directory
import           System.Environment
import           System.Environment.XDG.BaseDir
import           Text.Config.Ini
import           System.Exit

import           Steam.Core.Types
import           Steam.Action
import           Steam.CLI

-- TODO progress bar while fetching ?

firstExistingFile :: [FilePath] -> IO (Maybe FilePath)
firstExistingFile = firstM doesFileExist

main :: IO ()
main = do
    ProgramArgs optCfgOverride pa <- parseProgArgs

    optConfigPath <- getConfigFile optCfgOverride

    case optConfigPath of
        Left configPath  -> die $ "missing configuration file: " ++ configPath
        Right configPath -> do
            optConfig <- readConfigFile configPath
            case optConfig of
                Left e               -> die $ "invalid configuration file format: " ++ e
                Right (Config kvs _) ->
                    case SteamID64 <$> lookup "steamid64" kvs <|> SteamID <$> lookup "steamid" kvs of
                        Nothing  -> die "no steam id given: specify at least 'steamid' or 'steamid64'"
                        Just sid -> do
                            userDB <- getUserDataFile progName dbName
                            optDBPath <- firstExistingFile [dbName, userDB]
                            prog (ProgramInfo sid optDBPath userDB) pa
  where
    cfgName  = "default-user.conf"
    dbName   = "default-user.db"
    progName = "steam-compare"

    getConfigFile optOverride = case optOverride of
        Just p  -> tryPaths p [p]
        Nothing -> do
            userConfig <- getUserConfigFile progName cfgName

            -- Pick local if exists, otherwise pick user config.
            tryPaths userConfig [cfgName, userConfig]

    tryPaths reportedPath ps = maybe (Left reportedPath) Right <$> firstExistingFile ps

prog :: ProgramInfo -> ProgramAction -> IO ()
prog pi@(ProgramInfo sid optDBPath defDBPath) pa = do
    c <- connectSqlite3 dbPath
    runRaw c "PRAGMA case_sensitive_like=ON;"

    dbInitAction c

    case pa of
        Update           -> progUpdate c sid
        MatchAction {..} -> case maAct of
            QueryAppID game -> progQueryAppID c maPrefs (inputMod game)
            Blacklist       -> progBlacklist c maPrefs inputMod
            Match           -> progMatch c maPrefs inputMod
          where
            inputMod = maybe id (\d -> head . splitOn d) maCutSuffix
    disconnect c
  where
    (dbInitAction, dbPath) = progDBInfo pi
