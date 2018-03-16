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


-- TODO progress bar while fetching ?

firstExistingFile :: [FilePath] -> IO (Maybe FilePath)
firstExistingFile = firstM doesFileExist

main :: IO ()
main = do
    -- Read config.
    userConfig <- getUserConfigFile progName cfgName
    -- Read from local config first, but prefer XDG user dir instead.
    optConfigPath <- firstExistingFile [cfgName, userConfig]

    case optConfigPath of
        Nothing         -> die $ "missing configuration file: " ++ userConfig
        Just configPath -> do
            optConfig <- readConfigFile configPath
            case optConfig of
                Left e               -> die $ "invalid configuration file format: " ++ e
                Right (Config kvs _) ->
                    case SteamID64 <$> lookup "steamid64" kvs <|> SteamID <$> lookup "steamid" kvs of
                        Nothing  -> die "no steam id given: specify at least 'steamid' or 'steamid64'"
                        Just sid -> do
                            userDB <- getUserDataFile progName dbName
                            optDBPath <- firstExistingFile [dbName, userDB]
                            prog $ ProgramInfo sid optDBPath userDB
  where
    cfgName  = "default-user.conf"
    dbName   = "default-user.db"
    progName = "steam-compare"

prog :: ProgramInfo -> IO ()
prog pi@(ProgramInfo sid optDBPath defDBPath) = do
    c <- connectSqlite3 dbPath
    dbInitAction c

    args <- getArgs
    case args of
        ["update"]      -> progUpdate c sid
        ["appid", game] -> progQueryAppID c game
        ["blacklist"]   -> progBlacklist c
        "match" : r     -> do
            let lineMod = case r of
                    []         -> id
                    cutSeq : _ -> head . splitOn cutSeq
            progMatch c lineMod
        _               -> putStrLn "Invalid command."

    disconnect c
  where
    (dbInitAction, dbPath) = progDBInfo pi
