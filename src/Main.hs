import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Loops                   (firstM)
import           Data.Bifunctor
import           Data.Char
import           Data.Either
import           Data.Functor
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           System.Console.ANSI
import           System.Directory
import           System.Environment
import           System.Environment.XDG.BaseDir
import           System.Exit
import           System.IO
import           Text.Config.Ini
import qualified Data.List.NonEmpty             as NE
import qualified System.IO.Strict               as IOS

import           Steam.Database
import           Steam.Fetch
import           Steam.Types
import           Steam.URL
import           Steam

import           Text.Layout.Table
import qualified System.Console.Terminal.Size as T

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
                Right (Config kvs _) -> do
                    case SteamID64 <$> lookup "steamid64" kvs <|> SteamID <$> lookup "steamid" kvs of
                        Nothing  -> die $ "no steam id given: specify at least 'steamid' or 'steamid64'"
                        Just sid -> do
                            userDB <- getUserDataFile progName dbName
                            optDBPath <- firstExistingFile [dbName, userDB]
                            prog sid optDBPath userDB
  where
    cfgName  = "default-user.conf"
    dbName   = "default-user.db"
    progName = "steam-compare"

putErrStrLn :: String -> IO ()
putErrStrLn e = hPutStr stderr e >> putChar '\n'

handleEither :: IConnection c => c -> (String -> IO ()) -> IO () -> Either String b -> IO ()
handleEither c failAct successAct r = case r of
    Left e  -> rollback c >> failAct ("Update failed: " ++ e)
    Right _ -> putStrLn "Update succesful." >> successAct

-- | Update owned games of a steam user.
progUpdate :: IConnection c => c -> SteamID -> IO ()
progUpdate c sid = resetOwnedGamesDB c >> updateOwnedGames c sid >>= handleEither c putErrStrLn (commit c)

prog :: SteamID -> Maybe FilePath -> String -> IO ()
prog sid optDBPath defDBPath = do

    let (dbInitAction, dbPath) = case optDBPath of
            Nothing -> ( \c -> do
                             let secure = (>>= handleEither c (\e -> disconnect c >> die e) (pure ()))
                             resetDB c
                             secure $ updateGames c 
                             secure $ updateOwnedGames c sid
                             commit c
                       , defDBPath
                       )
            Just p  -> (const $ return (), p)

    c <- connectSqlite3 dbPath
    dbInitAction c

    terminalWidth <- maybe 80 T.width <$> T.size
    let promptGamesList' = promptGamesList terminalWidth

    args <- getArgs
    case args of
        ["update"]      -> progUpdate c sid
        ["appid", game] -> queryAppID c game >>= putTableAlt [def, numCol, def] (\(n, i) -> [n, show i, urlShop i])
        ["blacklist"]   ->
            promptGamesList' id "blacklist" $ \someGames -> do
                i <- insertBlackList c someGames
                commit c
                putStrLn $ "Added " ++ show i ++ " entries."
        "match" : r     -> do
            let clearUser = case r of
                    []         -> id
                    cutSeq : _ -> map (head . splitOn cutSeq)
            promptGamesList' clearUser "match" $ \someGames -> do
                putStrLn "Querying games ..."
                ids <- queryIncompleteAppIDs c someGames
                case ids of
                    []        -> return ()
                    _         -> do
                        putStrLn $ "Found missing details for appids: "
                                    ++ intercalate ", " (map show ids)
                        putStr "Fetching missing details "
                        es <- processDetails c ids
                        case es of
                            [] -> return ()
                            _  -> do
                                putStrLn "Errors occured:"
                                putTableAlt [def, def] toErrorCols es
                        commit c
                putStrLn "Matching input ..."
                res <- queryMatchingGames c someGames
                case res of
                    [] -> putStrLn "No matches found."
                    _  -> do
                        putStrLn "Found matching games:"
                        putTableAlt [ column (expandUntil $ max 40 $ terminalWidth `div` 2) def def def
                                    , numCol
                                    , def
                                    ]
                                    toResultCols
                                    res

        _               -> putStrLn "Invalid command."

    disconnect c
  where
    -- Outputting data
    toResultCols (appID, name, mMC) = let mc = maybe "" show mMC
                                      in [name, mc, urlShop appID]
    toErrorCols (appID, err)        = [show appID, err]

    colorDullBlack s                = setSGRCode [SetColor Background Dull Black] ++ s ++ setSGRCode []
    putTableAlt specs f rs          = mapM_ putStrLnIndent $ altLines [colorDullBlack, id] $ gridLines specs (map f rs)
    putTableCheckered specs f rs    = mapM_ (putStrLnIndent . unwords) $ checkeredCells colorDullBlack id $ grid specs (map f rs)

    putStrLnIndent                  = putStrLn . ("    " ++)

    -- Remove trailing spaces and cut at tabs.
    clearInput = takeWhile (`notElem` "\t\r") . dropWhile isSpace
    clearInputList = map clearInput . filter (not . null) . lines

    getInputList' = clearInputList <$> IOS.getContents

    -- | Ask the user for a list of games, output it layouted and perform an
    -- action with it afterwars.
    promptGamesList tWidth pf verb act = do
            putStrLn $ "Paste games to " ++ verb ++ " for and hit CTRL + D on a new line."
            games <- IOS.run $ pf <$> getInputList'
            case NE.nonEmpty games of
                Just someGames -> do
                    let msg = "Games to " ++ verb ++ " (" ++ show (length games) ++ "):"
                    putStrLn $ '-' <$ msg
                    putStrLn msg
                    -- TODO does not fit the screen
                    putTableCheckered (repeat $ fixedLeftCol 30) id (chunksOf ((tWidth - 5) `div` 31) games)
                    act someGames
                Nothing -> putStrLn "No games specified."

processDetails :: IConnection c => c -> [Int] -> IO [(Int, String)]
processDetails c appIDs = do
    buffering <- hGetBuffering stdout
    hSetBuffering stdout NoBuffering

    -- steam limits queries to 200 entries every 5 minutes
    let batches = chunksOf 150 appIDs

    res <- case batches of
        []     -> return []
        b : bs -> (\l ls -> l ++ concat ls) <$> processBatch b
                                            <*> mapM (\b -> putStr "Waiting for steam limit "
                                                            >> waitFiveMinutes
                                                            >> putStr "Continuing fetching "
                                                            >> processBatch b
                                                     )
                                                     bs
    hSetBuffering stdout buffering
    return res
  where
    processBatch batch = do
        res <- forM batch $ \appID -> do
            let f = (,) appID
            r <- bimap f f <$> fetchGameInfo appID
            putStr "."
            threadDelay oneSecond
            return r
        putStrLn ""
        let (ls, rs) = partitionEithers res
        maybe (return ()) (\someRs -> insertAlias c someRs >> insertDetails c someRs >> commit c) $ NE.nonEmpty rs
        return ls

    waitFiveMinutes = replicateM (60 * 5) $ putStr "." >> threadDelay oneSecond
    
    oneSecond = 1000 * 1000
