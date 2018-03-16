-- | Actions for the executable.
module Steam.Action
    ( ProgramInfo(..)
    , progDBInfo
    , progUpdate
    , progQueryAppID
    , progBlacklist
    , progMatch
    ) where

import           Control.Concurrent
import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Either
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Database.HDBC
import           System.Console.ANSI
import           System.Exit
import           System.IO
import           Text.Layout.Table
import qualified Data.List.NonEmpty             as NE
import qualified System.Console.Terminal.Size as T
import qualified System.IO.Strict               as IOS

import           Steam.Core.Database
import           Steam.Core.Fetch
import           Steam.Core.Types
import           Steam.Core.URL
import           Steam.Update
import           Steam.Util

-- TODO add lookup and link generation for a list of games

data ProgramInfo
    = ProgramInfo
    { piSID :: SteamID
    , piOptDBPath :: Maybe String
    , piDefDBPath :: String
    }

progDBInfo :: IConnection c => ProgramInfo -> (c -> IO (), String)
progDBInfo (ProgramInfo sid optDBPath fallbackPath) = case optDBPath of
    Nothing -> (initDB, fallbackPath)
    Just p  -> (const $ pure (), p)
  where
    -- TODO replace die with some escape mechanism like continuations?
    initDB c = do
        let secure = (>>= handleEither c (\e -> disconnect c >> die e) (pure ()))
        resetDB c
        secure $ updateGames c 
        secure $ updateOwnedGames c sid
        commit c

-- | Update owned games of a steam user.
progUpdate :: IConnection c => c -> SteamID -> IO ()
progUpdate c sid = resetOwnedGamesDB c >> updateOwnedGames c sid >>= handleEither c putErrStrLn (commit c)

-- | Query the appid for a specific game.
progQueryAppID :: IConnection c => c -> String -> IO ()
progQueryAppID c game = queryAppID c game >>= putTableAltWith [def, numCol, def] (\(n, i) -> [n, show i, urlShop i])
putStrLnIndent :: String -> IO ()
putStrLnIndent = putStrLn . ("    " ++)

colorDullBlack :: String -> String
colorDullBlack s = setSGRCode [SetColor Background Dull Black] ++ s ++ setSGRCode []

putTableAlt :: [ColSpec] -> [Row String] -> IO ()
putTableAlt specs rs =
    mapM_ putStrLnIndent $ altLines [colorDullBlack, id]
                         $ gridLines specs rs

putTableAltWith :: [ColSpec] -> (a -> [String]) -> [a] -> IO ()
putTableAltWith specs extractCols =
    putTableAlt specs . fmap extractCols

-- Remove trailing spaces and cut at tabs.
cleanInputLine :: String -> String
cleanInputLine = takeWhile (`notElem` "\t\r") . dropWhile isSpace

cleanInputList :: [String] -> [String]
cleanInputList = fmap cleanInputLine . filter (not . null)

getInputList :: IOS.SIO [String]
getInputList = cleanInputList . lines <$> IOS.getContents

terminalWidth :: IO Int
terminalWidth = maybe 80 T.width <$> T.size

-- | Ask the user for a list of games, output it layouted and perform an
-- action with it afterwars.
promptGamesList :: (String -> String) -> String -> (NE.NonEmpty String -> IO ()) -> IO ()
promptGamesList inputLineMod verb act = do
    tWidth <- terminalWidth
    putStrLn $ "Paste games to " ++ verb ++ " for and hit CTRL + D on a new line."
    games <- IOS.run $ fmap inputLineMod <$> getInputList
    case NE.nonEmpty games of
        Just someGames -> do
            let msg = "Games to " ++ verb ++ " (" ++ show (length games) ++ "):"
            putStrLn $ '-' <$ msg
            putStrLn msg
            -- TODO does not fit the screen
            putGridCheckered (repeat $ fixedLeftCol 30) id (chunksOf ((tWidth - 5) `div` 31) games)
            act someGames
        Nothing -> putStrLn "No games specified."
  where
    putGridCheckered specs f rs =
        mapM_ (putStrLnIndent . unwords) $ checkeredCells colorDullBlack id
                                         $ grid specs (map f rs)


progBlacklist :: IConnection c => c -> IO ()
progBlacklist c = promptGamesList id "blacklist" $ \someGames -> do
    i <- insertBlackList c someGames
    commit c
    putStrLn $ "Added " ++ show i ++ " entries."

progMatch :: IConnection c => c -> (String -> String) -> IO ()
progMatch c lineModFun = promptGamesList lineModFun "match" $ \someGames -> do
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
                    putTableAltWith [def, def] toErrorCols es
            commit c
    putStrLn "Matching input ..."
    res <- queryMatchingGames c someGames
    case res of
        [] -> putStrLn "No matches found."
        _  -> do
            putStrLn "Found matching games:"
            tWidth <- terminalWidth
            putTableAltWith [ column (expandUntil $ max 40 $ tWidth `div` 2) def def def
                            , numCol
                            , def
                            ]
                            toResultCols
                            res

  where
    -- Outputting data
    toResultCols (appID, name, mMC) = [name, maybe "" show mMC, urlShop appID]
    toErrorCols (appID, err)        = [show appID, err]

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
