{-# LANGUAGE RecordWildCards, RankNTypes #-}
-- | Actions for the executable.
module Steam.Action
    ( ProgramInfo(..)
    , progDBInfo
    , progUpdate
    , progBlacklist
    , progDumpBlacklist
    , progQuery
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
import qualified Data.List.NonEmpty           as NE
import qualified System.Console.Terminal.Size as T
import qualified System.IO.Strict             as IOS

import           Steam.Core.Database
import           Steam.Core.Fetch
import           Steam.Core.Types
import           Steam.Core.URL
import           Steam.Types
import           Steam.Update
import           Steam.Util

-- TODO add lookup and link generation for a list of games

type InputMod = String -> String

data ProgramInfo
    = ProgramInfo
    { piSID    :: SteamID
    , piDbFile :: Either FilePath FilePath
    }

progDBInfo :: IConnection c => ProgramInfo -> (c -> IO (), String)
progDBInfo (ProgramInfo sid dbFile) = case dbFile of
    -- Create a new database.
    Left p  -> (initDB, p)
    Right p -> (const $ pure (), p)
  where
    -- TODO replace die with some escape mechanism like continuations?
    initDB c = do
        let secure = (>>= handleEither c (\e -> disconnect c >> die e) (pure ()))
        resetDB c
        secure $ updateGames c 
        secure $ updateOwnedGames c sid
        commit c

-- TODO necessary?
-- | Update owned games of a steam user.
progUpdate :: IConnection c => c -> SteamID -> IO ()
progUpdate c sid = resetOwnedGamesDB c >> updateOwnedGames c sid >>= handleEither c putErrStrLn (commit c)

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
promptGamesList :: InputMod -> String -> (NE.NonEmpty String -> IO ()) -> IO ()
promptGamesList inputLineMod verb act = do
    tWidth <- terminalWidth
    putStrLn $ "Paste games to " ++ verb ++ " for and hit CTRL + D on a new line."
    games <- IOS.run $ fmap inputLineMod <$> getInputList
    case NE.nonEmpty games of
        Just someGames -> do
            putOverviewMsgLn ("Games to " ++ verb) someGames
            -- TODO does not fit the screen
            putGridCheckered (repeat $ fixedLeftCol 30) id (chunksOf ((tWidth - 5) `div` 31) games)
            act someGames
        Nothing -> putStrLn "No games specified."
  where
    putGridCheckered specs f rs =
        mapM_ (putStrLnIndent . unwords) $ checkeredCells colorDullBlack id
                                         $ grid specs (map f rs)


progBlacklist :: IConnection c => c -> MatchPrefs -> InputMod -> IO ()
progBlacklist c mp inputMod = promptGamesList inputMod "blacklist" $ \someGames -> do
    i <- insertBlackList c mp someGames
    commit c
    putStrLn $ "Added " ++ show i ++ " entries."

promptGamesListAndFetchDetails :: IConnection c => c -> MatchPrefs -> InputMod -> String -> (NE.NonEmpty String -> IO ()) -> IO ()
promptGamesListAndFetchDetails c mp inputMod verb act =
    promptGamesList inputMod verb $ \someGames -> do
        putStrLn "Querying games ..."
        ids <- queryIncompleteAppIDs c mp someGames
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
        act someGames
  where
    toErrorCols (appID, err)        = [show appID, err]

requiresOptional :: UnmatchedAction -> Bool
requiresOptional ua = case ua of
    Hide -> False
    _    -> True

requiresDetails :: OutputMode -> Bool
requiresDetails om = case om of
    Tabular -> True
    _       -> False

seperateResultRows :: [ResultRow Maybe d] -> ([String], [ResultRow Identity d])
seperateResultRows = partitionEithers . fmap f
  where
    f rr@(ResultRow name optRow) = maybe (Left name) (Right . ResultRow name . Identity) optRow

putTable :: [[String]] -> IO ()
putTable css = do
    tWidth <- terminalWidth
    let colSpecs = replicate 2 (column (expandUntil $ max 40 $ tWidth `div` 3) def def def)
                   ++ numCol : repeat def
    putTableAlt colSpecs css

putTableRR :: (f (GameEntry, d) -> Maybe [String]) -> [ResultRow f d] -> IO ()
putTableRR f = putTable . fmap (\r -> rrInput r : fromMaybe [] (f $ rrRow r))

putPlain :: [String] -> IO ()
putPlain = mapM_ putStrLn

putPlainRR :: (f (GameEntry, d) -> Maybe String) -> [ResultRow f d] -> IO ()
putPlainRR f = putPlain . fmap (\r -> fromMaybe (rrInput r) $ f (rrRow r))

putOutputRR
    :: OutputMode
    -> (forall a. ((GameEntry, d) -> a) -> f (GameEntry, d) -> Maybe a)
    -> (d -> [String])
    -> [ResultRow f d]
    -> IO ()
putOutputRR om f g = case om of
    Plain    -> putPlainRR (f $ \(ge, d) -> name ge)
    Markdown -> putPlainRR (f $ \(ge, d) -> markdownLink ge)
    Tabular  -> putTableRR (f $ \(ge, d) -> tableRow ge $ g d)
  where
    markdownLink :: GameEntry -> String
    markdownLink GameEntry {..} = '[' : name ++ "](" ++ urlShop appid ++ ")"

    tableRow :: GameEntry -> [String] -> [String]
    tableRow GameEntry {..} cs = name : cs ++ [urlShop appid]

putOverviewMsgLn :: Foldable f => String -> f a -> IO ()
putOverviewMsgLn msg xs =
    putStrLn header >> putStrLn ('-' <$ header)
  where
    header = msg ++ " (" ++ show (length xs) ++ "):"

putActionMsgLn :: Foldable f => String -> String -> f a -> IO () -> IO ()
putActionMsgLn failMsg msg xs act =
    if null xs
    then putStrLn failMsg
    else putOverviewMsgLn msg xs >> act

progMatchingQuery :: IConnection c => UseFilter -> c -> MatchPrefs -> InputMod -> OutputPrefs -> IO ()
progMatchingQuery useFilter c mp inputMod OutputPrefs {..} = promptGamesList' inputMod "match" $ \someGames ->
    let queryAndDisplayEnsureSeperate q d = do
            rs <- q c mp useFilter someGames
            case opUnmatchedAction of
                DisplaySeperate -> do
                    let (us, rs') = seperateResultRows rs
                    unless (null us) $ putOverviewMsgLn "Unmatched input" us >> putPlain us >> putStrLn ""
                    outputMatches handleIdentity d rs'
                _               -> outputMatches handleMaybe d rs
        queryAndDisplay q d =
            q c mp useFilter someGames >>= outputMatches handleIdentity d
    in case (useDetails, requiresOptional opUnmatchedAction) of
        (False, False) -> queryAndDisplay queryMatchingSimple emptyDetailRow
        (False, True ) -> queryAndDisplayEnsureSeperate queryMatchingOptSimple emptyDetailRow
        (True , False) -> queryAndDisplay queryMatchingDetail detailRow
        (True , True ) -> queryAndDisplayEnsureSeperate queryMatchingOptDetail detailRow
  where
    promptGamesList' =
        if useDetails
        then promptGamesListAndFetchDetails c mp
        else promptGamesList
    useDetails = requiresDetails opOutputMode

    outputMatches :: forall f d. (forall a. ((GameEntry, d) -> a) -> f (GameEntry, d) -> Maybe a)
                  -> (d -> [String])
                  -> [ResultRow f d]
                  -> IO ()
    outputMatches h d rs  =
        putActionMsgLn "There were no matches." "Matched input" rs $ putOutputRR opOutputMode h d rs

    handleIdentity :: forall a b. (a -> b) -> Identity a -> Maybe b
    handleIdentity g (Identity t) = Just $ g t
    handleMaybe :: forall a b. (a -> b) -> Maybe a -> Maybe b
    handleMaybe = fmap

    detailRow         = maybe [""] ((: []) . show)
    emptyDetailRow () = []

progQuery :: IConnection c => c -> MatchPrefs -> InputMod -> OutputPrefs -> IO ()
progQuery = progMatchingQuery (UseFilter False)

progMatch :: IConnection c => c -> MatchPrefs -> InputMod -> OutputPrefs -> IO ()
progMatch = progMatchingQuery (UseFilter True)

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

progDumpBlacklist :: IConnection c => c -> IO ()
progDumpBlacklist c = queryBlacklist c >>= mapM_ putStrLn
