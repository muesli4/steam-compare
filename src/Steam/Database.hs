{-# LANGUAGE ViewPatterns #-}
module Steam.Database where

import           Data.Bifunctor
import           Data.List
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Database.HDBC

import           Steam.Types

placeholder :: String
placeholder = "(?)"

placeholders :: [a] -> String
placeholders = intercalate "," . map (const placeholder)

mergeQ :: [String] -> String
mergeQ qs = concat $ zipWith (++) qs (repeat ";")

--newtype DB c a = DB (ReaderT c IO a)

resetDB :: IConnection c => c -> IO ()
resetDB c = resetGamesDB c >> resetDetailsDB c >> resetOwnedGamesDB c >> resetBlacklistDB c >> resetAliasDB c

resetGamesDB :: IConnection c => c -> IO ()
resetGamesDB c = runRaw c query
  where
    query = mergeQ [ "DROP TABLE IF EXISTS games"
                   , "CREATE TABLE games (appid int primary key, name string)"
                   , "CREATE INDEX games_name_idx ON games(name)"
                   ]

-- TODO sqlite does have no boolean
resetDetailsDB :: IConnection c => c -> IO ()
resetDetailsDB c = runRaw c query
  where
    query = mergeQ [ "DROP TABLE IF EXISTS details"
                   , "CREATE TABLE details (appid int primary key, linux int not null, metacritic_score int)"
                   ]
        
resetAliasDB :: IConnection c => c -> IO ()
resetAliasDB c = runRaw c query
  where
    query = mergeQ [ "DROP TABLE IF EXISTS alias"
                   , "CREATE TABLE alias (appid int primary key, real_appid int not null)"
                   ]

resetOwnedGamesDB :: IConnection c => c -> IO ()
resetOwnedGamesDB c = runRaw c query
  where
    query = mergeQ [ "DROP TABLE IF EXISTS owned_games"
                   , "CREATE TABLE owned_games (appid int primary key)"
                   ]

resetBlacklistDB :: IConnection c => c -> IO ()
resetBlacklistDB c = runRaw c query
  where
    query = mergeQ [ "DROP TABLE IF EXISTS blacklist"
                   , "CREATE TABLE blacklist (appid int primary key)"
                   ]

insertGames :: IConnection c => c -> GameEntryList -> IO ()
insertGames c (GameEntryList ges) = do
    stmt <- prepare c "INSERT INTO games VALUES (?, ?)"
    executeMany stmt $ map (\(GameEntry i n) -> [toSql i, toSql n]) ges

insertOwnedGames :: IConnection c => c -> OwnedGameList -> IO ()
insertOwnedGames c (OwnedGameList ogs) = do
    stmt <- prepare c "INSERT INTO owned_games VALUES (?)"
    executeMany stmt $ map (\(OwnedGame i) -> [toSql i]) ogs

-- | Finds all entries in the database which do not have details.
queryIncompleteAppIDs :: IConnection c => c -> NE.NonEmpty String -> IO [Int]
queryIncompleteAppIDs c (NE.toList -> ns) = do
    stmt <- prepare c query
    execute stmt $ map toSql ns
    (fmap $ fromSql . head) <$> fetchAllRows' stmt
  where
    query = "WITH inputs(name) as (\
                \VALUES " ++ placeholders ns ++ "\
            \) \
            \SELECT g.appid \
            \FROM games AS g, inputs AS i \
            \WHERE g.name LIKE i.name \
                  \AND g.appid NOT IN (SELECT d.appid FROM details AS d) \
            \EXCEPT SELECT * FROM blacklist;"

insertDetails :: IConnection c => c -> NE.NonEmpty (Int, GameInfo) -> IO ()
insertDetails c (NE.toList -> ts) = do
        stmt <- prepare c "INSERT INTO details VALUES (?, ?, ?)"
        executeMany stmt $ map (\(appID, gi) -> [toSql appID, toSql $ if linux gi then 1 :: Int else 0, toSql $ optMCScore gi]) ts

insertAlias :: IConnection c => c -> NE.NonEmpty (Int, GameInfo) -> IO ()
insertAlias c (NE.toList -> ts) = do
        stmt <- prepare c "INSERT INTO alias VALUES (?, ?)"
        executeMany stmt $ map (\(appID, realAppID) -> [toSql appID, toSql realAppID])
                         $ filter (uncurry (/=))
                         $ map (second realAppID) ts

insertBlackList :: IConnection c => c -> NE.NonEmpty String -> IO Integer
insertBlackList c (NE.toList -> ns) = run c query $ map toSql ns
  where
    query = "WITH inputs(name) as (\
                \VALUES " ++ placeholders ns ++ "\
            \) \
            \INSERT INTO blacklist \
            \SELECT games.appid \
            \FROM games, inputs \
            \WHERE games.name LIKE inputs.name;"
            -- TODO what if rows already exist?


queryMatchingGames :: IConnection c => c -> NE.NonEmpty String -> IO [(Int, String, Maybe Int)]
queryMatchingGames c (NE.toList -> ns) = 
    map (\[i, n, mS] -> (fromSql i, fromSql n, fromSql mS)) <$> quickQuery' c query (map toSql ns)
  where
    query = "WITH inputs(name) as (\
                \VALUES " ++ placeholders ns ++ "\
            \) \
            \SELECT games.appid, games.name, details.metacritic_score \
            \FROM games, details, inputs \
            \WHERE inputs.name LIKE games.name \
                   \AND games.appid = details.appid \
                   \AND games.appid NOT IN owned_games \
                   \AND games.appid NOT IN blacklist \
                   \AND games.appid NOT IN (SELECT appid FROM alias) \
                   \AND details.linux = 1 \
            \ORDER BY details.metacritic_score DESC"

-- TODO does not escape? test %Civ%
queryAppID :: IConnection c => c -> String -> IO [(String, Int)]
queryAppID c game = map (\[n, i] -> (fromSql n, fromSql i)) <$> quickQuery' c "SELECT name, appid FROM games WHERE name LIKE (?)" [toSql game]
