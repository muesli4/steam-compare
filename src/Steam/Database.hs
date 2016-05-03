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
resetDB c = resetNamesDB c >> resetDetailsDB c >> resetOwnedGamesDB c >> resetBlacklistDB c >> resetAliasDB c

resetNamesDB :: IConnection c => c -> IO ()
resetNamesDB c = runRaw c query
  where
    query = mergeQ [ "DROP TABLE IF EXISTS names"
                   , "CREATE TABLE names (appid int primary key, name string)"
                   , "CREATE INDEX names_name_idx ON names(name)"
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
    stmt <- prepare c "INSERT INTO names VALUES (?, ?)"
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
    query = "WITH vals(name) as (\
                \VALUES " ++ placeholders ns ++ "\
            \) \
            \SELECT n.appid \
            \FROM names as n, vals as v \
            \WHERE n.name LIKE v.name \
                  \AND n.appid not in (select d.appid from details as d) \
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
    query = "WITH games(name) as (\
                \VALUES " ++ placeholders ns ++ "\
            \) \
            \INSERT INTO blacklist \
            \SELECT names.appid \
            \FROM names, games \
            \WHERE names.name LIKE games.name;"
            -- TODO what if rows already exist?


queryMatchingGames :: IConnection c => c -> NE.NonEmpty String -> IO [(Int, String, Maybe Int)]
queryMatchingGames c (NE.toList -> ns) = 
    map (\[i, n, mS] -> (fromSql i, fromSql n, fromSql mS)) <$> quickQuery' c query (map toSql ns)
  where
    query = "WITH games(name) as (\
                \VALUES " ++ placeholders ns ++ "\
            \) \
            \SELECT names.appid, names.name, details.metacritic_score \
            \FROM names, details, games \
            \WHERE games.name LIKE names.name \
                   \AND names.appid = details.appid \
                   \AND names.appid NOT IN owned_games \
                   \AND names.appid NOT IN blacklist \
                   \AND names.appid NOT IN (SELECT appid FROM alias) \
                   \AND details.linux = 1 \
            \ORDER BY details.metacritic_score DESC"

-- TODO does not escape? test %Civ%
queryAppID :: IConnection c => c -> String -> IO [(String, Int)]
queryAppID c game = map (\[n, i] -> (fromSql n, fromSql i)) <$> quickQuery' c "SELECT name, appid FROM names WHERE name LIKE (?)" [toSql game]
