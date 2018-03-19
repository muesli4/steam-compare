{-# LANGUAGE ViewPatterns, RecordWildCards, TupleSections, RankNTypes, StandaloneDeriving, UndecidableInstances #-}
module Steam.Core.Database
    ( resetDB
    , resetGamesDB
    , resetDetailsDB
    , resetAliasDB
    , resetOwnedGamesDB
    , resetBlacklistDB
    , insertGames
    , insertOwnedGames
    , insertDetails
    , insertAlias
    , insertBlackList
    , queryIncompleteAppIDs
    , UseFilter(UseFilter)
    , ResultRow(..)
    , OptSimpleRow
    , OptDetailRow
    , Identity(..)
    , SimpleRow
    , DetailRow
    , queryMatchingOptSimple
    , queryMatchingOptDetail
    , queryMatchingSimple
    , queryMatchingDetail
    , queryBlacklist
    , module Steam.Core.Database.Types
    ) where

import           Data.Bifunctor
import           Data.Function
import           Data.Functor.Identity
import           Data.List
import           Data.Maybe
import           Database.HDBC
import qualified Data.List.NonEmpty as NE

import           Steam.Types
import           Steam.Core.Types
import           Steam.Core.Database.Types

placeholder :: String
placeholder = "(?)"

placeholders :: [a] -> String
placeholders = intercalate "," . (placeholder <$)

mergeQ :: [String] -> String
mergeQ = concatMap (++ ";")

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

data Table = Table
           { name    :: String
           , numCols :: Int
           }

games, ownedGames, details, alias :: Table
games      = Table "games" 2
ownedGames = Table "owned_games" 1
details    = Table "details" 3
alias      = Table "alias" 2

insertMany :: IConnection c => c -> Table -> [[SqlValue]] -> IO ()
insertMany c (Table name numCols) vs = do
    stmt <- prepare c $ "INSERT INTO " ++ name ++ " VALUES (" ++ intercalate "," (replicate numCols "?") ++ ")"
    executeMany stmt vs
    finish stmt

insertGames :: IConnection c => c -> GameEntryList -> IO ()
insertGames c (GameEntryList ges) = insertMany c games $ map (\(GameEntry i n) -> [toSql i, toSql n]) ges

insertOwnedGames :: IConnection c => c -> OwnedGameList -> IO ()
insertOwnedGames c (OwnedGameList ogs) = insertMany c ownedGames $ map (\(OwnedGame i) -> [toSql i]) ogs

insertDetails :: IConnection c => c -> NE.NonEmpty (Int, GameInfo) -> IO ()
insertDetails c (NE.toList -> ts) = insertMany c details $ map (\(appID, gi) -> [toSql appID, toSql $ if linux gi then 1 :: Int else 0, toSql $ optMCScore gi]) ts

insertAlias :: IConnection c => c -> NE.NonEmpty (Int, GameInfo) -> IO ()
insertAlias c (NE.toList -> ts) = insertMany c alias $ map (\(appID, realAppID) -> [toSql appID, toSql realAppID])
                                                     $ filter (uncurry (/=))
                                                     $ map (second realAppID) ts

unaryCall :: String -> String -> String
unaryCall f x = f ++ '(' : x ++ ")"

alterBinFunCase
    :: CaseMode
    -> (String -> String -> String)
    -> (String -> String -> String)
alterBinFunCase c f = f `on` mod
  where
    mod = case c of
        CaseInsensitive -> unaryCall "lower"
        CaseSensitive   -> id

predForMatchMode :: MatchMode -> (String -> String -> String)
predForMatchMode m = case m of
    Exact        -> equal
    PartialLeft  -> partial
    PartialRight -> flip partial
    PartialBoth  -> \ls rs -> '(' : partial ls rs ++ " OR " ++ partial rs ls ++ ")"
    Wildcard     -> like
  where
    equal ls rs   = ls ++ " = " ++ rs
    partial ls rs = ls `like` ("('%' || " ++ rs ++ " || '%')")
    like ls rs = ls ++ " like " ++ rs

predicate :: MatchPrefs -> (String -> String -> String)
predicate MatchPrefs {..} = alterBinFunCase mpCaseMode $ predForMatchMode mpMatchMode

-- | Creates a query that provides a table named __matched_games__ which
-- contains all games that matched the input either partially or exact. This
-- depends on the passed 'MatchPrefs'. If missing input entries are included,
-- the query may yield null values for rows that didn't have a (single) match.
-- Otherwise they are completely dropped.
matchedGamesQuery :: IncludeNulls -> MatchPrefs -> [a] -> String -> String
matchedGamesQuery IncludeNulls {..} mp@MatchPrefs {..} ns = withQuery $ case mpMatchMode of
    -- Do not bother with partial matches.
    Exact -> [ inputsCTE
             , if mpSingleMatch
               then foundExactUniqueCTE
               else foundExactAllCTE
             , matchedGamesTemplate $
                  "    SELECT i.name, e.input_name, e.exact_appid\n\
                  \    FROM " ++ rejoinLeft "inputs i" "found_exact e" "i.name" "e.input_name"
             ]
    _     -> if mpSingleMatch
             then [ inputsCTE
                  , foundExactUniqueCTE
                  , foundPartialUniqueCTE
                  , matchedGamesCTE
                  ]
             -- Do not bother with exact matches.
             else [ inputsCTE
                  , foundPartialAllCTE
                  , matchedGamesTemplate $
                        "    SELECT i.name, p.full_name, p.appid\n\
                        \    FROM " ++ rejoinLeft "inputs i" "found_partial p" "i.name" "p.input_name"
                  ]
  where
    -- | Rejoin to include missing columns with null values.
    rejoinLeft ltd rtd lc rc =
        let join = if includeNulls then "LEFT OUTER JOIN" else "INNER JOIN"
        in ltd ++ ' ' : join ++ ' ' : rtd ++ " ON " ++ lc ++ " = " ++ rc

    cte n cs body         = n ++ '(' : intercalate ", " cs ++ ") AS (\n" ++ body ++ "\n)\n"
    withQuery ctes q      = "WITH " ++ intercalate ", " ctes ++ q
    inputsCTE             =
        cte "inputs" ["name"] $ "    VALUES " ++ placeholders ns

    matchedGamesTemplate  = cte "matched_games" ["search_term", "name", "appid"]
    foundPartialTemplate  = cte "found_partial" ["input_name", "full_name", "appid"]
    foundExactTemplate    = cte "found_exact" ["input_name", "exact_appid"]
    -- TODO don't lookup entries that have been found exact ?
    -- | Include only partial matches that have exactly one match.
    foundPartialUniqueCTE = foundPartialTemplate $
            "    SELECT i.name, MIN(g.name), MIN(g.appid)\n\
            \    FROM inputs i, games g\n\
            \    WHERE " ++ predicate mp "g.name" "i.name" ++ "\n\
            \    GROUP BY i.name\n\
            \    HAVING MIN(g.appid) = MAX(g.appid)"

    foundPartialAllCTE    = foundPartialTemplate $
            "    SELECT i.name, g.name, g.appid\n\
            \    FROM inputs i, games g\n\
            \        WHERE " ++ predicate mp "g.name" "i.name"

    foundExactUniqueCTE   = foundExactTemplate
            "    SELECT i.name, MIN(g.appid)\n\
            \    FROM inputs i, games g\n\
            \    WHERE i.name = g.name\n\
            \    GROUP BY i.name\n\
            \    HAVING MIN(g.appid) = MAX(g.appid)"

    foundExactAllCTE      = foundExactTemplate
            "    SELECT i.name, g.appid\n\
            \    FROM inputs i, games g\n\
            \    WHERE i.name = g.name"

    matchedGamesCTE       = matchedGamesTemplate $
            "    SELECT i.name as search_term,\n\
            \           COALESCE(e.input_name, p.full_name) as name,\n\
            \           COALESCE(e.exact_appid, p.appid) as appid\n\
            \    FROM (inputs i LEFT OUTER JOIN found_exact e ON i.name = e.input_name)\n\
            \         LEFT OUTER JOIN found_partial p\n\
            \         ON (i.name = p.input_name" ++ ( if includeNulls
                                                      then ""
                                                      else " AND e.input_name IS NULL"
                                                    ) ++ ")"
            ++ if includeNulls
               then ""
               -- Work around wrong column name resolution in sqlite3
               else "         WHERE NOT (COALESCE(e.input_name, p.full_name) IS NULL)"

insertBlackList :: IConnection c => c -> MatchPrefs -> NE.NonEmpty String -> IO Integer
insertBlackList c mp (NE.toList -> ns) = run c query $ toSql <$> ns
  where
    query = matchedGamesQuery (IncludeNulls False) mp ns
                "INSERT INTO blacklist \
                \SELECT DISTINCT m.appid \
                \FROM matched_games m"

-- | Finds all entries in the database which do not have details.
queryIncompleteAppIDs :: IConnection c => c -> MatchPrefs -> NE.NonEmpty String -> IO [Int]
queryIncompleteAppIDs c mp (NE.toList -> ns) =
    fmap (fromSql . head) <$> quickQuery' c query (toSql <$> ns)
  where
    query = matchedGamesQuery (IncludeNulls False) mp ns
                "SELECT DISTINCT m.appid \
                \FROM matched_games m \
                \WHERE m.appid NOT IN (SELECT d.appid FROM details AS d) \
                \EXCEPT SELECT * FROM blacklist;"
                -- \AND NOT (m.appid IS NULL) \

-- | 'f :: * -> * -> *' and will recieve the input name as first argument and
-- the succesful output with optional details as second.
data ResultRow f d
    = ResultRow
    { rrInput :: String
    , rrRow   :: f (GameEntry, d)
    }

deriving instance (Show d, Show (f (GameEntry, d))) => Show (ResultRow f d)

type OptSimpleRow = ResultRow Maybe ()
type OptDetailRow = ResultRow Maybe (Maybe Int)
type SimpleRow = ResultRow Identity ()
type DetailRow = ResultRow Identity (Maybe Int)

type QueryIO row = forall c. IConnection c => c -> MatchPrefs -> UseFilter -> NE.NonEmpty String -> IO [row]

newtype IncludeNulls = IncludeNulls { includeNulls :: Bool } deriving Show
newtype IncludeDetails = IncludeDetails { includeDetails :: Bool } deriving Show
newtype UseFilter = UseFilter { useFilter :: Bool } deriving Show

matchQuery :: UseFilter -> IncludeDetails -> String
matchQuery UseFilter {..} IncludeDetails {..} =
    -- TODO use includeNulls to make join more efficient
    if useFilter
    then "SELECT m.search_term, m.name, m.appid" ++ (if includeDetails then ", d.metacritic_score" else "") ++ " \
          \FROM matched_games m LEFT OUTER JOIN details d ON m.appid = d.appid \
          \WHERE m.appid NOT IN owned_games \
          \      AND m.appid NOT IN blacklist \
          \      AND m.appid NOT IN (SELECT appid FROM alias) \
          \      AND d.linux = 1 \
          \ORDER BY d.metacritic_score DESC"
    else "SELECT m.search_term, m.name, m.appid" ++ (if includeDetails then ", d.metacritic_score" else "") ++ " \
         \FROM matched_games m" ++ if includeDetails then " LEFT OUTER JOIN details d ON m.appid = d.appid" else ""

-- TODO does not escape? test %Civ%
-- TODO use alias even without filter?
queryMatchingTemplate :: IncludeNulls -> IncludeDetails -> ([SqlValue] -> row) -> QueryIO row
queryMatchingTemplate includeNulls includeDetails convert c mp useFilter (NE.toList -> ns) =
    fmap convert <$> quickQuery' c query (toSql <$> ns)
  where
    query = matchedGamesQuery includeNulls mp ns $ matchQuery useFilter includeDetails

queryMatchingOptSimple :: QueryIO OptSimpleRow
queryMatchingOptSimple = queryMatchingTemplate (IncludeNulls True) (IncludeDetails False) convert
  where
    convert :: [SqlValue] -> OptSimpleRow
    convert [input, name, appid] = ResultRow (fromSql input) $ (, ()) <$> (GameEntry <$> fromSql appid <*> fromSql name)

queryMatchingOptDetail :: QueryIO OptDetailRow
queryMatchingOptDetail = queryMatchingTemplate (IncludeNulls True) (IncludeDetails True) convert
  where
    convert :: [SqlValue] -> OptDetailRow
    convert [input, name, appid, score] = ResultRow (fromSql input) $ (, fromSql score) <$> (GameEntry <$> fromSql appid <*> fromSql name)

queryMatchingSimple :: QueryIO SimpleRow
queryMatchingSimple = queryMatchingTemplate (IncludeNulls False) (IncludeDetails False) convert
  where
    -- TODO use coerce
    convert [input, name, appid] = ResultRow (fromSql input) $ Identity (GameEntry (fromSql appid) (fromSql name), ())

queryMatchingDetail :: QueryIO DetailRow
queryMatchingDetail = queryMatchingTemplate (IncludeNulls False) (IncludeDetails True) convert
  where
    convert :: [SqlValue] -> DetailRow
    convert [input, name, appid, score] = ResultRow (fromSql input) $ Identity (GameEntry (fromSql appid) (fromSql name), fromSql score)

queryBlacklist :: IConnection c => c -> IO [String]
queryBlacklist c = map (\[c] -> fromSql c) <$> quickQuery' c "SELECT g.name FROM blacklist b, games g WHERE g.appid = b.appid" []
