{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Steam.Types where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import           GHC.Generics

-- | Specifies a valid steam identification.
data SteamID = SteamID String
             | SteamID64 String

data GameEntry = GameEntry
               { appid    :: Int
               , name     :: String
               } deriving (Generic, Show)

newtype GameEntryList = GameEntryList [GameEntry] deriving Show

data GameInfo = GameInfo
              { linux      :: Bool
              , optMCScore :: Maybe Int
              , realAppID  :: Int
              } deriving Show

newtype OwnedGame = OwnedGame Int deriving (Eq, Ord, Show)

newtype OwnedGameList = OwnedGameList { unpackOwnedGameList :: [OwnedGame] } deriving Show

pathP :: FromJSON a => [T.Text] -> Value -> Parser a
pathP l v = case l of
    p : ps -> case v of
        Object o -> case HM.lookup p o of
            Just v' -> pathP ps v'
            Nothing -> fail $ "missing segment '" ++ T.unpack p ++ "' of path"
    []     -> parseJSON v

instance FromJSON GameInfo where
    parseJSON v = case v of
        Object (HM.elems -> [o])
            -> GameInfo <$> pathP ["data", "platforms", "linux"] o
                        <*> optional (pathP ["data", "metacritic", "score"] o)
                        <*> pathP ["data", "steam_appid"] o
        _   -> fail "expected object with single property"

instance FromJSON GameEntry where

instance FromJSON GameEntryList where
    parseJSON v = GameEntryList <$> pathP ["applist", "apps"] v

instance FromJSON OwnedGame where
    parseJSON = withObject "expected object" (\o -> OwnedGame <$> o .: "appid")

instance FromJSON OwnedGameList where
    parseJSON v = OwnedGameList <$> pathP ["response", "games"] v
