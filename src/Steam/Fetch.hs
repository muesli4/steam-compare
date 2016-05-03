module Steam.Fetch where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.List
import           Data.Maybe
import           Network.Download
import           Text.Read
import           Text.XML.Light.Proc
import           Text.XML.Light.Types

import           Steam.Types

-- https://partner.steamgames.com/documentation/webapi


fetchJSON :: FromJSON a => String -> IO (Either String a)
fetchJSON = fmap (>>= eitherDecode . BS.fromStrict) . openURI

fetchGameInfo :: Int -> IO (Either String GameInfo)
fetchGameInfo appID = fetchJSON url
  where
    url = "http://store.steampowered.com/api/appdetails?appids=" ++ show appID

fetchGameEntryList :: IO (Either String GameEntryList)
fetchGameEntryList = fetchJSON url
  where
    url = "http://api.steampowered.com/ISteamApps/GetAppList/v2/"

urlOwnedGameList :: String -> String -> String
urlOwnedGameList apiKey steamID64 =
    "http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=" ++ steamID64

fetchOwnedGameList :: String -> String -> IO (Either String OwnedGameList)
fetchOwnedGameList apiKey = fetchJSON . urlOwnedGameList apiKey

-- | Specifies a valid steam identification.
data SteamID = SteamID String
             | SteamID64 String

-- TODO can also be used to resolve SteamID64 from SteamID: "gamesList/steamID", "gamesList/steamID64"
urlOwnedGameListXML :: SteamID -> String
urlOwnedGameListXML sid = urlPrefix ++ urlInfix sid ++ urlPostfix
  where
    urlPrefix    = "http://steamcommunity.com/"
    urlPostfix   = "/games?tab=all&xml=1"
    urlInfix sid = case sid of
        SteamID s   -> "id/" ++ s
        SteamID64 s -> "profile/" ++ s

-- | Fetch the list of games from the profile using a SteamID. Note that this
-- list also contains free to play games.
fetchOwnedGameListXML :: SteamID -> IO (Either String OwnedGameList)
fetchOwnedGameListXML sid = (>>= parseXML) <$> openAsXML (urlOwnedGameListXML sid)
  where
    parseXML cs = case parseAppIDs cs of
        Just ts -> Right $ OwnedGameList $ map OwnedGame ts
        Nothing -> Left "failed to parse xml"
    strQName s = blank_name { qName = s }
    findChildStr = findChild . strQName
    parseAppIDs = find ((== "gamesList") . qName . elName) . onlyElems
                  >=> findChildStr "games"
                  >=> mapM (findChildStr "appID" >=> readMaybe . strContent) . findChildren (strQName "game")

