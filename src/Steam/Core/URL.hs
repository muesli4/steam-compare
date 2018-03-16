-- | Provides tools to construct steam URLs.
module Steam.Core.URL where

import Steam.Core.Types (SteamID(..))

urlShop :: Int -> String
urlShop appID = "http://store.steampowered.com/app/" ++ show appID ++ "/"

-- https://partner.steamgames.com/documentation/webapi

urlGameInfo :: Int -> String
urlGameInfo appID = "http://store.steampowered.com/api/appdetails?appids=" ++ show appID

urlGameEntryList :: String
urlGameEntryList = "http://api.steampowered.com/ISteamApps/GetAppList/v2/"

urlOwnedGameList :: String -> String -> String
urlOwnedGameList apiKey steamID64 =
    "http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=" ++ steamID64

communityUrlPrefix :: String
communityUrlPrefix = "http://steamcommunity.com/"

communityUrlInfix :: SteamID -> String
communityUrlInfix sid = case sid of
    SteamID s   -> "id/" ++ s
    SteamID64 s -> "profiles/" ++ s

communityUrl :: SteamID -> String -> String
communityUrl sid postfix = communityUrlPrefix ++ communityUrlInfix sid ++ '/' : postfix

urlWishlistHTML :: SteamID -> String
urlWishlistHTML sid = communityUrl sid "wishlist"

-- | Fetch the list of owned games from the steam community public site as XML.
-- This list contains also free games.
-- Note: The XML result can also be used to resolve the SteamID64 from a
-- SteamID, see path: "gamesList/steamID", "gamesList/steamID64".
urlOwnedGameListXML :: SteamID -> String
urlOwnedGameListXML sid = communityUrl sid "games?tab=all&xml=1"
