module Steam.Fetch where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.List
import           Data.Maybe
import           Network.Download
import           Text.Read
import           Text.XML.Light

import           Steam.Types

-- https://partner.steamgames.com/documentation/webapi

urlGameInfo :: Int -> String
urlGameInfo appID = "http://store.steampowered.com/api/appdetails?appids=" ++ show appID

urlGameEntryList :: String
urlGameEntryList = "http://api.steampowered.com/ISteamApps/GetAppList/v2/"

urlOwnedGameList :: String -> String -> String
urlOwnedGameList apiKey steamID64 =
    "http://api.steampowered.com/IPlayerService/GetOwnedGames/v0001/?key=" ++ apiKey ++ "&steamid=" ++ steamID64

-- | Specifies a valid steam identification.
data SteamID = SteamID String
             | SteamID64 String

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
urlOwnedGameListXML :: SteamID -> String
urlOwnedGameListXML sid = communityUrl sid "games?tab=all&xml=1"
-- TODO can also be used to resolve SteamID64 from SteamID: "gamesList/steamID", "gamesList/steamID64"

fetchJSON :: FromJSON a => String -> IO (Either String a)
fetchJSON = fmap (>>= eitherDecode . BS.fromStrict) . openURI

fetchGameInfo :: Int -> IO (Either String GameInfo)
fetchGameInfo = fetchJSON . urlGameInfo

fetchGameEntryList :: IO (Either String GameEntryList)
fetchGameEntryList = fetchJSON urlGameEntryList

fetchOwnedGameList :: String -> String -> IO (Either String OwnedGameList)
fetchOwnedGameList apiKey = fetchJSON . urlOwnedGameList apiKey

-- | Fetch the list of games from the profile using a SteamID. Note that this
-- list also contains free to play games.
fetchOwnedGameListXML :: SteamID -> IO (Either String OwnedGameList)
fetchOwnedGameListXML sid =
    (>>= parseXML) <$> openAsXML (urlOwnedGameListXML sid)
  where
    parseXML cs = case parseAppIDs cs of
        Just ts -> Right $ OwnedGameList $ map OwnedGame ts
        Nothing -> Left "failed to parse XML"
    findChildStr = findChild . unqual
    parseAppIDs = find ((== "gamesList") . qName . elName) . onlyElems
                  >=> findChildStr "games"
                  >=> mapM (findChildStr "appID" >=> readMaybe . strContent)
                      . findChildren (unqual "game")

fetchWishlistHTML :: SteamID -> IO (Either String GameEntryList)
fetchWishlistHTML sid = (>>= parseHTML) <$> openAsXML (urlWishlistHTML sid)
  where
    parseHTML cs = case parseRows cs of
        Just ges -> Right ges
        Nothing  -> Left "failed to parse HTML"
    {-
        html
        body
        div.responsive_page_frame with_header
        div.responsive_page_content
        div.responsive_page_template_content
        div.pagecontent no_header
        div#BG_bottom
        div#mainContents
        div#tabs_basebg
        div#wishlist_items
            div#wishListRow -> appid in id attribute with format game_<appid>
                div#wishListRowItem
                    h4#ellipsis -> game name
    -}
    parseRows = find ((== "html") . qName . elName) . onlyElems
                >=> findChildStr "body"
                >=> classSeq [ "responsive_page_frame with_header"
                             , "responsive_page_content"
                             , "responsive_page_template_content"
                             , "pagecontent no_header"
                             ]
                >=> idSeq [ "BG_bottom"
                          , "mainContents"
                          , "tabs_basebg"
                          , "wishlist_items"
                          ]
                >=> fmap GameEntryList
                  . mapM parseRow
                  -- Work arround bug in 'xml' with space at the end.
                  . filterChildren (isDivWithAttr "class" "wishlistRow ")

    classSeq = foldr (\c r -> divChildWithClass c >=> r) pure
    idSeq = foldr (\c r -> divChildWithId c >=> r) pure
    findChildStr = findChild . unqual
    divChildWithId = divChildWithAttr "id"
    divChildWithClass = divChildWithAttr "class"
    divChildWithAttr a v = filterChild (isDivWithAttr a v)
    isDivWithAttr a v e =
        elName e == unqual "div"
        && Attr (unqual a) v `elem` elAttribs e

    parseRow e = do
        'g' : 'a' : 'm' : 'e' : '_' : appIDStr <- findAttr (unqual "id") e
        appID <- readMaybe appIDStr
        h4 <- divChildWithClass "wishlistRowItem" e >>= findChild (unqual "h4")
        pure $ GameEntry appID (strContent h4)

