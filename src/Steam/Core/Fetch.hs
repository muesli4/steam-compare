module Steam.Core.Fetch where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.List
import           Data.Maybe
import           Network.Download
import           Text.Read
import           Text.XML.Light

import           Steam.Core.Types
import           Steam.Core.URL

{-
import           Text.XML.Light.Extractors
import Prelude hiding (div)
import Control.Applicative
-}

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


{-
htmlBody :: ContentsExtractor a -> ContentsExtractor a
htmlBody = element "html" . children . element "body" . children

div :: ElementExtractor a -> ContentsExtractor a
div = element "div"

divChildWithKV :: String -> String -> ContentsExtractor a -> ContentsExtractor a
divChildWithKV key expectedValue ce = div $ do
    actualValue <- attrib key
    guard $ actualValue == expectedValue
    children ce

divChildWithClass, divChildWithId :: String -> ContentsExtractor a -> ContentsExtractor a
divChildWithClass = divChildWithKV "class"
divChildWithId = divChildWithKV "id"

nestedWithin
    :: [ContentsExtractor a -> ContentsExtractor a]
    -> ContentsExtractor a
    -> ContentsExtractor a
nestedWithin = flip $ foldr ($)


wishlistCE :: ContentsExtractor GameEntryList
wishlistCE =
    htmlBody $ nestedWithin (fmap divChildWithClass cs ++ fmap divChildWithId ids)
             $ fmap GameEntryList
             $ many
             $ div $ do
                appId <- attribAs "id" $ \s -> do
                    let (p, appIdStr) = splitAt 5 s
                    guard $ p == "game_"
                    integer appIdStr
                name <- children $ divChildWithClass "wishlistRowItem" $ element "h4" $ children text
                pure $ GameEntry appId name
  where
    cs  = [ "responsive_page_frame with_header"
          , "responsive_page_content"
          , "responsive_page_template_content"
          , "pagecontent no_header"
          ]
    ids = [ "BG_bottom"
          , "mainContents"
          , "tabs_basebg"
          , "wishlist_items"
          ]
-}
