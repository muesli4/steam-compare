-- TODO rename and move module, combines fetching and inserting
module Steam.Update where

import Database.HDBC

import Steam.Core.Database
import Steam.Core.Fetch
import Steam.Core.Types    (SteamID(..))

updateGames :: IConnection c => c -> IO (Either String ())
updateGames c = do
    optGEL <- fetchGameEntryList
    case optGEL of
        Right gel -> Right <$> insertGames c gel
        Left e    -> return $ Left e

updateOwnedGames :: IConnection c => c -> SteamID -> IO (Either String ())
updateOwnedGames c sid = do
    optOGL <- fetchOwnedGameListXML sid
    case optOGL of
        Right ogl -> Right <$> insertOwnedGames c ogl
        Left e    -> return $ Left e
