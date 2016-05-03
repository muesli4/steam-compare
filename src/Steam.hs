module Steam where

import Steam.Database
import Steam.Fetch

updateGames :: IConnection c => c -> IO (Either String ())
updateGames c = do
    optGEL <- fetchGameEntryList
    case optGEL of
        Right gel -> insertGames c gel
        Left e    -> Left e

updateOwnedGames :: IConnection c => c -> SteamID -> IO (Either String ())
updateOwnedGames c sid = do
    optOGL <- fetchOwnedGameListXML sid
    case optOGL of
        Right ogl -> insertOwnedGames ogl
        Left e    -> Left e
