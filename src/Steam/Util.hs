module Steam.Util
    ( handleEither
    , putErrStrLn
    ) where

import           Database.HDBC
import           System.IO

putErrStrLn :: String -> IO ()
putErrStrLn e = hPutStr stderr e >> putChar '\n'

handleEither :: IConnection c => c -> (String -> IO ()) -> IO () -> Either String b -> IO ()
handleEither c failAct successAct r = case r of
    Left e  -> rollback c >> failAct ("Update failed: " ++ e)
    Right _ -> putStrLn "Update succesful." >> successAct

