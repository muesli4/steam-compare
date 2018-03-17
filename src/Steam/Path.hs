{-# LANGUAGE RecordWildCards #-}
module Steam.Path
    ( pickExistingFile
    , FilePathConfig(..)
    , pickFile
    , pickXDGFile
    , pickConfigFile
    , pickDataFile
    ) where

import Control.Monad.Loops            (firstM)
import System.Directory
import System.Environment.XDG.BaseDir

pickExistingFile :: [FilePath] -> IO (Maybe FilePath)
pickExistingFile = firstM doesFileExist

data FilePathConfig
    = FilePathConfig
    { fpcOptOverride :: Maybe FilePath
    -- | This should generate a list of paths in order of preference when given
    -- the config file name.
    , fpcPathGen     :: FilePath -> IO [FilePath]
    , fpcConfigName  :: FilePath
    }

pickFile :: FilePathConfig -> IO (Either FilePath FilePath)
pickFile FilePathConfig {..} = case fpcOptOverride of
    -- Pick the override when provided. Fail without fallback, if it is not
    -- found.
    Just p  -> tryPaths p [p]
    Nothing -> do
        generatedPaths <- fpcPathGen fpcConfigName
        let ps = generatedPaths ++ [fpcConfigName]
        tryPaths (head ps) ps
  where
    tryPaths reportedPath ps = maybe (Left reportedPath) Right <$> pickExistingFile ps

pickXDGFile :: (String -> FilePath -> IO [FilePath]) -> Maybe FilePath -> String -> FilePath -> IO (Either FilePath FilePath)
pickXDGFile getFiles op app p = pickFile FilePathConfig {..}
  where
    fpcOptOverride = op
    fpcPathGen     = getFiles app
    fpcConfigName  = p

pickConfigFile, pickDataFile :: Maybe FilePath -> String -> FilePath -> IO (Either FilePath FilePath)
pickConfigFile = pickXDGFile getAllConfigFiles
pickDataFile = pickXDGFile getAllDataFiles

