module BucketLoader
    ( loadBucketFrom
    , createBucket
    ) where

import System.Directory

createBucket :: FilePath -> IO ()
createBucket = createDirectory

loadBucketFrom :: String -> IO [String]
loadBucketFrom pathToBucket = do
    exists <- doesDirectoryExist pathToBucket
    case exists of
        True -> do
            files <- getDirectoryContents pathToBucket
            return $ filter isBucketFile files
        False -> return []

isBucketFile "." = False
isBucketFile ".." = False
isBucketFile _ = True
