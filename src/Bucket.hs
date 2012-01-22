module Bucket
    ( createBucket
    , loadBucketFrom
    , importFile
    ) where

import System.Directory
import System.FilePath

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

importFile :: FilePath -> FilePath -> IO ()
importFile bucketPath filePath = do
    let sourceFileName = takeFileName filePath
    let itemDirectory = bucketPath </> (createItemName filePath)
    createDirectory itemDirectory
    renameFile filePath (itemDirectory </> sourceFileName)

createItemName :: FilePath -> String
createItemName filePath = takeBaseName filePath
