module Bucket
    ( createBucket
    , loadBucketFrom
    , importFile
    , createItemName
    ) where

import Data.List
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
    existingItems <- loadBucketFrom bucketPath
    let sourceFileName = takeFileName filePath
    let itemDirectory = bucketPath </> (createItemName existingItems filePath)
    createDirectory itemDirectory
    renameFile filePath (itemDirectory </> sourceFileName)

createItemName :: [String] -> FilePath -> String
createItemName existingItems filePath = uniqueItemName
    where
        idealItemName = takeBaseName filePath
        itemsWithSamePrefix = filter (idealItemName `isPrefixOf`) existingItems
        uniqueItemName = untilUnique idealItemName 1
        isUnique name = name `notElem` existingItems
        untilUnique name n | isUnique (name ++ "-" ++ (show n)) = name ++ "-" ++ (show n)
                           | otherwise     = untilUnique name (n + 1)
