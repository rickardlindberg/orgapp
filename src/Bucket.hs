module Bucket
    ( createBucket
    , loadBucketFrom
    , importFile
    , createItemName
    , Bucket(..)
    , BucketItem(..)
    ) where

import Data.List
import System.Directory
import System.FilePath

data Bucket = Bucket {
    bucketPath  :: FilePath,
    bucketItems :: [BucketItem]
} deriving (Show)

data BucketItem = BucketItem {
    itemPath :: FilePath
} deriving (Show)

createBucket :: FilePath -> IO ()
createBucket = createDirectory

loadBucketFrom :: FilePath -> IO Bucket
loadBucketFrom pathToBucket = do
    exists <- doesDirectoryExist pathToBucket
    case exists of
        True -> do
            files <- getDirectoryContents pathToBucket
            return $ Bucket pathToBucket (map BucketItem (filter isBucketFile files))
        False -> return $ Bucket pathToBucket []

isBucketFile "." = False
isBucketFile ".." = False
isBucketFile _ = True

importFile :: FilePath -> FilePath -> IO ()
importFile bucketPath filePath = do
    existingItems <- loadBucketFrom bucketPath
    let sourceFileName = takeFileName filePath
    let itemDirectory = bucketPath </> (createItemName (bucketItems existingItems) filePath)
    createDirectory itemDirectory
    renameFile filePath (itemDirectory </> sourceFileName)

createItemName :: [BucketItem] -> FilePath -> String
createItemName existingItems filePath = uniqueItemName
    where
        idealItemName = takeBaseName filePath
        itemsWithSamePrefix = filter (idealItemName `isPrefixOf`) (map itemPath existingItems)
        uniqueItemName = untilUnique idealItemName 1
        isUnique name = name `notElem` (map itemPath existingItems)
        untilUnique name n | isUnique (name ++ "-" ++ (show n)) = name ++ "-" ++ (show n)
                           | otherwise     = untilUnique name (n + 1)
