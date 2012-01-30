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
} deriving (Eq, Show)

createBucket :: FilePath -> IO Bucket
createBucket path = do
    createDirectory path
    return $ Bucket path []

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

importFile :: Bucket -> FilePath -> IO Bucket
importFile bucket srcPath = do
    createDirectory itemDirectory
    renameFile srcPath itemPath
    return $ extendBucketWith itemName
    where
        itemDirectory             = bucketPath bucket </> itemName
        itemName                  = createItemName (bucketItems bucket) srcPath
        itemPath                  = itemDirectory </> srcFileName
        srcFileName               = takeFileName srcPath
        extendBucketWith itemName = bucket {
            bucketItems = (BucketItem itemName):(bucketItems bucket)
        }

createItemName :: [BucketItem] -> FilePath -> String
createItemName existingItems filePath = uniqueItemName
    where
        idealItemName = takeBaseName filePath
        itemsWithSamePrefix = filter (idealItemName `isPrefixOf`) (map itemPath existingItems)
        uniqueItemName = untilUnique idealItemName 1
        isUnique name = name `notElem` (map itemPath existingItems)
        untilUnique name n | isUnique (name ++ "-" ++ (show n)) = name ++ "-" ++ (show n)
                           | otherwise     = untilUnique name (n + 1)
