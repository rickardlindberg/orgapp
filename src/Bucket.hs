module Bucket
    ( createBucket
    , loadBucketFrom
    , importFile
    , createItemName
    , Bucket(..)
    , BucketItem(..)
    ) where

import Data.List
import FileInfo
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

loadBucketFrom :: FilePath -> IO (Maybe Bucket)
loadBucketFrom pathToBucket = doesDirectoryExist pathToBucket >>= loadBucketIfExists
    where
        loadBucketIfExists False = return Nothing
        loadBucketIfExists True  = do
            fileInfos <- getFileInfos pathToBucket
            return $ Just $ Bucket pathToBucket (fileInfosToItems fileInfos)
        fileInfosToItems x = map fileInfoToItem (filter isBucketItem x)
        fileInfoToItem (FileInfo path _) = BucketItem path

isBucketItem :: FileInfo -> Bool
isBucketItem (FileInfo "."  _    ) = False
isBucketItem (FileInfo ".." _    ) = False
isBucketItem (FileInfo _    isDir) = isDir

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
