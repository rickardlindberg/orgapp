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
import System.IO

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
loadBucketFrom pathToBucket = doesDirectoryExist pathToBucket >>= loadBucketWhenExists
    where
        loadBucketWhenExists False = return Nothing
        loadBucketWhenExists True  = do
            fileInfos <- getFileInfos pathToBucket
            return    $  Just $ fileInfosToBucket pathToBucket fileInfos

fileInfosToBucket :: FilePath -> [FileInfo] -> Bucket
fileInfosToBucket pathToBucket fileInfos = Bucket pathToBucket items
    where
        items = fileInfosToItems fileInfos
        fileInfosToItems fileInfo = map fileInfoToItem $ filter isBucketItem fileInfo
        fileInfoToItem (FileInfo { relativePath = path }) = BucketItem path

isBucketItem :: FileInfo -> Bool
isBucketItem fileInfo
    | hasMetaFile fileInfo = True
    | otherwise            = False

hasMetaFile :: FileInfo -> Bool
hasMetaFile FileInfo { subFiles = subFiles } = "meta.txt" `elem` subFiles

importFile :: Bucket -> FilePath -> IO Bucket
importFile bucket srcPath = do
    createDirectory itemDirectory
    createMetaFile srcPath itemDirectory
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

createMetaFile :: FilePath -> FilePath -> IO ()
createMetaFile srcPath itemDirectory = do
    openFile (itemDirectory </> "meta.txt") WriteMode >>= hClose

createItemName :: [BucketItem] -> FilePath -> String
createItemName existingItems filePath = uniqueItemName
    where
        idealItemName = takeBaseName filePath
        itemsWithSamePrefix = filter (idealItemName `isPrefixOf`) (map itemPath existingItems)
        uniqueItemName = untilUnique idealItemName 1
        isUnique name = name `notElem` (map itemPath existingItems)
        untilUnique name n | isUnique (name ++ "-" ++ (show n)) = name ++ "-" ++ (show n)
                           | otherwise     = untilUnique name (n + 1)
