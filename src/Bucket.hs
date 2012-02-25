module Bucket
    ( createBucket
    , loadBucketFrom
    , importFile
    , createItemName
    , Bucket(..)
    , addItem
    , BucketItem(..)
    ) where

import Control.Exception
import Data.List
import DirectoryInfo
import Meta
import System.Directory
import System.FilePath
import System.IO

metaFileName = "meta.txt"

data Bucket = Bucket {
    bucketPath  :: FilePath,
    bucketItems :: [BucketItem]
} deriving (Eq, Show)

addItem :: Bucket -> BucketItem -> Bucket
addItem bucket item = bucket { bucketItems = item:(bucketItems bucket) }

data BucketItem = BucketItem {
    itemPath :: FilePath,
    itemMeta :: Meta
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
            directories <- getDirectoryInfoRecursive pathToBucket
            return $ Just $ directoriesToBucket pathToBucket directories

directoriesToBucket :: FilePath -> [DirectoryInfo] -> Bucket
directoriesToBucket pathToBucket directories = Bucket pathToBucket items
    where
        items = directoriesToItems directories
        directoriesToItems directoryInfo = map directoryToItem $ filter isBucketItem directoryInfo
        -- TODO: parse meta from file
        directoryToItem (DirectoryInfo { path = path }) = BucketItem path createMeta

isBucketItem :: DirectoryInfo -> Bool
isBucketItem directoryInfo
    | hasMetaFile directoryInfo = True
    | otherwise                 = False

hasMetaFile :: DirectoryInfo -> Bool
hasMetaFile DirectoryInfo { files = files } = metaFileName `elem` files

importFile :: Bucket -> FilePath -> Meta -> IO Bucket
importFile bucket srcPath meta =
    let newItem = bucketItemFrom bucket srcPath meta
        newMeta = itemMeta newItem
        itemDir = itemPath newItem
    in prepareDirectory itemDir $ do
        writeMeta newMeta (itemDir </> metaFileName)
        -- TODO: copy and delete after instead to be more safe; how to test?
        renameFile srcPath (itemDir </> (filename newMeta))
        return $ addItem bucket newItem

bucketItemFrom :: Bucket -> FilePath -> Meta -> BucketItem
bucketItemFrom bucket srcPath meta = BucketItem itemDirectory updatedMeta
    where
        updatedMeta   = setFilename srcFileName meta
        itemDirectory = bucketPath bucket </> itemName
        itemName      = createItemName (bucketItems bucket) srcPath
        itemFilePath  = itemDirectory </> srcFileName
        srcFileName   = takeFileName srcPath

prepareDirectory :: FilePath -> IO a -> IO a
prepareDirectory path action =
    bracketOnError
        (createDirectory path >> return path)
        removeDirectoryRecursive
        (\_ -> action)

createItemName :: [BucketItem] -> FilePath -> String
createItemName existingItems filePath = uniqueItemName
    where
        idealItemName = takeBaseName filePath
        itemNames = map (takeFileName . itemPath) existingItems
        itemsWithSamePrefix = filter (idealItemName `isPrefixOf`) itemNames
        uniqueItemName = untilUnique idealItemName 1
        isUnique name = name `notElem` itemNames
        untilUnique name n | isUnique (name ++ "-" ++ (show n)) = name ++ "-" ++ (show n)
                           | otherwise     = untilUnique name (n + 1)
