module Bucket.Import where

import Bucket.Types
import Control.Exception
import Data.List
import DirectoryInfo
import Meta
import System.Directory
import System.FilePath

importFile :: Bucket -> FilePath -> IO Bucket
importFile bucket srcPath =
    let newItem = bucketItemFromSrc bucket srcPath createMeta
        newMeta = itemMeta newItem
        itemDir = itemPath newItem
    in prepareDirectory itemDir $ do
        writeMeta newMeta (itemDir </> metaFileName)
        copyFile srcPath (itemDir </> fileName newItem)
        return $ addItem bucket newItem

prepareDirectory :: FilePath -> IO a -> IO a
prepareDirectory path action =
    bracketOnError
        (createDirectory path >> return path)
        removeDirectoryRecursive
        (\_ -> action)

bucketItemFromSrc :: Bucket -> FilePath -> Meta -> BucketItem
bucketItemFromSrc bucket srcPath meta = setFileName (BucketItem itemDirectory meta) srcFileName
    where
        itemDirectory = bucketPath bucket </> itemName
        itemName      = createItemName (bucketItems bucket) srcPath
        srcFileName   = takeFileName srcPath

createItemName :: [BucketItem] -> FilePath -> String
createItemName existingItems filePath = uniqueItemName
    where
        idealItemName = takeBaseName filePath
        -- TODO: take into consideration in which subdirectory the item is in
        itemNames = map (takeFileName . itemPath) existingItems
        itemsWithSamePrefix = filter (idealItemName `isPrefixOf`) itemNames
        uniqueItemName = untilUnique idealItemName 1
        isUnique name = name `notElem` itemNames
        untilUnique name n | isUnique (name ++ "-" ++ show n) = name ++ "-" ++ show n
                           | otherwise     = untilUnique name (n + 1)
