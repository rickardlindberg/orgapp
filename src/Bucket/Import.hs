module Bucket.Import where

import Bucket.Types
import Control.Exception
import Data.List
import DirectoryInfo
import Meta
import System.Directory
import System.FilePath

importFile :: Bucket -> FilePath -> Meta -> IO Bucket
importFile bucket srcPath meta =
    let newItem = bucketItemFromSrc bucket srcPath meta
        newMeta = itemMeta newItem
        itemDir = itemPath newItem
    in prepareDirectory itemDir $ do
        writeMeta newMeta (itemDir </> metaFileName)
        -- TODO: copy and delete after instead to be more safe; how to test?
        renameFile srcPath (itemDir </> (metaFilename newMeta))
        return $ addItem bucket newItem

prepareDirectory :: FilePath -> IO a -> IO a
prepareDirectory path action =
    bracketOnError
        (createDirectory path >> return path)
        removeDirectoryRecursive
        (\_ -> action)

bucketItemFromSrc :: Bucket -> FilePath -> Meta -> BucketItem
bucketItemFromSrc bucket srcPath meta = BucketItem itemDirectory updatedMeta
    where
        updatedMeta   = setFilename srcFileName meta
        itemDirectory = bucketPath bucket </> itemName
        itemName      = createItemName (bucketItems bucket) srcPath
        itemFilePath  = itemDirectory </> srcFileName
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
        untilUnique name n | isUnique (name ++ "-" ++ (show n)) = name ++ "-" ++ (show n)
                           | otherwise     = untilUnique name (n + 1)
