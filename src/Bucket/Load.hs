module Bucket.Load where

import Bucket.Types
import DirectoryInfo
import Meta
import System.Directory

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
        directoriesToItems = map directoryToItem . filter hasMetaFile
        directoryToItem (DirectoryInfo { path = path, meta = Just meta }) = BucketItem path (metaFromStr meta)
