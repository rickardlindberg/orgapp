module Fixtures where

import Bucket
import Control.Exception.Base (bracket)
import System.Directory
import System.FilePath
import System.IO

withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory = bracket setUp tearDown
    where
        tmpDir   = "/tmp/org-app"
        setUp    = createDirectory tmpDir >> return tmpDir
        tearDown = removeDirectoryRecursive

withBucket :: ((FilePath, Bucket) -> IO a) -> IO a
withBucket = bracket setUp tearDown
    where
        tmpDir     = "/tmp/org-app"
        bucketPath = tmpDir </> "a-bucket"
        setUp      = do
            createDirectory tmpDir
            bucket <- createBucket bucketPath
            return (tmpDir, bucket)
        tearDown (tmpDir, _) = removeDirectoryRecursive tmpDir

createEmptyFile :: FilePath -> IO FilePath
createEmptyFile path =
    (createDirectoryIfMissing True (takeDirectory path)) >>
    openFile path WriteMode >>= hClose >> return path

createItemAt :: FilePath -> FilePath -> IO FilePath
createItemAt path name = do
    createEmptyFile (path </> name)
    createEmptyFile (path </> "meta.txt")

anItemWithName :: String -> BucketItem
anItemWithName name = BucketItem ("/tmp/" ++ name)
