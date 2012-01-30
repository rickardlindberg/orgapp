module Fixtures where

import Bucket
import Control.Exception.Base (bracket)
import System.Directory
import System.FilePath
import System.IO

withTemporaryDirectory :: (FilePath -> IO ()) -> IO ()
withTemporaryDirectory = bracket setUp tearDown
    where
        tmpDir   = "/tmp/org-app"
        setUp    = createDirectory tmpDir >> return tmpDir
        tearDown = removeDirectoryRecursive

withBucket :: ((FilePath, Bucket) -> IO ()) -> IO ()
withBucket = bracket setUp tearDown
    where
        tmpDir     = "/tmp/org-app"
        bucketPath = tmpDir </> "a-bucket"
        setUp      = do
            createDirectory tmpDir
            createBucket bucketPath
            return (tmpDir, Bucket bucketPath [])
        tearDown (tmpDir, _) = removeDirectoryRecursive tmpDir

createEmptyFile :: FilePath -> IO FilePath
createEmptyFile path =
    (createDirectoryIfMissing True (takeDirectory path)) >>
    openFile path WriteMode >>= hClose >> return path
