module Utils where

import Bucket (createBucket)
import Control.Exception.Base (bracket)
import System.Directory
import System.FilePath
import System.IO
import Test.HUnit

withTemporaryDirectory :: (FilePath -> IO ()) -> IO ()
withTemporaryDirectory = bracket setUp tearDown
    where
        tmpDir   = "/tmp/org-app"
        setUp    = createDirectory tmpDir >> return tmpDir
        tearDown = removeDirectoryRecursive

withBucket :: ((FilePath, FilePath) -> IO ()) -> IO ()
withBucket = bracket setUp tearDown
    where
        tmpDir   = "/tmp/org-app"
        bucketPath = tmpDir </> "a-bucket"
        setUp    = do
            createDirectory tmpDir
            createBucket bucketPath
            return (tmpDir, bucketPath)
        tearDown (tmpDir, bucketPath) = removeDirectoryRecursive tmpDir

assertMovedTo src dest = do
    assertFileExists dest
    assertFileDoesNotExist src

createEmptyFile path = openFile path WriteMode >>= hClose

assertFileDoesNotExist file = doesFileExist file >>= \exists -> assertBool "file does exist" (not exists)

assertFileExists file = doesFileExist file >>= assertBool "file does not exist"

assertDirectoryExists dir = do
    exists <- doesDirectoryExist dir
    assertBool "directory does not exist" exists
