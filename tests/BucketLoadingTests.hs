module BucketLoadingTests (tests) where

import Asserts
import Bucket
import Fixtures
import System.Directory
import System.FilePath
import Test.HUnit

tests = test
    [ "return empty file list when no bucket exist" ~: do
        bucket <- loadBucketFrom "/a/path"
        bucketPath bucket @?= "/a/path"
        bucket `assertHasItems` []

    , "return empty file list when bucket is empty" ~: withTemporaryDirectory $ \tmpDir -> do
        let bucketPath = tmpDir </> "bucket"
        createBucket bucketPath
        bucket <- loadBucketFrom bucketPath
        bucket `assertHasItems` []

    , "return files in bucket" ~: withTemporaryDirectory $ \tmpDir -> do
        let bucketPath = tmpDir </> "bucket"
        bucket <- createBucket bucketPath
        givenFilesInBucketAt bucketPath ["oneFile", "anotherFile"]
        bucket <- loadBucketFrom bucketPath
        bucket `assertHasItems` ["oneFile", "anotherFile"]

    , "roundtrip" ~: withBucket $ \((tmpDir, bucket)) -> do
        file1 <- createEmptyFile $ tmpDir </> "file1.png"
        file2 <- createEmptyFile $ tmpDir </> "file2.png"
        bucket <- importFile bucket file1
        bucket <- importFile bucket file2
        bucket `assertHasItems` ["file1-1", "file2-1"]
    ]

givenFilesInBucketAt path fileNames = do
    mapM (\fileName -> createDirectory $ path </> fileName) fileNames
