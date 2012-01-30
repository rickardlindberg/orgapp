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
        (map itemPath (bucketItems bucket)) `shouldBeSameAs` []

    , "return empty file list when bucket is empty" ~: withTemporaryDirectory $ \tmpDir -> do
        let bucketPath = tmpDir </> "bucket"
        givenABucketAt bucketPath
        bucket <- whenLoadingBucketFrom bucketPath
        (map itemPath (bucketItems bucket)) `shouldBeSameAs` []

    , "return files in bucket" ~: withTemporaryDirectory $ \tmpDir -> do
        let bucketPath = tmpDir </> "bucket"
        givenABucketAt bucketPath
        givenFilesInBucketAt bucketPath ["oneFile", "anotherFile"]
        bucket <- whenLoadingBucketFrom bucketPath
        (map itemPath (bucketItems bucket)) `shouldBeSameAs` ["oneFile", "anotherFile"]
    ]

givenABucketAt = createBucket

givenFilesInBucketAt path fileNames = do
    mapM (\fileName -> createDirectory $ path </> fileName) fileNames

whenLoadingBucketFrom = loadBucketFrom
