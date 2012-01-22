module BucketLoadingTests (tests) where

import Asserts
import Bucket (createBucket, loadBucketFrom)
import Fixtures
import System.Directory
import System.FilePath
import Test.HUnit

tests = test
    [ "return empty file list when no bucket exist" ~: do
        filesInBucket <- loadBucketFrom "/a/path"
        filesInBucket `shouldBeSameAs` []

    , "return empty file list when bucket is empty" ~: withTemporaryDirectory $ \tmpDir -> do
        let bucketPath = tmpDir </> "bucket"
        givenABucketAt bucketPath
        filesInBucket <- whenLoadingBucketFrom bucketPath
        filesInBucket `shouldBeSameAs` []

    , "return files in bucket" ~: withTemporaryDirectory $ \tmpDir -> do
        let bucketPath = tmpDir </> "bucket"
        givenABucketAt bucketPath
        givenFilesInBucketAt bucketPath ["oneFile", "anotherFile"]
        filesInBucket <- whenLoadingBucketFrom bucketPath
        filesInBucket `shouldBeSameAs` ["oneFile", "anotherFile"]
    ]

givenABucketAt = createBucket

givenFilesInBucketAt path fileNames = do
    mapM (\fileName -> createDirectory $ path </> fileName) fileNames

whenLoadingBucketFrom = loadBucketFrom
