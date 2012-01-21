module BucketLoaderTests
    ( bucketLoadingTests
    ) where

import Test.HUnit
import BucketLoader (loadBucketFrom)
import Utils (withTemporaryDirectory)
import BucketLoader (createBucket)
import System.Directory

givenABucketAt = createBucket
givenFilesInBucketAt path = do
    createDirectory $ path ++ "/oneFile"
    createDirectory $ path ++ "/anotherFile"

whenLoadingBucketFrom = loadBucketFrom
thenOneFileShouldBeReturned filesInBucket = assertEqual "" ["oneFile", "anotherFile"] filesInBucket

bucketLoadingTests = test [

    "return empty file list when no bucket exist" ~: do
        filesInBucket <- loadBucketFrom "/a/path"
        assertEqual "Should be empty" [] filesInBucket

    , "return files in bucket" ~: withTemporaryDirectory $ \path -> do
        givenABucketAt $ path ++ "/bucket"
        givenFilesInBucketAt $ path ++ "/bucket"
        filesInBucket <- whenLoadingBucketFrom $ path ++ "/bucket"
        thenOneFileShouldBeReturned filesInBucket
    ]
