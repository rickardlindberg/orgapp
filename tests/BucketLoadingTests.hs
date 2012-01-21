module BucketLoadingTests (tests) where

import Bucket (createBucket, loadBucketFrom)
import qualified Data.Set as Set
import System.Directory
import System.FilePath
import Test.HUnit
import Utils (withTemporaryDirectory)

tests = test
    [ "return empty file list when no bucket exist" ~: do
        filesInBucket <- loadBucketFrom "/a/path"
        assertEqual "Should be empty" [] filesInBucket

    , "return files in bucket" ~: withTemporaryDirectory $ \tmpDir -> do
        let bucketPath = tmpDir </> "bucket"
        givenABucketAt bucketPath
        givenFilesInBucketAt bucketPath
        filesInBucket <- whenLoadingBucketFrom bucketPath
        thenOneFileShouldBeReturned filesInBucket
    ]

givenABucketAt = createBucket

givenFilesInBucketAt path = do
    createDirectory $ path </> "oneFile"
    createDirectory $ path </> "anotherFile"

whenLoadingBucketFrom = loadBucketFrom

thenOneFileShouldBeReturned filesInBucket = assertEqual "" (Set.fromList ["oneFile", "anotherFile"]) (Set.fromList filesInBucket)
