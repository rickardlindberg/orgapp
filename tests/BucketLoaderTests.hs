module BucketLoaderTests
    ( bucketLoadingTests
    ) where

import Test.HUnit
import Bucket (createBucket, loadBucketFrom)
import Utils (withTemporaryDirectory)
import System.Directory
import qualified Data.Set as Set

givenABucketAt = createBucket
givenFilesInBucketAt path = do
    createDirectory $ path ++ "/oneFile"
    createDirectory $ path ++ "/anotherFile"

whenLoadingBucketFrom = loadBucketFrom
thenOneFileShouldBeReturned filesInBucket = assertEqual "" (Set.fromList ["oneFile", "anotherFile"]) (Set.fromList filesInBucket)

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
