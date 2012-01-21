module BucketCreationTests (tests) where

import Test.HUnit
import Bucket (createBucket)
import Utils (withTemporaryDirectory)
import System.Directory

tests = test [
    "can create bucket" ~: withTemporaryDirectory $ \path -> do
        createBucket (path ++ "/a-bucket")
        exists <- (doesDirectoryExist (path ++ "/a-bucket"))
        assertBool "default bucket does not exist" exists
    ]
