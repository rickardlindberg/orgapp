module BucketCreationTests (tests) where

import Bucket (createBucket)
import System.Directory
import System.FilePath
import Test.HUnit
import Utils (withTemporaryDirectory)

tests = test
    [ "can create bucket" ~: withTemporaryDirectory $ \tmpDir -> do
        let bucketPath = tmpDir </> "a-bucket"
        createBucket bucketPath
        assertDirectoryExists bucketPath
    ]

assertDirectoryExists dir = do
    exists <- doesDirectoryExist dir
    assertBool "directory does not exist" exists
