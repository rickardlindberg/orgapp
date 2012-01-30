module BucketCreationTests (tests) where

import Asserts
import Bucket
import Fixtures
import System.FilePath
import Test.HUnit

tests = test
    [ "has directory for new bucket" ~: withBucket $ \((tmpDir, bucket)) -> do
        assertDirectoryExists (bucketPath bucket)

    , "has no items to begin with" ~: withBucket $ \((tmpDir, bucket)) -> do
        (bucketItems bucket) @?= []
    ]
