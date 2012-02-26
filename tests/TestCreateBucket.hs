module TestCreateBucket (tests) where

import Asserts
import Bucket.Load
import Bucket.Types
import Fixtures
import System.FilePath
import Test.HUnit

tests = test
    [ "has directory for new bucket" ~: withBucket $ \((tmpDir, bucket)) ->
        assertDirectoryExists (bucketPath bucket)

    , "has no items to begin with" ~: withBucket $ \((tmpDir, bucket)) ->
        bucketItems bucket @?= []
    ]
