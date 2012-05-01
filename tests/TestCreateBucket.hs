module TestCreateBucket (tests) where

import Asserts
import Bucket.Types
import Fixtures
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.HUnit

tests = describe "creating bucket" $ do

    it "has directory for new bucket" $ withBucket $ \((tmpDir, bucket)) ->
        assertDirectoryExists (bucketPath bucket)

    it "has no items to begin with" $ withBucket $ \((tmpDir, bucket)) ->
        bucketItems bucket @?= []
