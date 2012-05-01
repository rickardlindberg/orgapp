module TestBucket (tests) where

import Bucket.Types
import Fixtures()
import Meta
import qualified Data.Map as M
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck

tests = describe "Bucket" $ do

    it "can get path to item file" $ do
        let item = setFileName "bar.png" (BucketItem "a/path" createMeta)
        filePath item @?= "a/path/bar.png"

    prop "adding item makes bucket bigger" $ \(bucket, item) ->
        let newBucket = addItem bucket item
            newSize   = length $ bucketItems newBucket
            oldSize   = length $ bucketItems bucket
        in M.notMember (itemPath item) (bucketItemsMap bucket) ==> newSize == oldSize + 1
