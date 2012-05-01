module TestLoadBucket (tests) where

import Asserts
import Bucket.Import
import Bucket.Load
import Bucket.Types
import Data.Maybe
import Fixtures
import System.FilePath
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.HUnit

tests = describe "loading bucket" $ do

    it "loading non-existent bucket returns nothing" $ do
        bucket <- loadBucketFrom "/a/non/existing/path"
        assertBool "" (isNothing bucket)

    describe "loading empty bucket" $ do

        it "returns bucket with path" $ withBucket $ \((tmpDir, bucket)) -> do
            Just loadedBucket <- loadBucketFrom $ bucketPath bucket
            bucketPath loadedBucket @?= bucketPath bucket

        it "returns bucket with no items" $ withBucket $ \((tmpDir, bucket)) -> do
            Just loadedBucket <- loadBucketFrom $ bucketPath bucket
            loadedBucket `assertHasItems` []

    describe "loading non-empty bucket" $ do

        it "loads all items" $ withBucket $ \((tmpDir, bucket)) -> do
            createItemAt (bucketPath bucket </> "one-item")     "item1.png"
            createItemAt (bucketPath bucket </> "another-item") "item2.png"
            Just loadedBucket <- loadBucketFrom (bucketPath bucket)
            loadedBucket `assertHasItems` [bucketPath bucket </> "one-item", bucketPath bucket </> "another-item"]

        it "loads items in subdirectories" $ withBucket $ \((tmpDir, bucket)) -> do
            createItemAt (bucketPath bucket </> "subdir" </> "one-item") "item1.png"
            Just loadedBucket <- loadBucketFrom (bucketPath bucket)
            loadedBucket `assertHasItems` [bucketPath bucket </> "subdir" </> "one-item"]

        it "skips files which are not items" $ withBucket $ \((tmpDir, bucket)) -> do
            createEmptyFile (bucketPath bucket </> "not-an-item.png")
            Just loadedBucket <- loadBucketFrom (bucketPath bucket)
            loadedBucket `assertHasItems` []

        it "loads meta back" $ withBucket $ \((tmpDir, bucket)) -> do
            file <- createEmptyFile $ tmpDir </> "file1.png"
            importFile bucket file
            Just loadedBucket <- loadBucketFrom (bucketPath bucket)
            let item = head (bucketItems loadedBucket)
            fileName item @?= "file1.png"
