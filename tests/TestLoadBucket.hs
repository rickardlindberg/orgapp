module TestLoadBucket (tests) where

import Asserts
import Bucket
import Data.Maybe
import Fixtures
import Meta
import System.Directory
import System.FilePath
import Test.HUnit

tests = test
    [ "loading non-existent bucket returns nothing" ~: do
        bucket <- loadBucketFrom "/a/non/existing/path"
        assertBool "" (isNothing bucket)

    , "loading empty bucket" ~:

        [ "returns bucket with path" ~: withBucket $ \((tmpDir, bucket)) -> do
            Just loadedBucket <- loadBucketFrom $ bucketPath bucket
            (bucketPath loadedBucket) @?= (bucketPath bucket)

        , "returns bucket with no items" ~: withBucket $ \((tmpDir, bucket)) -> do
            Just loadedBucket <- loadBucketFrom $ bucketPath bucket
            loadedBucket `assertHasItems` []
        ]

    , "loading non-empty bucket" ~:

        [ "loads all items" ~: withBucket $ \((tmpDir, bucket)) -> do
            createItemAt (bucketPath bucket </> "one-item")     "item1.png"
            createItemAt (bucketPath bucket </> "another-item") "item2.png"
            Just loadedBucket <- loadBucketFrom (bucketPath bucket)
            loadedBucket `assertHasItems` [bucketPath bucket </> "one-item", bucketPath bucket </> "another-item"]

        , "loads items in subdirectories" ~: withBucket $ \((tmpDir, bucket)) -> do
            createItemAt (bucketPath bucket </> "subdir" </> "one-item") "item1.png"
            Just loadedBucket <- loadBucketFrom (bucketPath bucket)
            loadedBucket `assertHasItems` [bucketPath bucket </> "subdir" </> "one-item"]

        , "skips files which are not items" ~: withBucket $ \((tmpDir, bucket)) -> do
            createEmptyFile (bucketPath bucket </> "not-an-item.png")
            Just loadedBucket <- loadBucketFrom (bucketPath bucket)
            loadedBucket `assertHasItems` []

        , "loads meta back" ~: withBucket $ \((tmpDir, bucket)) -> do
            file <- createEmptyFile $ tmpDir </> "file1.png"
            importFile bucket file createMeta
            Just loadedBucket <- loadBucketFrom (bucketPath bucket)
            let item = head (bucketItems loadedBucket)
            metaFilename (itemMeta item) @?= "file1.png"
        ]
    ]
