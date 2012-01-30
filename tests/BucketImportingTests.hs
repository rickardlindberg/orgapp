module BucketImportingTests (tests) where

import Asserts
import Bucket
import Fixtures
import System.FilePath
import Test.HUnit

tests = test
    [ "importing a file moves it inside the bucket" ~: withBucket $ \((tmpDir, bucket)) -> do
        aSourceFile <- createEmptyFile $ tmpDir </> "a-file.png"
        importFile bucket aSourceFile
        aSourceFile `assertMovedTo` (bucketPath bucket </> "a-file-1" </> "a-file.png")

    , "roundtrip" ~: withBucket $ \((tmpDir, bucket)) -> do
        file1 <- createEmptyFile $ tmpDir </> "file1.png"
        file2 <- createEmptyFile $ tmpDir </> "file2.png"
        bucket <- importFile bucket file1
        bucket <- importFile bucket file2
        bucket `assertHasItems` ["file1-1", "file2-1"]

    , "an item has a unique name" ~:

        [ "when it's the only file" ~: do
            (createItemName [] "/tmp/foo.png") @?= "foo-1"

        , "when it's the third file with the same name" ~: do
            (createItemName [BucketItem "foo", BucketItem "foo-1"] "/tmp/foo.png") @?= "foo-2"

        , "when one of the existing items with same name has been deleted" ~: do
            (createItemName [BucketItem "foo", BucketItem "foo-2"] "/tmp/foo.png") @?= "foo-1"
        ]
    ]
