module BucketCreationTests (tests) where

import Asserts
import Bucket
import Fixtures
import System.FilePath
import Test.HUnit

tests = test
    [ "has directory for new bucket" ~: withBucket $ \((tmpDir, bucket)) -> do
        assertDirectoryExists (bucketPath bucket)

    , "importing a file moves it inside the bucket" ~: withBucket $ \((tmpDir, bucket)) -> do
        aSourceFile <- createEmptyFile $ tmpDir </> "a-file.png"
        importFile (bucketPath bucket) aSourceFile
        aSourceFile `assertMovedTo` (bucketPath bucket </> "a-file-1" </> "a-file.png")

    , "an item has a unique name" ~:

        [ "when it's the only file" ~: do
            (createItemName [] "/tmp/foo.png") @?= "foo-1"

        , "when it's the third file with the same name" ~: do
            (createItemName [BucketItem "foo", BucketItem "foo-1"] "/tmp/foo.png") @?= "foo-2"

        , "when one of the existing items with same name has been deleted" ~: do
            (createItemName [BucketItem "foo", BucketItem "foo-2"] "/tmp/foo.png") @?= "foo-1"
        ]
    ]
