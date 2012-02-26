module TestImportFile (tests) where

import Asserts
import Bucket
import Control.Exception
import Fixtures
import Meta
import Prelude hiding (catch)
import System.FilePath
import Test.HUnit

tests = test
    [ "importing a file moves it inside the bucket" ~: withBucket $ \((tmpDir, bucket)) -> do
        aSourceFile <- createEmptyFile $ tmpDir </> "a-file.png"
        importFile bucket aSourceFile createMeta
        aSourceFile `assertMovedTo` (bucketPath bucket </> "a-file-1" </> "a-file.png")

    , "importing a file creates a meta.txt file" ~: withBucket $ \((tmpDir, bucket)) -> do
        aSourceFile <- createEmptyFile $ tmpDir </> "a-file.png"
        importFile bucket aSourceFile createMeta
        let metaFile = (bucketPath bucket </> "a-file-1" </> "meta.txt")
        assertFileContains metaFile "name::a-file.png\n"

    , "importing files updates the bucket" ~: withBucket $ \((tmpDir, bucket)) -> do
        file1 <- createEmptyFile $ tmpDir </> "file1.png"
        file2 <- createEmptyFile $ tmpDir </> "file2.png"
        bucket <- importFile bucket file1 createMeta
        bucket <- importFile bucket file2 createMeta
        bucket `assertHasItems` [bucketPath bucket </> "file1-1", bucketPath bucket </> "file2-1"]

    , "does not leave a trace in bucket if importing fails" ~: withBucket $ \((tmpDir, bucket)) -> do
        let importNonExistingFile = do
            importFile bucket (tmpDir </> "nonExistingFile.png") createMeta
            assertFailure "importing should have thrown IOException"
        let assertBucketHasNoTraceOfFile = \e -> do
            let _ = (e :: IOException)
            assertDirectoryDoesNotExist $ (bucketPath bucket) </> "nonExistingFile-1"
        catch importNonExistingFile assertBucketHasNoTraceOfFile

    , "an item has a unique name" ~:

        [ "when it's the only file" ~: do
            (createItemName [] "/tmp/foo.png") @?= "foo-1"

        , "when it's the third file with the same name" ~: do
            (createItemName [anItemWithName "foo", anItemWithName "foo-1"] "/tmp/foo.png") @?= "foo-2"

        , "when one of the existing items with same name has been deleted" ~: do
            (createItemName [anItemWithName "foo", anItemWithName "foo-2"] "/tmp/foo.png") @?= "foo-1"
        ]
    ]
