module TestImportFile (tests) where

import Asserts
import Bucket.Import
import Bucket.Types
import Control.Exception
import Fixtures
import Prelude hiding (catch)
import System.FilePath
import Test.HUnit

tests = test
    [ "importing a file copies it inside the bucket" ~: withBucket $ \((tmpDir, bucket)) -> do
        aSourceFile <- createEmptyFile $ tmpDir </> "a-file.png"
        importFile bucket aSourceFile
        assertFileExists (bucketPath bucket </> "a-file-1" </> "a-file.png")

    , "importing a file creates a meta.txt file" ~: withBucket $ \((tmpDir, bucket)) -> do
        aSourceFile <- createEmptyFile $ tmpDir </> "a-file.png"
        importFile bucket aSourceFile
        let metaFile = (bucketPath bucket </> "a-file-1" </> "meta.txt")
        assertFileContains metaFile "name::a-file.png\n"

    , "importing a file writes the current modification date to meta" ~: withBucket $ \((tmpDir, bucket)) -> do
        aSourceFile <- createEmptyFile $ tmpDir </> "a-file.png"
        setModificationTime aSourceFile 2012 2 15
        importFile bucket aSourceFile
        let metaFile = (bucketPath bucket </> "a-file-1" </> "meta.txt")
        assertFileContains metaFile "creationdate::2012-02-15\n"

    , "importing a file copies modification time" ~: withBucket $ \((tmpDir, bucket)) -> do
        aSourceFile <- createEmptyFile $ tmpDir </> "a-file.png"
        setModificationTime aSourceFile 2012 2 15
        mtime1 <- getMtime aSourceFile
        importFile bucket aSourceFile
        mtime2 <- getMtime (bucketPath bucket </> "a-file-1" </> "a-file.png")
        assertEqual "" mtime1 mtime2

    , "importing files updates the bucket" ~: withBucket $ \((tmpDir, bucket)) -> do
        file1 <- createEmptyFile $ tmpDir </> "file1.png"
        file2 <- createEmptyFile $ tmpDir </> "file2.png"
        bucket <- importFile bucket file1
        bucket <- importFile bucket file2
        bucket `assertHasItems` [bucketPath bucket </> "file1-1", bucketPath bucket </> "file2-1"]

    , "does not leave a trace in bucket if importing fails" ~: withBucket $ \((tmpDir, bucket)) -> do
        let importNonExistingFile = do
            importFile bucket (tmpDir </> "nonExistingFile.png")
            assertFailure "importing should have thrown IOException"
        let assertBucketHasNoTraceOfFile = \e -> do
            let _ = (e :: IOException)
            assertDirectoryDoesNotExist $ bucketPath bucket </> "nonExistingFile-1"
        catch importNonExistingFile assertBucketHasNoTraceOfFile

    , "an item has a unique name" ~:

        [ "when it's the only file" ~:
            createItemName [] "/tmp/foo.png" @?= "foo-1"

        , "when it's the third file with the same name" ~:
            createItemName [anItemWithName "foo", anItemWithName "foo-1"] "/tmp/foo.png" @?= "foo-2"

        , "when one of the existing items with same name has been deleted" ~:
            createItemName [anItemWithName "foo", anItemWithName "foo-2"] "/tmp/foo.png" @?= "foo-1"
        ]
    ]
