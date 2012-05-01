module TestImportFile (tests) where

import Asserts
import Bucket.Import
import Bucket.Types
import Control.Exception
import Fixtures
import Prelude hiding (catch)
import System.FilePath
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck

tests = describe "importing a file" $ do

    it "copies it inside the bucket" $ withBucket $ \((tmpDir, bucket)) -> do
        aSourceFile <- createEmptyFile $ tmpDir </> "a-file.png"
        importFile bucket aSourceFile
        assertFileExists (bucketPath bucket </> "a-file-1" </> "a-file.png")

    it "creates a meta.txt file" $ withBucket $ \((tmpDir, bucket)) -> do
        aSourceFile <- createEmptyFile $ tmpDir </> "a-file.png"
        importFile bucket aSourceFile
        let metaFile = (bucketPath bucket </> "a-file-1" </> "meta.txt")
        assertFileContains metaFile "name::a-file.png\n"

    it "writes the current modification date to meta" $ withBucket $ \((tmpDir, bucket)) -> do
        aSourceFile <- createEmptyFile $ tmpDir </> "a-file.png"
        setModificationTime aSourceFile 2012 2 15
        importFile bucket aSourceFile
        let metaFile = (bucketPath bucket </> "a-file-1" </> "meta.txt")
        assertFileContains metaFile "creationdate::2012-02-15\n"

    it "copies modification time" $ withBucket $ \((tmpDir, bucket)) -> do
        aSourceFile <- createEmptyFile $ tmpDir </> "a-file.png"
        setModificationTime aSourceFile 2012 2 15
        mtime1 <- getMtime aSourceFile
        importFile bucket aSourceFile
        mtime2 <- getMtime (bucketPath bucket </> "a-file-1" </> "a-file.png")
        assertEqual "" mtime1 mtime2

    it "updates the bucket" $ withBucket $ \((tmpDir, bucket)) -> do
        file1 <- createEmptyFile $ tmpDir </> "file1.png"
        file2 <- createEmptyFile $ tmpDir </> "file2.png"
        bucket <- importFile bucket file1
        bucket <- importFile bucket file2
        bucket `assertHasItems` [bucketPath bucket </> "file1-1", bucketPath bucket </> "file2-1"]

    it "does not leave a trace in bucket if importing fails" $ withBucket $ \((tmpDir, bucket)) -> do
        let importNonExistingFile = do
            importFile bucket (tmpDir </> "nonExistingFile.png")
            assertFailure "importing should have thrown IOException"
        let assertBucketHasNoTraceOfFile = \e -> do
            let _ = (e :: IOException)
            assertDirectoryDoesNotExist $ bucketPath bucket </> "nonExistingFile-1"
        catch importNonExistingFile assertBucketHasNoTraceOfFile

    describe "item name" $ do

        prop "is unique" $
            forAll ourListOfStrings $ \itemNames ->
                let newItemName = createItemName itemNames ("/tmp/" ++ aItem ++ ".png")
                    aItem = itemPath $ head itemNames
                in newItemName `notElem` map itemPath itemNames

        it "when it's the only file" $
            createItemName [] "/tmp/foo.png" @?= "foo-1"

        it "when it's the third file with the same name" $
            createItemName [anItemWithName "foo", anItemWithName "foo-1"] "/tmp/foo.png" @?= "foo-2"

        it "when one of the existing items with same name has been deleted" $
            createItemName [anItemWithName "foo", anItemWithName "foo-2"] "/tmp/foo.png" @?= "foo-1"
