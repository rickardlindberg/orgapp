module TestEditItem (tests) where

import Asserts
import Bucket.EditItem
import Bucket.Import
import Bucket.Types
import DirectoryInfo
import Fixtures
import Prelude hiding (catch)
import System.FilePath
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.HUnit

tests = describe "editing item" $ do

    it "item changes the meta on disk" $ withBucket $ \((tmpDir, bucket)) -> do
        aSourceFile <- createEmptyFile $ tmpDir </> "a-file.png"
        bucket <- importFile bucket aSourceFile
        let item = head (bucketItems bucket)
        let newItem = setTags ["foo"] item
        newBucket <- editItem bucket item newItem
        assertFileContains (bucketPath bucket </> "a-file-1" </> metaFileName) "tag::foo"

    it "returns an updated bucket" $ withBucket $ \((tmpDir, bucket)) -> do
        aSourceFile <- createEmptyFile $ tmpDir </> "a-file.png"
        bucket <- importFile bucket aSourceFile
        let item = head (bucketItems bucket)
        let newItem = setTags ["foo"] item
        newBucket <- editItem bucket item newItem
        assertEqual "" [newItem] (bucketItems newBucket)
