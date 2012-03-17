module TestEditItem (tests) where

import Asserts
import Bucket.EditItem
import Bucket.Import
import Bucket.Types
import Control.Exception
import DirectoryInfo
import Fixtures
import Meta
import Prelude hiding (catch)
import System.FilePath
import Test.HUnit

tests = test
    [ "editing an item changes the meta on disk" ~: withBucket $ \((tmpDir, bucket)) -> do
        aSourceFile <- createEmptyFile $ tmpDir </> "a-file.png"
        bucket <- importFile bucket aSourceFile
        let item = head (bucketItems bucket)
        let newItem = setTags item ["foo"]
        newBucket <- editItem bucket item newItem
        assertFileContains (bucketPath bucket </> "a-file-1" </> metaFileName) "tag::foo"

    , "editing an item returns an updated bucket" ~: withBucket $ \((tmpDir, bucket)) -> do
        aSourceFile <- createEmptyFile $ tmpDir </> "a-file.png"
        bucket <- importFile bucket aSourceFile
        let item = head (bucketItems bucket)
        let newItem = setTags item ["foo"]
        newBucket <- editItem bucket item newItem
        assertEqual "" [newItem] (bucketItems newBucket)
    ]
