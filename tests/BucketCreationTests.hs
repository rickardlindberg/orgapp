module BucketCreationTests (tests) where

import Bucket (createBucket, importFile)
import Fixtures
import System.FilePath
import Test.HUnit
import Utils

tests = test
    [ "has directory for new bucket" ~: withBucket $ \((tmpDir, bucketPath)) -> do
        assertDirectoryExists bucketPath

    , "importing a file moves it inside the bucket" ~: withBucket $ \((tmpDir, bucketPath)) -> do
        let aSourceFile = tmpDir </> "a-file.png"
        createEmptyFile aSourceFile
        importFile bucketPath aSourceFile
        aSourceFile `assertMovedTo` (bucketPath </> "a-file" </> "a-file.png")
    ]
