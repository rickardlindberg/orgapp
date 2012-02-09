module TestReadDirectoryInfo (tests) where

import DirectoryInfo
import Fixtures
import System.Directory
import System.FilePath
import Test.HUnit

tests = test
    [ "can read directory info from file system" ~: withTemporaryDirectory $ \tmpDir -> do
        createEmptyFile $ tmpDir </> "a-file.png"
        createDirectory $ tmpDir </> "b-dir"
        createEmptyFile $ tmpDir </> "b-dir" </> "b-file.png"
        createDirectory $ tmpDir </> "b-dir" </> "c-dir"
        info <- getDirectoryInfoRecursive tmpDir
        info @?= [ DirectoryInfo (tmpDir)                         ["a-file.png"]
                 , DirectoryInfo (tmpDir </> "b-dir")             ["b-file.png"]
                 , DirectoryInfo (tmpDir </> "b-dir" </> "c-dir") []
                 ]
    ]
