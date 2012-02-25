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
        info @?= [ DirectoryInfo (tmpDir)                         ["a-file.png"] Nothing
                 , DirectoryInfo (tmpDir </> "b-dir")             ["b-file.png"] Nothing
                 , DirectoryInfo (tmpDir </> "b-dir" </> "c-dir") []             Nothing
                 ]

    , "pupulates meta" ~: withTemporaryDirectory $ \tmpDir -> do
        writeFile (tmpDir </> "meta.txt") "meta content"
        info <- getDirectoryInfoRecursive tmpDir
        info @?= [ DirectoryInfo tmpDir ["meta.txt"] (Just "meta content")
                 ]
    ]
