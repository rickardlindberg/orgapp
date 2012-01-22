module Asserts where

import qualified Data.Set as Set
import System.Directory
import Test.HUnit

assertMovedTo src dest = do
    assertFileExists dest
    assertFileDoesNotExist src

assertFileDoesNotExist file = doesFileExist file >>= \exists -> assertBool "file does exist" (not exists)

assertFileExists file = doesFileExist file >>= assertBool "file does not exist"

assertDirectoryExists dir = do
    exists <- doesDirectoryExist dir
    assertBool "directory does not exist" exists

shouldBeSameAs :: [String] -> [String] -> Assertion
shouldBeSameAs filesInBucket expectedFiles =
    assertEqual "contents differed" (Set.fromList expectedFiles)
                                    (Set.fromList filesInBucket)
