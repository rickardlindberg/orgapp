module Asserts where

import Bucket
import qualified Data.Set as Set
import System.Directory
import Test.HUnit

assertMovedTo src dest = do
    assertFileExists dest
    assertFileDoesNotExist src

assertFileDoesNotExist file = doesFileExist file >>= \exists -> assertBool ("file '" ++file ++ "' does exist") (not exists)

assertFileExists file = doesFileExist file >>= assertBool ("file '" ++ file ++ "' does not exist")

assertDirectoryExists dir = do
    exists <- doesDirectoryExist dir
    assertBool "directory does not exist" exists

assertHasItems :: Bucket -> [String] -> Assertion
assertHasItems bucket itemNames = (map itemPath (bucketItems bucket)) `shouldBeSameAs` itemNames

shouldBeSameAs :: [String] -> [String] -> Assertion
shouldBeSameAs filesInBucket expectedFiles =
    assertEqual "contents differed" (Set.fromList expectedFiles)
                                    (Set.fromList filesInBucket)
