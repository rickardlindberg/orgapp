module Asserts where

import Bucket
import Control.Monad
import Data.List
import qualified Data.Set as Set
import System.Directory
import Test.HUnit

assertMovedTo :: FilePath -> FilePath -> Assertion
assertMovedTo src dest = do
    assertFileExists dest
    assertFileDoesNotExist src

assertFileExists :: FilePath -> Assertion
assertFileExists file =
    doesFileExist file >>=
    assertBool ("expected file '" ++ file ++ "' to exist")

assertFileContains :: FilePath -> String -> Assertion
assertFileContains file needle = do
    contents <- readFile file
    assertBool ("expected file '" ++ file ++ "' to contain '" ++ needle ++
                "' but was:\n" ++ contents)
               (needle `isInfixOf` contents)

assertFileDoesNotExist :: FilePath -> Assertion
assertFileDoesNotExist file =
    liftM not (doesFileExist file) >>=
    assertBool ("expected file '" ++ file ++ "' to not exist")

assertDirectoryExists :: FilePath -> Assertion
assertDirectoryExists dir =
    doesDirectoryExist dir >>=
    assertBool ("expected directory '" ++ dir ++ "' to exist")

assertDirectoryDoesNotExist :: FilePath -> Assertion
assertDirectoryDoesNotExist dir =
    liftM not (doesDirectoryExist dir) >>=
    assertBool ("expeected directory '" ++ dir ++ "' to not exist")

assertHasItems :: Bucket -> [String] -> Assertion
assertHasItems bucket itemNames =
    (map itemPath (bucketItems bucket)) `shouldBeSameAs` itemNames

shouldBeSameAs :: [String] -> [String] -> Assertion
shouldBeSameAs filesInBucket expectedFiles =
    assertEqual "contents differed" (Set.fromList expectedFiles)
                                    (Set.fromList filesInBucket)
