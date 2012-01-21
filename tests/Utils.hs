module Utils
    ( withTemporaryDirectory
    ) where

import Control.Exception.Base (bracket)
import System.Directory

withTemporaryDirectory :: (FilePath -> IO ()) -> IO ()
withTemporaryDirectory = bracket setUp tearDown
    where
        setUp :: IO FilePath
        setUp = do
            createDirectory "/tmp/org-app"
            return "/tmp/org-app"
        tearDown :: FilePath -> IO ()
        tearDown path = do
            removeDirectoryRecursive path
