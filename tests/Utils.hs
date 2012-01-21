module Utils
    ( withTemporaryDirectory
    ) where

import Control.Exception.Base (bracket)
import System.Directory (createDirectory, removeDirectoryRecursive)

withTemporaryDirectory :: (FilePath -> IO ()) -> IO ()
withTemporaryDirectory = bracket setUp tearDown
    where
        tmpDir   = "/tmp/org-app"
        setUp    = createDirectory tmpDir >> return tmpDir
        tearDown = removeDirectoryRecursive
