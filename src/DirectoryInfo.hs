module DirectoryInfo
    ( DirectoryInfo(..)
    , getDirectoryInfoRecursive
    ) where

import System.Directory
import System.FilePath

data DirectoryInfo = DirectoryInfo {
    relativePath :: FilePath,
    files        :: [FilePath]
}

getDirectoryInfoRecursive :: FilePath -> IO [DirectoryInfo]
getDirectoryInfoRecursive rootPath = getFileInfosNew rootPath ""

getFileInfosNew :: FilePath -> FilePath -> IO [DirectoryInfo]
getFileInfosNew rootPath relative = do
    isDirectory <- doesDirectoryExist $ rootPath </> relative
    case isDirectory of
        False -> return $ [DirectoryInfo relative []]
        True  -> do
            files <- getDirectoryContents $ rootPath </> relative
            inner <- mapM (\x -> getFileInfosNew rootPath (relative </> x)) (filter (`notElem` [".", ".."]) files)
            return $ (DirectoryInfo relative files):(concat inner)
