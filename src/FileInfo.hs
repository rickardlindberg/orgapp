module FileInfo
    ( FileInfo(..)
    , getFileInfos
    ) where

import System.Directory
import System.FilePath

data FileInfo = FileInfo {
    relativePath :: FilePath,
    subFiles     :: [FilePath]
}

getFileInfos :: FilePath -> IO [FileInfo]
getFileInfos rootDir = getFileInfosNew rootDir ""

getFileInfosNew :: FilePath -> FilePath -> IO [FileInfo]
getFileInfosNew rootPath relative = do
    isDirectory <- doesDirectoryExist $ rootPath </> relative
    case isDirectory of
        False -> return $ [FileInfo relative []]
        True  -> do
            files <- getDirectoryContents $ rootPath </> relative
            inner <- mapM (\x -> getFileInfosNew rootPath (relative </> x)) (filter (`notElem` [".", ".."]) files)
            return $ (FileInfo relative files):(concat inner)
