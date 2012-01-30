module FileInfo
    ( FileInfo(..)
    , getFileInfos
    ) where

import System.Directory
import System.FilePath

data FileInfo = FileInfo {
    relativePath :: FilePath,
    isDirectory  :: Bool
}

getFileInfos :: FilePath -> IO [FileInfo]
getFileInfos rootDir = do
    files         <- getDirectoryContents rootDir
    isDirectories <- mapM isNameDirectory files
    return        $  zipWith FileInfo files isDirectories
    where
        isNameDirectory name = doesDirectoryExist $ rootDir </> name
