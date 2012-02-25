module DirectoryInfo
    ( DirectoryInfo(..)
    , getDirectoryInfoRecursive
    , metaFileName
    , hasMetaFile
    ) where

import Control.Monad
import System.Directory
import System.FilePath

metaFileName = "meta.txt"

data DirectoryInfo = DirectoryInfo {
    path  :: FilePath,
    files :: [String],
    meta  :: Maybe String
} deriving (Eq, Show)

getDirectoryInfoRecursive :: FilePath -> IO [DirectoryInfo]
getDirectoryInfoRecursive = mapDirectories dirToInfo

mapDirectories :: (FilePath -> IO a) -> FilePath -> IO [a]
mapDirectories fn rootDir = do
    thisDir   <- fn rootDir
    contents  <- getDirectoryContents rootDir
    innerDirs <- forM (filter (`notElem` [".", ".."]) contents) $ \path -> do
        let fullPath = rootDir </> path
        isDirectory <- doesDirectoryExist fullPath
        case isDirectory of
            True  -> mapDirectories fn fullPath
            False -> return []
    return $ thisDir:(concat innerDirs)

dirToInfo :: FilePath -> IO DirectoryInfo
dirToInfo dir = do
    contents <- getDirectoryContents dir
    files    <- filterM (\path -> isFile (dir </> path)) contents
    meta     <- case metaFileName `elem` files of
                    True  -> readFile (dir </> metaFileName) >>= return . Just
                    False -> return Nothing
    return   $  DirectoryInfo dir files meta

isFile :: FilePath -> IO Bool
isFile = liftM not . doesDirectoryExist

hasMetaFile :: DirectoryInfo -> Bool
hasMetaFile DirectoryInfo { files = files } = metaFileName `elem` files
