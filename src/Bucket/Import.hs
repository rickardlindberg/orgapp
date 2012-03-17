module Bucket.Import where

import Bucket.Types
import Control.Exception
import Control.Monad
import Data.List
import Data.Time
import Data.Time.Clock.POSIX
import DirectoryInfo
import Meta
import System.Directory
import System.FilePath
import System.Locale (defaultTimeLocale)
import System.Posix (getFileStatus, accessTime, modificationTime, EpochTime, setFileTimes)
import Text.Printf

importFile :: Bucket -> FilePath -> IO Bucket
importFile bucket srcPath = do
    newItem <- bucketItemFromSrc bucket srcPath
    (atime, mtime) <- getUnixTimestamps srcPath
    let itemDir = itemPath newItem
    prepareDirectory itemDir $ do
        writeMeta (itemMeta newItem) (itemDir </> metaFileName)
        copyFile srcPath (itemDir </> fileName newItem)
        setFileTimes (itemDir </> fileName newItem) atime mtime
        return $ addItem bucket newItem

prepareDirectory :: FilePath -> IO a -> IO a
prepareDirectory path action =
    bracketOnError
        (createDirectory path >> return path)
        removeDirectoryRecursive
        (\_ -> action)

bucketItemFromSrc :: Bucket -> FilePath -> IO BucketItem
bucketItemFromSrc bucket srcPath = do
    dateStr' <- dateStr srcPath
    return $ setCreationDate dateStr'
           $ setFileName srcFileName
           $ BucketItem itemDirectory createMeta
    where
        itemDirectory = bucketPath bucket </> itemName
        itemName      = createItemName (bucketItems bucket) srcPath
        srcFileName   = takeFileName srcPath

dateStr :: FilePath -> IO String
dateStr path = do
    fs <- getFileStatus path
    zone <- getCurrentTimeZone
    let mtime     = modificationTime fs
    let posixTime = realToFrac mtime
    let utc       = posixSecondsToUTCTime posixTime
    let local     = utcToLocalTime zone utc
    return $ formatTime defaultTimeLocale "%Y-%m-%d" local

getUnixTimestamps :: FilePath -> IO (EpochTime, EpochTime)
getUnixTimestamps path = do
    fs <- getFileStatus path
    return (accessTime fs, modificationTime fs)

createItemName :: [BucketItem] -> FilePath -> String
createItemName existingItems filePath = uniqueItemName
    where
        idealItemName = takeBaseName filePath
        -- TODO: take into consideration in which subdirectory the item is in
        itemNames = map (takeFileName . itemPath) existingItems
        itemsWithSamePrefix = filter (idealItemName `isPrefixOf`) itemNames
        uniqueItemName = untilUnique idealItemName 1
        isUnique name = name `notElem` itemNames
        untilUnique name n | isUnique (name ++ "-" ++ show n) = name ++ "-" ++ show n
                           | otherwise     = untilUnique name (n + 1)
