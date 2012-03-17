module Fixtures where

import Bucket.Load
import Bucket.Types
import Control.Exception.Base (bracket)
import Data.Time
import Data.Time.Clock.POSIX
import Meta
import System.Directory
import System.FilePath
import System.IO
import System.Posix (getFileStatus, modificationTime, EpochTime, setFileTimes)

withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory = bracket setUp tearDown
    where
        tmpDir   = "/tmp/org-app"
        setUp    = createDirectory tmpDir >> return tmpDir
        tearDown = removeDirectoryRecursive

withBucket :: ((FilePath, Bucket) -> IO a) -> IO a
withBucket = bracket setUp tearDown
    where
        tmpDir     = "/tmp/org-app"
        bucketPath = tmpDir </> "a-bucket"
        setUp      = do
            createDirectory tmpDir
            bucket <- createBucket bucketPath
            return (tmpDir, bucket)
        tearDown (tmpDir, _) = removeDirectoryRecursive tmpDir

createEmptyFile :: FilePath -> IO FilePath
createEmptyFile path =
    createDirectoryIfMissing True (takeDirectory path) >>
    openFile path WriteMode >>= hClose >> return path

createItemAt :: FilePath -> FilePath -> IO FilePath
createItemAt path name = do
    createEmptyFile (path </> name)
    createEmptyFile (path </> "meta.txt")

anItemWithName :: String -> BucketItem
anItemWithName name = BucketItem ("/tmp/" ++ name) createMeta

setModificationTime :: FilePath -> Integer -> Int -> Int -> IO ()
setModificationTime path year month day = do
    let localDate = LocalTime (fromGregorian year month day) midnight
    zone <- getCurrentTimeZone
    let utcDate = localTimeToUTC zone localDate
    let posix = utcTimeToPOSIXSeconds utcDate
    let atime = 0
    let mtime = fromIntegral (round posix)
    setFileTimes path atime mtime

getMtime :: FilePath -> IO EpochTime
getMtime = fmap modificationTime . getFileStatus
