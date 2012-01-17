import Control.Exception.Base (bracket)
import System.Directory
import System.Exit
import Test.HUnit

createBucket :: FilePath -> IO ()
createBucket = createDirectory

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

tests = test [ "can create bucket" ~: withTemporaryDirectory $ \path -> do
                 createBucket (path ++ "/a-bucket")
                 exists <- (doesDirectoryExist (path ++ "/a-bucket"))
                 assertBool "default bucket does not exist" exists
             ]

main = runTestTT tests >>= exit
    where
        exit Counts { errors=0, failures=0 } = exitSuccess
        exit _                               = exitFailure
