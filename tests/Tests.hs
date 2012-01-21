import System.Exit
import Test.HUnit
import Utils (withTemporaryDirectory)
import BucketLoaderTests (bucketLoadingTests)
import System.Directory
import Bucket (createBucket)

tests = test [
    "can create bucket" ~: withTemporaryDirectory $ \path -> do
        createBucket (path ++ "/a-bucket")
        exists <- (doesDirectoryExist (path ++ "/a-bucket"))
        assertBool "default bucket does not exist" exists

    ]

allTests = test [tests, bucketLoadingTests]

main = runTestTT allTests >>= exit
    where
        exit Counts { errors=0, failures=0 } = exitSuccess
        exit _                               = exitFailure
