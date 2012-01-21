import qualified BucketCreationTests as BC
import qualified BucketLoadingTests as BL
import System.Exit
import Test.HUnit

allTests = test
    [ BC.tests
    , BL.tests
    ]

main = runTestTT allTests >>= exit
    where
        exit Counts { errors=0, failures=0 } = exitSuccess
        exit _                               = exitFailure
