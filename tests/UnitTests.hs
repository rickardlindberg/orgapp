import qualified TestCreateBucket as TestCreateBucket
import qualified TestImportFile as TestImportFile
import qualified TestLoadBucket as TestLoadBucket
import qualified TestMeta as TestMeta
import qualified TestReadDirectoryInfo as TestReadDirectoryInfo
import System.Exit
import Test.HUnit

allTests = test
    [ TestCreateBucket.tests
    , TestImportFile.tests
    , TestLoadBucket.tests
    , TestMeta.tests
    , TestReadDirectoryInfo.tests
    ]

main = runTestTT allTests >>= exit
    where
        exit Counts { errors=0, failures=0 } = exitSuccess
        exit _                               = exitFailure
