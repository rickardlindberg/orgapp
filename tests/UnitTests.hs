import qualified TestBucket as TestBucket
import qualified TestCreateBucket as TestCreateBucket
import qualified TestEditItem as TestEditItem
import qualified TestImportFile as TestImportFile
import qualified TestLoadBucket as TestLoadBucket
import qualified TestMeta as TestMeta
import qualified TestReadDirectoryInfo as TestReadDirectoryInfo
import qualified TestSearch as TestSearch
import System.Exit
import Test.HUnit

allTests = test
    [ TestBucket.tests
    , TestCreateBucket.tests
    , TestEditItem.tests
    , TestImportFile.tests
    , TestLoadBucket.tests
    , TestMeta.tests
    , TestReadDirectoryInfo.tests
    , TestSearch.tests
    ]

main = runTestTT allTests >>= exit
    where
        exit Counts { errors=0, failures=0 } = exitSuccess
        exit _                               = exitFailure
