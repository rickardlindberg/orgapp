import qualified TestBucket as TestBucket
import qualified TestCreateBucket as TestCreateBucket
import qualified TestEditItem as TestEditItem
import qualified TestImportFile as TestImportFile
import qualified TestLoadBucket as TestLoadBucket
import qualified TestMeta as TestMeta
import qualified TestReadDirectoryInfo as TestReadDirectoryInfo
import qualified TestSearch as TestSearch
import Test.Hspec.Monadic

main = hspecX $ do

    TestBucket.tests
    TestCreateBucket.tests
    TestEditItem.tests
    TestImportFile.tests
    TestLoadBucket.tests
    TestMeta.tests
    TestReadDirectoryInfo.tests
    TestSearch.tests
