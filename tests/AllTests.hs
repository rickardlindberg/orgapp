import qualified TestBucket as TestBucket
import qualified TestCreateBucket as TestCreateBucket
import qualified TestEditItem as TestEditItem
import qualified TestImportFile as TestImportFile
import qualified TestLoadBucket as TestLoadBucket
import qualified TestMeta as TestMeta
import qualified TestReadDirectoryInfo as TestReadDirectoryInfo
import qualified TestSearch as TestSearch
import Test.Hspec.HUnit()
import Test.Hspec.Monadic

main = hspecX $

    describe "unit tests" $ do

        TestBucket.tests
        it "TestCreateBucket" TestCreateBucket.tests
        it "TestEditItem" TestEditItem.tests
        TestImportFile.tests
        it "TestLoadBucket" TestLoadBucket.tests
        TestMeta.tests
        it "TestReadDirectoryInfo" TestReadDirectoryInfo.tests
        it "TestSearch" TestSearch.tests
