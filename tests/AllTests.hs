import Bucket.Import
import Bucket.Types
import Fixtures
import Meta
import qualified Data.Map as M
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
import Test.Hspec.QuickCheck
import Test.QuickCheck

main = hspecX $ do

    describe "unit tests" $ do

        it "TestBucket" TestBucket.tests
        it "TestCreateBucket" TestCreateBucket.tests
        it "TestEditItem" TestEditItem.tests
        it "TestImportFile" TestImportFile.tests
        it "TestLoadBucket" TestLoadBucket.tests
        it "TestMeta" TestMeta.tests
        it "TestReadDirectoryInfo" TestReadDirectoryInfo.tests
        it "TestSearch" TestSearch.tests

    describe "quick check tests" $ do

        prop "name is unique" $
            forAll ourListOfStrings $ \itemNames ->
                let newItemName = createItemName itemNames ("/tmp/" ++ aItem ++ ".png")
                    aItem = itemPath $ head itemNames
                in newItemName `notElem` map itemPath itemNames

        prop "adding item makes bucket bigger" $ \(bucket, item) ->
            let newBucket = addItem bucket item
                newSize   = length $ bucketItems newBucket
                oldSize   = length $ bucketItems bucket
            in M.notMember (itemPath item) (bucketItemsMap bucket) ==> newSize == oldSize + 1

        prop "roundtrip meta" $ \meta ->
            metaFromStr (metaToStr meta) == meta
