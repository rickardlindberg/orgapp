import Bucket
import Control.Monad
import Test.QuickCheck

instance Arbitrary Bucket where
    arbitrary = liftM2 Bucket arbitratyPath arbitrary

instance Arbitrary BucketItem where
    arbitrary = liftM BucketItem arbitratyPath

arbitratyPath :: Gen FilePath
arbitratyPath = oneof [ return "/tmp", return "/home" ]

ourListOfStrings :: Gen [BucketItem]
ourListOfStrings =
    oneof
        [ vectorOf 1 itemNameGenerator
        , vectorOf 2 itemNameGenerator
        , vectorOf 3 itemNameGenerator
        ]
    where
        itemNameGenerator = elements [BucketItem "foo", BucketItem "foo-1", BucketItem "foo-2"]

prop_name_is_unique :: Property
prop_name_is_unique =
    forAll ourListOfStrings $ \itemNames ->
        let newItemName = createItemName itemNames ("/tmp/" ++ aItem ++ ".png")
            aItem = itemPath $ head itemNames
        in newItemName `notElem` (map itemPath itemNames)

prop_adding_item_makes_bucket_bigger bucket item = newSize == oldSize + 1
    where
        newBucket = addItem bucket item
        newSize   = length $ bucketItems newBucket
        oldSize   = length $ bucketItems bucket

main = do
    quickCheck prop_name_is_unique
    quickCheck prop_adding_item_makes_bucket_bigger
