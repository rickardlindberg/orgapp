import Bucket
import Test.QuickCheck

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

main = do
    quickCheck prop_name_is_unique
