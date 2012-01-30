import Bucket (createItemName)
import Test.QuickCheck

ourListOfStrings :: Gen [String]
ourListOfStrings =
    oneof
        [ vectorOf 1 itemNameGenerator
        , vectorOf 2 itemNameGenerator
        , vectorOf 3 itemNameGenerator
        ]
    where
        itemNameGenerator = elements ["foo", "foo-1", "foo-2"]

prop_name_is_unique :: Property
prop_name_is_unique =
    forAll ourListOfStrings $ \itemNames ->
        let newItemName = createItemName itemNames ("/tmp/" ++ aItem ++ ".png")
            aItem = head itemNames
        in newItemName `notElem` itemNames

main = do
    quickCheck prop_name_is_unique
