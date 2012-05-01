module TestSearch (tests) where

import Bucket.Types
import Meta
import SearchFilter
import Test.Hspec.Monadic

tests = describe "search" $ do

    let testItem = setTags ["football", "fun"]
                 $ setFileName "foo.png"
                 $ BucketItem "/path/to/item" createMeta

    describe "matches" $ do

        it "partial item name" $
            matchSearch "foo" testItem

        it "partial item name with wrong case" $
            matchSearch "Foo" testItem

        it "partial tag" $
            matchSearch "foot" testItem

        it "when all words match" $
            matchSearch "foo fun" testItem

    describe "doesn't match" $ do

        it "item path" $
            not $ matchSearch "item" testItem

        it "tag with wrong case" $
            not $ matchSearch "FootBall" testItem

        it "when all words don't match" $
            not $ matchSearch "foo XYZ" testItem
