module TestSearch (tests) where

import Bucket.Types
import Meta
import SearchFilter
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.HUnit

testItem = setTags ["football", "fun"]
         $ setFileName "foo.png"
         $ BucketItem "/path/to/item" createMeta

tests = describe "search" $ do

    it "matches" $ do
        "foo"      `assertMatch`   "because partial item name"
        "Foo"      `assertMatch`   "because partial item name with wrong case"
        "foot"     `assertMatch`   "because partial tag"

    it "does not match" $ do
        "item"     `assertNoMatch` "because item path is not searched"
        "FootBall" `assertNoMatch` "because tag with wrong case"

assertMatch search message = assertBool message (matchSearch search testItem)

assertNoMatch search message = assertBool message (not (matchSearch search testItem))
