module TestSearch (tests) where

import Bucket.Types
import Meta
import SearchFilter
import Test.HUnit

testItem = setTags
               (setFileName
                   (BucketItem "/path/to/item" createMeta)
               "foo.png")
           ["football", "fun"]

tests = test
    [ "matches" ~: do
        "foo"      `assertMatch`   "because partial item name"
        "Foo"      `assertMatch`   "because partial item name with wrong case"
        "foot"     `assertMatch`   "because partial tag"

    , "does not match" ~: do
        "item"     `assertNoMatch` "because item path is not searched"
        "FootBall" `assertNoMatch` "because tag with wrong case"
    ]

assertMatch search message = assertBool message (matchSearch search testItem)

assertNoMatch search message = assertBool message (not (matchSearch search testItem))
