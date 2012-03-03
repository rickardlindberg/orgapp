module TestBucket (tests) where

import Bucket.Types
import Meta
import Test.HUnit

tests = test
    [ "can get path to item file" ~: do
        let item = BucketItem "a/path" (setValue "filename" "bar.png" createMeta)
        itemFilePath item @?= "a/path/bar.png"
    ]
