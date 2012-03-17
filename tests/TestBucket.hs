module TestBucket (tests) where

import Bucket.Types
import Meta
import Test.HUnit

tests = test
    [ "can get path to item file" ~: do
        let item = setFileName "bar.png" (BucketItem "a/path" createMeta)
        filePath item @?= "a/path/bar.png"
    ]
