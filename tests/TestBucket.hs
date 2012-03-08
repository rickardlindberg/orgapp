module TestBucket (tests) where

import Bucket.Types
import Meta
import Test.HUnit

tests = test
    [ "can get path to item file" ~: do
        let item = setFileName (BucketItem "a/path" createMeta) "bar.png"
        filePath item @?= "a/path/bar.png"
    ]
