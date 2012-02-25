module TestMeta (tests) where

import Meta
import Test.HUnit

tests = test
    [ "parsing meta examples" ~: do
        parseMeta "" @?= []
        parseMeta "foo::bar\n" @?= [("foo", "bar")]
        parseMeta "foo::bar\na::b\n" @?= [("foo", "bar"), ("a", "b")]
    ]
