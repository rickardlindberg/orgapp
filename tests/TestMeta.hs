module TestMeta (tests, props) where

import Meta
import Test.HUnit
import Test.QuickCheck

tests = test
    [ "parsing meta examples" ~: do
        parseMeta "" @?= []
        parseMeta "foo::bar\n" @?= [("foo", "bar")]
        parseMeta "foo::bar\na::b\n" @?= [("foo", "bar"), ("a", "b")]
    ]

prop_roundtrip_meta meta = metaFromStr (metaToStr meta) == meta

props =
    [ property prop_roundtrip_meta
    ]

instance Arbitrary Meta where
    arbitrary = do
        f <- arbitraryMetaValue
        return (setFilename f createMeta)

arbitraryMetaValue :: Gen String
arbitraryMetaValue = oneof [ return "foo", return "bar" ]
