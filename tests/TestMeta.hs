module TestMeta (tests, props) where

import Meta
import Test.HUnit
import Test.QuickCheck

tests = test
    [ "reading meta" ~:

        [ "filename" ~: do
            let meta = metaFromStr "filename::foo.png\n"
            getValue "filename" "" meta @?= "foo.png"
        ]

    , "preserves unkonw fields" ~: do
        let metaFile = "unknownField::5\n"
        metaToStr (metaFromStr metaFile) @?= metaFile

    , "implementation details" ~:

        [ "parses text into list of pairs" ~: do
            parseMeta ""                 @?= []
            parseMeta "foo::bar\n"       @?= [("foo", "bar")]
            parseMeta "foo::bar\na::b\n" @?= [("foo", "bar"), ("a", "b")]
        ]
    ]

prop_roundtrip_meta meta = metaFromStr (metaToStr meta) == meta

props =
    [ property prop_roundtrip_meta
    ]

instance Arbitrary Meta where
    arbitrary = do
        f <- arbitraryMetaValue
        return (setValue "filename" f createMeta)

arbitraryMetaValue :: Gen String
arbitraryMetaValue = oneof [ return "foo", return "bar" ]
