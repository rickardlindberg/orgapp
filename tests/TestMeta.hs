module TestMeta (tests) where

import Meta
import Test.HUnit

tests = test
    [ "reading meta" ~:

        [ "single items" ~: do
            let meta = metaFromStr "filename::foo.png\n"
            getValue "filename" "" meta @?= "foo.png"

        , "defaults to given value" ~:
            getValue "nonExisting" "DEF" createMeta @?= "DEF"

        , "multiple items" ~: do
            let meta = metaFromStr "tag::good\ntag::sports\n"
            getValues "tag" meta @?= ["good", "sports"]

        , "defaults to empty list" ~:
            getValues "nonExisting" createMeta @?= []

        , "getting single returns first" ~: do
            let meta = setValues "tag" ["sports", "good"] createMeta
            getValue "tag" "" meta @?= "sports"
        ]

    , "writing meta" ~:

        [ "single items" ~: do
            let meta = setValue "filename" "foo.png" createMeta
            metaToStr meta @?= "filename::foo.png\n"

        , "multiple items" ~: do
            let meta = setValues "tag" ["good", "sports"] createMeta
            metaToStr meta @?= "tag::good\ntag::sports\n"

        , "does not write empty lists" ~: do
            let meta = setValues "tag" [] createMeta
            metaToStr meta @?= ""
        ]

    , "implementation details" ~:

        [ "parses text into list of pairs" ~: do
            parseMeta ""                 @?= []
            parseMeta "foo::bar\n"       @?= [("foo", "bar")]
            parseMeta "foo::bar\na::b\n" @?= [("foo", "bar"), ("a", "b")]
        ]
    ]
