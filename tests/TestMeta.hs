module TestMeta (tests) where

import Fixtures()
import Meta
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.HUnit

tests = describe "Meta" $ do

    describe "can read" $ do

        it "single items" $ do
            let meta = metaFromStr "filename::foo.png\n"
            getValue "filename" "" meta @?= "foo.png"

        it "defaults to given value" $
            getValue "nonExisting" "DEF" createMeta @?= "DEF"

        it "multiple items" $ do
            let meta = metaFromStr "tag::good\ntag::sports\n"
            getValues "tag" meta @?= ["good", "sports"]

        it "defaults to empty list" $
            getValues "nonExisting" createMeta @?= []

        it "getting single returns first" $ do
            let meta = setValues "tag" ["sports", "good"] createMeta
            getValue "tag" "" meta @?= "sports"

    describe "can write" $ do

        it "single items" $ do
            let meta = setValue "filename" "foo.png" createMeta
            metaToStr meta @?= "filename::foo.png\n"

        it "multiple items" $ do
            let meta = setValues "tag" ["good", "sports"] createMeta
            metaToStr meta @?= "tag::good\ntag::sports\n"

        it "does not write empty lists" $ do
            let meta = setValues "tag" [] createMeta
            metaToStr meta @?= ""

    describe "implementation details:" $ do

        it "parses text into list of pairs" $ do
            parseMeta ""                 @?= []
            parseMeta "foo::bar\n"       @?= [("foo", "bar")]
            parseMeta "foo::bar\na::b\n" @?= [("foo", "bar"), ("a", "b")]

        prop "roundtrip meta property" $ \meta ->
            metaFromStr (metaToStr meta) == meta
