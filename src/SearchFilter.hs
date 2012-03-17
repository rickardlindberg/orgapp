module SearchFilter (matchSearch) where

import Bucket.Types
import Data.Text hiding (any)

type Predicate = String -> BucketItem -> Bool

type Matcher = BucketItem -> Bool

matchSearch :: Predicate
matchSearch searchString =
    matchFileName searchString `matchOr` matchTag searchString

matchFileName :: String -> Matcher
matchFileName searchString item =
    toLower (pack searchString) `isInfixOf` toLower (pack (fileName item))

matchTag :: String -> Matcher
matchTag searchString item = any matchTag (tags item)
    where
        matchTag tag = pack searchString `isInfixOf` pack tag

matchOr :: Matcher -> Matcher -> Matcher
matchOr left right item = or [left item, right item]
