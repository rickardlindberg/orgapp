module SearchFilter (matchSearch) where

import Bucket.Types
import Data.Text (pack, toLower, isInfixOf)

type Matcher = BucketItem -> Bool

matchSearch :: String -> Matcher
matchSearch = foldl matchAnd (const True) . map matchItem . words

matchItem :: String -> Matcher
matchItem itemString =
    matchDisplayTitle itemString `matchOr` matchTag itemString

matchDisplayTitle :: String -> Matcher
matchDisplayTitle searchString item =
    toLower (pack searchString) `isInfixOf` toLower (pack (displayTitle item))

matchTag :: String -> Matcher
matchTag searchString item = any matchTag (tags item)
    where
        matchTag tag = pack searchString `isInfixOf` pack tag

matchOr :: Matcher -> Matcher -> Matcher
matchOr left right item = or [left item, right item]

matchAnd :: Matcher -> Matcher -> Matcher
matchAnd left right item = and [left item, right item]
