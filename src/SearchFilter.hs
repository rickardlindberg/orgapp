module SearchFilter (matchSearch) where

import Bucket.Types
import Data.Text

matchSearch :: String -> BucketItem -> Bool
matchSearch searchString item =
    toLower (pack searchString) `isInfixOf` toLower (pack (itemFileName item))
