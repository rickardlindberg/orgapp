module SearchFilter (matchSearch) where

import Bucket

matchSearch :: String -> BucketItem -> Bool
matchSearch searchString item = True
