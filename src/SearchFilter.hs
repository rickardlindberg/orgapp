module SearchFilter (matchSearch) where

import Bucket

matchSearch :: String -> BucketItem -> Bool
matchSearch searchString item =
    -- TODO: implement smart search
    True
