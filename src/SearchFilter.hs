module SearchFilter (matchSearch) where

import Bucket.Types

matchSearch :: String -> BucketItem -> Bool
matchSearch searchString item =
    -- TODO: implement smart search
    True
