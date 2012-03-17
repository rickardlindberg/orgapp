module Bucket.EditItem where

import Bucket.Types
import DirectoryInfo
import Meta
import System.FilePath

editItem :: Bucket -> BucketItem -> BucketItem -> IO Bucket
editItem bucket oldItem newItem =
    -- TODO: fail if oldItem and newItem does not have same path?
    let newBucket = addItem bucket newItem
        newMeta = itemMeta newItem
        itemDir = itemPath newItem
    in do
        writeMeta newMeta (itemDir </> metaFileName)
        return newBucket
