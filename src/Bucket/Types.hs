module Bucket.Types where

import Meta
import System.FilePath

data Bucket = Bucket {
    bucketPath  :: FilePath,
    bucketItems :: [BucketItem]
} deriving (Eq, Show)

data BucketItem = BucketItem {
    itemPath :: FilePath,
    itemMeta :: Meta
} deriving (Eq, Show)

addItem :: Bucket -> BucketItem -> Bucket
addItem bucket item = bucket { bucketItems = item:bucketItems bucket }

itemFilePath :: BucketItem -> FilePath
itemFilePath item = itemPath item </> itemFileName item

itemFileName :: BucketItem -> FilePath
itemFileName item = getValue "filename" "" (itemMeta item)

setItemFileName :: BucketItem -> FilePath -> BucketItem
setItemFileName (BucketItem a meta) path = BucketItem a (setValue "filename" path meta)
