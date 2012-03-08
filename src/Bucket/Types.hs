module Bucket.Types where

import Meta
import qualified Data.Map as M
import System.FilePath

data Bucket = Bucket {
    bucketPath     :: FilePath,
    bucketItemsMap :: (M.Map FilePath BucketItem)
} deriving (Eq, Show)

data BucketItem = BucketItem {
    itemPath :: FilePath,
    itemMeta :: Meta
} deriving (Eq, Show)

addItem :: Bucket -> BucketItem -> Bucket
addItem bucket item = bucket { bucketItemsMap = M.insert (itemPath item) item (bucketItemsMap bucket) }

bucketItems (Bucket _ m) = M.elems m

bucketFromList :: FilePath -> [BucketItem] -> Bucket
bucketFromList path items = Bucket path (M.fromList (map (\a -> (itemPath a, a)) items))

itemFilePath :: BucketItem -> FilePath
itemFilePath item = itemPath item </> itemFileName item

itemFileName :: BucketItem -> FilePath
itemFileName = getValue "filename" "" . itemMeta

setItemFileName :: BucketItem -> FilePath -> BucketItem
setItemFileName (BucketItem a meta) path = BucketItem a (setValue "filename" path meta)

itemTags :: BucketItem -> [String]
itemTags = getValues "tag" . itemMeta
