module Bucket.Types where

import Meta
import qualified Data.Map as M
import System.FilePath

data Bucket = Bucket {
    bucketPath     :: FilePath,
    bucketItemsMap :: (M.Map FilePath BucketItem)
} deriving (Eq, Show)

addItem :: Bucket -> BucketItem -> Bucket
addItem bucket item = bucket { bucketItemsMap = M.insert (itemPath item) item (bucketItemsMap bucket) }

bucketItems (Bucket _ m) = M.elems m

bucketFromList :: FilePath -> [BucketItem] -> Bucket
bucketFromList path items = Bucket path (M.fromList (map (\a -> (itemPath a, a)) items))

data BucketItem = BucketItem {
    itemPath :: FilePath,
    itemMeta :: Meta
} deriving (Eq, Show)

filePath :: BucketItem -> FilePath
filePath item = itemPath item </> fileName item

fileName :: BucketItem -> FilePath
fileName = getValue "filename" "" . itemMeta

setFileName :: BucketItem -> FilePath -> BucketItem
setFileName (BucketItem a meta) path = BucketItem a (setValue "filename" path meta)

tags :: BucketItem -> [String]
tags = getValues "tag" . itemMeta

setTags :: BucketItem -> [String] -> BucketItem
setTags (BucketItem a meta) tags = BucketItem a (setValues "tag" tags meta)

setCreationDate :: BucketItem -> String -> BucketItem
setCreationDate (BucketItem a meta) date = BucketItem a (setValue "creationdate" date meta)
