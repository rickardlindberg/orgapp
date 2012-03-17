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

displayTitle :: BucketItem -> String
displayTitle item = case title item of
    ""    -> fileName item
    title -> title

title :: BucketItem -> String
title = getValue "title" "" . itemMeta

setTitle :: String -> BucketItem -> BucketItem
setTitle title (BucketItem a meta) = BucketItem a (setValue "title" title meta)

filePath :: BucketItem -> FilePath
filePath item = itemPath item </> fileName item

fileName :: BucketItem -> FilePath
fileName = getValue "filename" "" . itemMeta

setFileName :: FilePath -> BucketItem -> BucketItem
setFileName path (BucketItem a meta) = BucketItem a (setValue "filename" path meta)

tags :: BucketItem -> [String]
tags = getValues "tag" . itemMeta

setTags :: [String] -> BucketItem -> BucketItem
setTags tags (BucketItem a meta) = BucketItem a (setValues "tag" tags meta)

creationDate :: BucketItem -> String
creationDate = getValue "creationdate" "" . itemMeta

setCreationDate :: String -> BucketItem -> BucketItem
setCreationDate date (BucketItem a meta) = BucketItem a (setValue "creationdate" date meta)
