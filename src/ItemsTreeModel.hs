module ItemsTreeModel
    ( ItemsTreeModel
    , itemsTreeModelNew
    , getItem
    ) where

import Bucket
import Graphics.UI.Gtk

type ItemsTreeModel = ListStore BucketItem

itemsTreeModelNew :: IO ItemsTreeModel
itemsTreeModelNew = listStoreNew []

getItem :: TreeView -> ItemsTreeModel -> TreePath -> IO BucketItem
getItem treeView model treePath = do
    Just treeIter <- treeModelGetIter model treePath
    listStoreGetValue model (listStoreIterToIndex treeIter)
