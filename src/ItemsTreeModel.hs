module ItemsTreeModel
    ( ItemsTreeModel
    , itemsTreeModelNew
    , updateModel
    , getItem
    ) where

import Bucket
import Graphics.UI.Gtk

type ItemsTreeModel = ListStore BucketItem

itemsTreeModelNew :: IO ItemsTreeModel
itemsTreeModelNew = listStoreNew []

updateModel :: ItemsTreeModel -> Bucket -> IO ()
updateModel model bucket = do
    listStoreClear model
    mapM (listStoreAppend model) (bucketItems bucket)
    return ()

getItem :: TreeView -> ItemsTreeModel -> TreePath -> IO BucketItem
getItem treeView model treePath = do
    Just treeIter <- treeModelGetIter model treePath
    listStoreGetValue model (listStoreIterToIndex treeIter)
