module GUI.ItemsTreeModel
    ( ItemsTreeModel
    , itemsTreeModelNew
    , updateModel
    , getItem
    ) where

import Bucket.Types
import Graphics.UI.Gtk

type ItemsTreeModel = ListStore BucketItem

itemsTreeModelNew :: IO ItemsTreeModel
itemsTreeModelNew = listStoreNew []

updateModel :: ItemsTreeModel -> [BucketItem] -> IO ()
updateModel model items = do
    listStoreClear model
    mapM_ (listStoreAppend model) items
    return ()

getItem :: TreeView -> ItemsTreeModel -> TreePath -> IO BucketItem
getItem treeView model treePath = do
    Just treeIter <- treeModelGetIter model treePath
    listStoreGetValue model (listStoreIterToIndex treeIter)
