module GUI.ItemsTreeModel
    ( ItemsTreeModel
    , itemsTreeModelNew
    , updateModel
    , getItem
    ) where

import Bucket.Types
import Data.Maybe
import Graphics.UI.Gtk

type ItemsTreeModel = ListStore BucketItem

itemsTreeModelNew :: IO ItemsTreeModel
itemsTreeModelNew = listStoreNew []

updateModel :: ItemsTreeModel -> [BucketItem] -> IO ()
updateModel model items = do
    listStoreClear model
    mapM_ (listStoreAppend model) items
    return ()

getItem :: TreeView -> ItemsTreeModel -> TreePath -> IO (Maybe BucketItem)
getItem treeView model treePath = do
    treeIter <- treeModelGetIter model treePath
    if isJust treeIter
        then fmap Just (listStoreGetValue model (listStoreIterToIndex (fromJust treeIter)))
        else return Nothing
