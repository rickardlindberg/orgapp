module GUI.TagEditor where

import Bucket.EditItem
import Bucket.Import
import Bucket.Types
import Control.Monad
import Data.IORef
import Data.List
import Graphics.UI.Gtk
import GUI.ItemsTreeModel
import Meta
import Open
import SearchFilter
import System.Directory

handleEditButtonClicked tagEditor treeView model tagEditorText currentBucketRef updateItemList = do
    -- TODO: extract tags -> comma sep string -> tags somehere else
    (treePath, _) <- treeViewGetCursor treeView
    item <- getItem treeView model treePath
    entrySetText tagEditorText (intercalate "," (tags item))
    response <- dialogRun tagEditor
    when (response == ResponseOk) $ do
        currentBucket <- readIORef currentBucketRef
        tagsText <- entryGetText tagEditorText
        let newItem = setTags item (getTags tagsText)
        -- TODO: show error dialog if we can't save item
        newBucket <- editItem currentBucket item newItem
        writeIORef currentBucketRef newBucket
        updateItemList
    widgetHide tagEditor
    where
        getTags "" = []
        getTags s = (takeWhile notComma s):getTags (drop 1 (dropWhile notComma s))
        notComma ',' = False
        notComma _ = True
