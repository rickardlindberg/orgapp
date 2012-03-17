module GUI.TagEditor where

import Bucket.EditItem
import Bucket.Types
import Control.Monad
import Data.IORef
import Data.List
import Graphics.UI.Gtk
import GUI.ItemsTreeModel

handleEditButtonClicked tagEditor treeView model tagEditorText currentBucketRef updateItemList = do
    (treePath, _) <- treeViewGetCursor treeView
    item <- getItem treeView model treePath
    entrySetText tagEditorText (tagsToString (tags item))
    response <- dialogRun tagEditor
    when (response == ResponseOk) $ do
        currentBucket <- readIORef currentBucketRef
        tagsText <- entryGetText tagEditorText
        let newItem = setTags (stringToTags tagsText) item
        -- TODO: show error dialog if we can't save item
        newBucket <- editItem currentBucket item newItem
        writeIORef currentBucketRef newBucket
        updateItemList
    widgetHide tagEditor

tagsToString :: [String] -> String
tagsToString = intercalate ","

stringToTags :: String -> [String]
stringToTags = getTags
    where
        getTags "" = []
        getTags s = (takeWhile notComma s):getTags (drop 1 (dropWhile notComma s))
        notComma ',' = False
        notComma _ = True
