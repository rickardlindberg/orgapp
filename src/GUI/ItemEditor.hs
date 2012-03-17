module GUI.ItemEditor where

import Bucket.EditItem
import Bucket.Types
import Control.Monad
import Data.IORef
import Data.List
import Graphics.UI.Gtk
import GUI.ItemsTreeModel

handleEditButtonClicked itemEditor treeView model tagsText currentBucketRef updateItemList = do
    (treePath, _) <- treeViewGetCursor treeView
    item <- getItem treeView model treePath
    entrySetText tagsText (tagsToString (tags item))
    response <- dialogRun itemEditor
    when (response == ResponseOk) $ do
        currentBucket <- readIORef currentBucketRef
        tagsText <- entryGetText tagsText
        let newItem = setTags (stringToTags tagsText) item
        -- TODO: show error dialog if we can't save item
        newBucket <- editItem currentBucket item newItem
        writeIORef currentBucketRef newBucket
        updateItemList
    widgetHide itemEditor

tagsToString :: [String] -> String
tagsToString = intercalate ","

stringToTags :: String -> [String]
stringToTags = getTags
    where
        getTags "" = []
        getTags s = (takeWhile notComma s):getTags (drop 1 (dropWhile notComma s))
        notComma ',' = False
        notComma _ = True
