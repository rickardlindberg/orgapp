module GUI.MainWindow (showMainWindow) where

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

showMainWindow :: IORef Bucket -> IO ()
showMainWindow currentBucketRef = do
    builder       <- builderFromFile "interface.glade"

    mainWindow    <- builderGetObject builder castToWindow            "main_window"
    importButton  <- builderGetObject builder castToButton            "import_button"
    searchText    <- builderGetObject builder castToEntry             "search_text"
    itemsTreeView <- builderGetObject builder castToTreeView          "items_tree_view"
    fileChooser   <- builderGetObject builder castToFileChooserDialog "import_file_dialog"
    editButton    <- builderGetObject builder castToButton            "edit_button"
    tagEditor     <- builderGetObject builder castToDialog            "tag_editor_dialog"
    tagEditorText <- builderGetObject builder castToEntry             "tags_text"

    itemsModel    <- itemsTreeModelNew

    let updateItemList = createUpdateItemList itemsModel currentBucketRef searchText

    mainWindow    `onDestroy`         mainQuit
    importButton  `onClicked`         handleImportButtonClicked fileChooser currentBucketRef updateItemList
    searchText    `onEditableChanged` updateItemList
    itemsTreeView `onRowActivated`    handleItemActivated itemsTreeView itemsModel
    editButton    `onClicked`         handleEditButtonClicked tagEditor itemsTreeView itemsModel tagEditorText currentBucketRef updateItemList

    initItemsTreeView itemsTreeView itemsModel
    updateItemList

    widgetShowAll mainWindow

createUpdateItemList :: ItemsTreeModel -> IORef Bucket -> Entry -> IO ()
createUpdateItemList model bucketRef searchText = do
    bucket <- readIORef bucketRef
    searchString <- editableGetChars searchText 0 (-1)
    let filteredItems = filter (matchSearch searchString) (bucketItems bucket)
    updateModel model filteredItems

builderFromFile :: FilePath -> IO Builder
builderFromFile path = do
    builder <- builderNew
    builderAddFromFile builder path
    return builder

initItemsTreeView :: TreeView -> ItemsTreeModel -> IO ()
initItemsTreeView treeView model = do
    treeViewSetModel treeView model
    createNameColumn model >>= treeViewAppendColumn treeView
    return ()

createNameColumn :: ItemsTreeModel -> IO TreeViewColumn
createNameColumn model = do
    textRenderer <- cellRendererTextNew
    column       <- treeViewColumnNew
    treeViewColumnPackStart column textRenderer True
    cellLayoutSetAttributes column textRenderer model $
        \item -> [cellText := fileName item]
    return column

handleImportButtonClicked fileChooser currentBucketRef updateItemList = do
    response <- dialogRun fileChooser
    when (response == ResponseOk) $ do
        Just file <- fileChooserGetFilename fileChooser
        currentBucket <- readIORef currentBucketRef
        -- TODO: create meta from user input
        -- TODO: show error dialog if file can't be imported
        newBucket <- importFile currentBucket file createMeta
        -- TODO: show warning dialog if file can't be removed
        removeFile file
        writeIORef currentBucketRef newBucket
        updateItemList
    widgetHide fileChooser

handleItemActivated treeView model treePath treeViewColumn =
    getItem treeView model treePath >>= open

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
