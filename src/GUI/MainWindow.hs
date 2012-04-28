module GUI.MainWindow (showMainWindow) where

import Control.Monad.Trans (liftIO)
import Bucket.Import
import Bucket.Types
import Control.Monad
import Data.IORef
import Data.List
import Graphics.UI.Gtk
import GUI.ItemEditor
import GUI.ItemsTreeModel
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
    itemEditor    <- builderGetObject builder castToDialog            "item_editor_dialog"
    tagsText      <- builderGetObject builder castToEntry             "tags_text"
    titleText     <- builderGetObject builder castToEntry             "title_text"

    itemsModel    <- itemsTreeModelNew

    let updateItemList = createUpdateItemList itemsModel currentBucketRef searchText

    mainWindow    `onDestroy`         mainQuit
    mainWindow    `on`                keyPressEvent $ tryEvent $ do
                                          [Control] <- eventModifier
                                          "l"       <- eventKeyName
                                          liftIO $ widgetGrabFocus searchText
    importButton  `onClicked`         handleImportButtonClicked fileChooser currentBucketRef updateItemList
    searchText    `onEditableChanged` updateItemList
    itemsTreeView `onRowActivated`    handleItemActivated itemsTreeView itemsModel
    editButton    `onClicked`         handleEditButtonClicked itemEditor itemsTreeView itemsModel tagsText titleText currentBucketRef updateItemList

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
    createTagsColumn model >>= treeViewAppendColumn treeView
    createDateColumn model >>= treeViewAppendColumn treeView
    return ()

createNameColumn :: ItemsTreeModel -> IO TreeViewColumn
createNameColumn model = do
    textRenderer <- cellRendererTextNew
    column       <- treeViewColumnNew
    treeViewColumnPackStart column textRenderer True
    cellLayoutSetAttributes column textRenderer model $
        \item -> [cellText := displayTitle item]
    return column

createTagsColumn :: ItemsTreeModel -> IO TreeViewColumn
createTagsColumn model = do
    textRenderer <- cellRendererTextNew
    column       <- treeViewColumnNew
    treeViewColumnPackStart column textRenderer True
    cellLayoutSetAttributes column textRenderer model $
        \item -> [cellText := "(" ++ intercalate "," (tags item) ++ ")", cellTextForeground := "#aaaaaa"]
    return column

createDateColumn :: ItemsTreeModel -> IO TreeViewColumn
createDateColumn model = do
    textRenderer <- cellRendererTextNew
    column       <- treeViewColumnNew
    treeViewColumnPackStart column textRenderer True
    cellLayoutSetAttributes column textRenderer model $
        \item -> [cellText := creationDate item]
    return column

handleImportButtonClicked fileChooser currentBucketRef updateItemList = do
    response <- dialogRun fileChooser
    when (response == ResponseOk) $ do
        Just file <- fileChooserGetFilename fileChooser
        currentBucket <- readIORef currentBucketRef
        -- TODO: create meta from user input
        -- TODO: show error dialog if file can't be imported
        newBucket <- importFile currentBucket file
        -- TODO: show warning dialog if file can't be removed
        removeFile file
        writeIORef currentBucketRef newBucket
        updateItemList
    widgetHide fileChooser

handleItemActivated treeView model treePath treeViewColumn =
    getItem treeView model treePath >>= \(Just item) -> open item
