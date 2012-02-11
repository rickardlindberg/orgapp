module MainWindow (showMainWindow) where

import Bucket
import Control.Monad
import Data.IORef
import Graphics.UI.Gtk
import ItemsTreeModel

showMainWindow :: IORef Bucket -> IO ()
showMainWindow currentBucketRef = do
    builder       <- builderFromFile "interface.glade"

    mainWindow    <- builderGetObject builder castToWindow            "main_window"
    importButton  <- builderGetObject builder castToButton            "import_button"
    searchText    <- builderGetObject builder castToEntry             "search_text"
    itemsTreeView <- builderGetObject builder castToTreeView          "items_tree_view"
    fileChooser   <- builderGetObject builder castToFileChooserDialog "import_file_dialog"

    itemsModel    <- itemsTreeModelNew

    mainWindow    `onDestroy`         mainQuit
    importButton  `onClicked`         handleImportButtonClicked fileChooser currentBucketRef itemsModel
    searchText    `onEditableChanged` handleSearchTextChanged searchText
    itemsTreeView `onRowActivated`    handleItemActivated itemsTreeView itemsModel

    initItemsTreeView itemsTreeView itemsModel
    bucket <- readIORef currentBucketRef
    updateModel itemsModel (bucketItems bucket)

    widgetShowAll mainWindow

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
        \item -> [cellText := itemPath item]
    return column

handleImportButtonClicked fileChooser currentBucketRef itemsModel = do
    response <- dialogRun fileChooser
    when (response == ResponseOk) $ do
        Just file <- fileChooserGetFilename fileChooser
        currentBucket <- readIORef currentBucketRef
        newBucket <- importFile currentBucket file
        writeIORef currentBucketRef newBucket
        updateModel itemsModel (bucketItems newBucket)
    widgetHide fileChooser

handleSearchTextChanged searchText = do
    allText <- editableGetChars searchText 0 (-1)
    putStrLn $ "serach text changed: " ++ allText

handleItemActivated treeView model treePath treeViewColumn = do
    item <- getItem treeView model treePath
    putStrLn $ "activated item: " ++ (itemPath item)
