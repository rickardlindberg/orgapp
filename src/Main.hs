import Bucket
import Graphics.UI.Gtk
import ItemsTreeModel

main :: IO ()
main = do
    initGUI
    itemsTreeModel <- itemsTreeModelNew
    showMainWindow itemsTreeModel
    mainGUI

showMainWindow :: ItemsTreeModel -> IO ()
showMainWindow itemsTreeModel = do
    builder       <- builderFromFile "interface.glade"

    mainWindow    <- builderGetObject builder castToWindow   "main_window"
    importButton  <- builderGetObject builder castToButton   "import_button"
    searchText    <- builderGetObject builder castToEntry    "search_text"
    itemsTreeView <- builderGetObject builder castToTreeView "items_tree_view"

    mainWindow    `onDestroy`         mainQuit
    importButton  `onClicked`         handleImportButtonClicked
    searchText    `onEditableChanged` handleSearchTextChanged searchText
    itemsTreeView `onRowActivated`    handleItemActivated itemsTreeView itemsTreeModel

    initItemsTreeView itemsTreeView itemsTreeModel

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

handleImportButtonClicked = do
    putStrLn "import clicked"

handleSearchTextChanged searchText = do
    allText <- editableGetChars searchText 0 (-1)
    putStrLn $ "serach text changed: " ++ allText

handleItemActivated treeView model treePath treeViewColumn = do
    item <- getItem treeView model treePath
    putStrLn $ "activated item: " ++ (itemPath item)
