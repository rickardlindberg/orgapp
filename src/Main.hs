module Main (main) where

import Bucket
import Data.IORef
import Graphics.UI.Gtk
import ItemsTreeModel
import MainWindow
import System.Directory
import System.FilePath

main :: IO ()
main = do
    initGUI
    itemsTreeModel <- itemsTreeModelNew
    currentBucketRef <- loadDefaultBucket
    showMainWindow itemsTreeModel currentBucketRef
    mainGUI

loadDefaultBucket :: IO (IORef Bucket)
loadDefaultBucket = do
    home <- getHomeDirectory
    Just bucket <- loadBucketFrom $ home </> "org-app-bucket"
    newIORef bucket
