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
    bucket <- loadDefaultBucket
    currentBucketRef <- newIORef bucket
    showMainWindow itemsTreeModel currentBucketRef
    updateModel itemsTreeModel bucket
    mainGUI

loadDefaultBucket :: IO Bucket
loadDefaultBucket = do
    home <- getHomeDirectory
    Just bucket <- loadBucketFrom $ home </> "org-app-bucket"
    return bucket
