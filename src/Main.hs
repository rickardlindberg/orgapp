module Main (main) where

import Bucket.Load
import Bucket.Types
import Data.IORef
import Graphics.UI.Gtk
import MainWindow
import System.Directory
import System.FilePath

main :: IO ()
main = do
    initGUI
    loadDefaultBucket >>= showMainWindow
    mainGUI

loadDefaultBucket :: IO (IORef Bucket)
loadDefaultBucket = do
    home <- getHomeDirectory
    Just bucket <- loadBucketFrom $ home </> "org-app-bucket"
    newIORef bucket
