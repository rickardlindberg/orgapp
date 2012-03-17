module Main (main) where

import Bucket.Load
import Bucket.Types
import Control.Monad
import Data.IORef
import Graphics.UI.Gtk
import GUI.MainWindow
import System.Environment

main :: IO ()
main = do
    x <- loadBucketFromArgs
    case x of
        Left  msg       -> putStrLn msg
        Right bucketRef -> do
            initGUI
            showMainWindow bucketRef
            mainGUI

loadBucketFromArgs :: IO (Either String (IORef Bucket))
loadBucketFromArgs = do
    args <- getArgs
    if length args == 1
        then do
            bucket <- loadBucketFrom $ head args
            case bucket of
                Nothing     -> return $ Left "Failed to load bucket"
                Just bucket -> liftM Right (newIORef bucket)
        else return $ Left "Supply one argument!"
