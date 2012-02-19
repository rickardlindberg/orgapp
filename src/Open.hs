module Open (open) where

import Bucket
import System.Process

open :: BucketItem -> IO ()
open item = do
    -- TODO: specify path to file instead of item
    createProcess (proc "xdg-open" [itemPath item])
    return ()
