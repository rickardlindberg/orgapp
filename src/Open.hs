module Open (open) where

import Bucket.Types
import System.Process

open :: BucketItem -> IO ()
open item = do
    createProcess (proc "xdg-open" [filePath item])
    return ()
