module Open (open) where

import Bucket
import System.Process

open :: BucketItem -> IO ()
open item = do
    createProcess (proc "xdg-open" [itemFilePath item])
    return ()
