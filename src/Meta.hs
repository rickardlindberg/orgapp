module Meta
    ( Meta(..)
    , createMeta
    , writeMeta
    ) where

import System.IO

data Meta = Meta {
    foo :: [String]
}

createMeta = Meta []

writeMeta :: Meta -> FilePath -> IO ()
writeMeta meta destination = do
    openFile destination WriteMode >>= hClose
