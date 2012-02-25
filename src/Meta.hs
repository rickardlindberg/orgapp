module Meta
    ( Meta(..)
    , createMeta
    , setFilename
    , writeMeta
    ) where

import System.IO

data Meta = Meta {
    filename :: String
} deriving (Eq, Show)

createMeta = Meta { filename = "" }

setFilename :: String -> Meta -> Meta
setFilename filename meta = meta { filename = filename }

metaToStr :: Meta -> String
metaToStr meta = "name::" ++ filename meta

writeMeta :: Meta -> FilePath -> IO ()
writeMeta meta destination = writeFile destination (metaToStr meta)
