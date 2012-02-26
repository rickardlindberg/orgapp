module Meta where

import System.IO
import Text.ParserCombinators.Parsec

data Meta = Meta {
    filenameOld :: String
} deriving (Eq, Show)

createMeta = Meta { filenameOld = "" }

setFilename :: String -> Meta -> Meta
setFilename filename meta = meta { filenameOld = filename }

metaFilename :: Meta -> String
metaFilename Meta { filenameOld = f } = f

metaToStr :: Meta -> String
metaToStr meta = "name::" ++ filenameOld meta ++ "\n"

metaFromStr :: String -> Meta
metaFromStr str =
    let pairs = parseMeta str
    in Meta { filenameOld = findKey "name" pairs }

parseMeta :: String -> [(String, String)]
parseMeta input =
    case parse file "" input of
        Left _  -> []
        Right x -> x
    where
        file :: Parser [(String, String)]
        file  = do
            lines <- many line
            eof
            return lines
        line  = do
            key <- key
            sep
            value <- value
            eol
            return (key, value)
        key   = many (noneOf ":")
        sep   = string "::"
        value = many (noneOf "\n")
        eol   = char '\n'

findKey key = snd . head . filter (\(a, b) -> a == key)

writeMeta :: Meta -> FilePath -> IO ()
writeMeta meta destination = writeFile destination (metaToStr meta)
