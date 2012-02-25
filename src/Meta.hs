module Meta where

import System.IO
import Text.ParserCombinators.Parsec

data Meta = Meta {
    filename :: String
} deriving (Eq, Show)

createMeta = Meta { filename = "" }

setFilename :: String -> Meta -> Meta
setFilename filename meta = meta { filename = filename }

metaToStr :: Meta -> String
metaToStr meta = "name::" ++ filename meta ++ "\n"

metaFromStr :: String -> Meta
metaFromStr str =
    let pairs = parseMeta str
    in Meta { filename = findKey "name" pairs }

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
